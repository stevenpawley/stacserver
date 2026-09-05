# Plumber-based STAC API router.
# Creates an OGC API – Features / STAC API 1.0 compliant plumber router backed
# by the PostgreSQL database set up with stac_db_setup().

#' Create a plumber router serving a minimal STAC API
#'
#' Returns a `plumber` router pre-wired with the following endpoints:
#'
#' | Method | Path | Description |
#' |--------|------|-------------|
#' | GET | `/` | Landing page (root catalog) |
#' | GET | `/conformance` | Conformance classes |
#' | GET | `/collections` | List all collections |
#' | GET | `/collections/{collectionId}` | Single collection |
#' | GET | `/collections/{collectionId}/items` | Items in a collection |
#' | GET | `/collections/{collectionId}/items/{itemId}` | Single item |
#' | GET | `/search` | Search items (GET form) |
#' | POST | `/search` | Search items (POST / JSON body) |
#'
#' **Search parameters** (GET query string or POST JSON body):
#' * `bbox` — comma-separated `west,south,east,north` (GET) or array (POST)
#' * `datetime` — ISO 8601 value or range `start/end`; use `..` for open end
#' * `collections` — collection ID(s) to filter
#' * `ids` — item ID(s) to filter
#' * `limit` — max results per page (default 10, max 10 000)
#' * `offset` — zero-based page offset (default 0)
#' * `properties` — (POST only) JSON object for property equality matching,
#'   supporting any item property including extension fields such as
#'   `"eo:cloud_cover"`, `"sci:doi"`, `"classification:classes"`, etc.
#'
#' @param con A DBI connection.
#' @param base_url Base URL of the API (no trailing slash). Used in link hrefs.
#' @param title Human-readable API title.
#' @param description API description.
#' @param sign_fn A function `function(href)` that accepts an unsigned asset
#'   href and returns a signed href string. When non-`NULL`, asset hrefs in
#'   every item response are signed before being returned. Pass
#'   [sign_azure_ad()] to use Azure AD / managed identity, or supply your own
#'   function for other auth methods (service principal, Planetary Computer
#'   signing proxy, etc.). Default `NULL` (no signing).
#' @return A `plumber` router object.
#' @export
stac_api_router <- function(
  con,
  base_url = "http://localhost:8000",
  title = "STAC API",
  description = "A minimal STAC API served by stacserver",
  sign_fn = NULL
) {
  # Custom serializer (how results are returned back to the client)
  # JSON is default but for STAC API compliance we are altering the
  # defaults to unwrap single element R vectors and provide explicit
  # nulls
  pr <- plumber::pr() |>
    plumber::pr_set_serializer(
      plumber::serializer_json(auto_unbox = TRUE, null = "null", na = "null")
    )

  # CORS — must be first so OPTIONS pre-flight bypasses auth
  # Only needed if the API is accessed from other JS web pages
  # Not needed if accessed from R/Python scripts
  pr <- plumber::pr_filter(pr, "cors", function(req, res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader(
      "Access-Control-Allow-Headers",
      "Content-Type, Accept, Authorization"
    )
    if (identical(req$REQUEST_METHOD, "OPTIONS")) {
      res$status <- 200L
      return(list())
    }
    plumber::forward()
  })

  # Inject standard STAC navigation links and optionally sign asset hrefs
  prepare_item <- function(item) {
    item <- .inject_item_links(item, base_url)
    if (!is.null(sign_fn)) {
      item <- .sign_item_assets(item, sign_fn)
    }
    item
  }

  # STAC API spec requires the following `rel` types:
  # - self and root: required by OGC API Features on every response
  # - conformance: required by OGC API Features so clients can find /conformance
  # - data: required by OGC API Features to point to /collections
  # - search (×2): required by the STAC API Item Search spec, one per supported method

  # Landing page (root catalog)
  # GET /
  pr <- plumber::pr_get(pr, "/", function(req, res) {
    list(
      type = "Catalog",
      stac_version = "1.1.0",
      id = "stac-api",
      title = title,
      description = description,
      conformsTo = .stac_conformance_uris(),
      links = list(
        .link("self", base_url, "application/json"),
        .link("root", base_url, "application/json"),
        .link("conformance", paste0(base_url, "/conformance"), "application/json"),
        .link("data", paste0(base_url, "/collections"), "application/json"),
        .link("search", paste0(base_url, "/search"), "application/geo+json", method = "GET"),
        .link("search", paste0(base_url, "/search"), "application/geo+json", method = "POST")
      )
    )
  })

  # GET /conformance
  pr <- plumber::pr_get(pr, "/conformance", function(req, res) {
    list(conformsTo = .stac_conformance_uris())
  })

  # List all collections
  # GET /collections
  pr <- plumber::pr_get(pr, "/collections", function(req, res) {
    collections <- .db_get_all_collections(con)
    collections <- lapply(collections, function(col) {
      cid <- col$id
      col$links <- .merge_links(
        col$links,
        list(
          .link("self", paste0(base_url, "/collections/", cid), "application/json"),
          .link("root", base_url, "application/json"),
          .link("items", paste0(base_url, "/collections/", cid, "/items"), "application/geo+json")
        )
      )
      col
    })

    list(
      collections = collections,
      links = list(
        .link("self", paste0(base_url, "/collections"), "application/json"),
        .link("root", base_url, "application/json")
      )
    )
  })

  # Get a single collection using dynamic routing for the collectionId
  # GET /collections/{collectionId}
  pr <- plumber::pr_get(
    pr,
    "/collections/<collectionId>",
    function(req, res, collectionId) {
      col <- .db_get_collection(con, collectionId)
      if (is.null(col)) {
        return(.not_found(res, "Collection not found"))
      }

      col$links <- .merge_links(
        col$links,
        list(
          .link("self", paste0(base_url, "/collections/", collectionId), "application/json"),
          .link("root", base_url, "application/json"),
          .link("items", paste0(base_url, "/collections/", collectionId, "/items"), "application/geo+json")
        )
      )
      col
    }
  )

  # List items in a collection (dynamic routing for collectionId)
  # GET /collections/{collectionId}/items
  pr <- plumber::pr_get(
    pr,
    "/collections/<collectionId>/items",
    function(
      req,
      res,
      collectionId,
      bbox = "",
      datetime = "",
      limit = 10L,
      offset = 0L
    ) {
      if (is.null(.db_get_collection(con, collectionId))) {
        return(.not_found(res, "Collection not found"))
      }

      limit <- min(as.integer(limit), 10000L)
      offset <- max(as.integer(offset), 0L)
      bbox <- if (nzchar(bbox)) bbox else NULL
      datetime <- if (nzchar(datetime)) datetime else NULL

      bbox_parsed <- tryCatch(.parse_bbox_param(bbox), error = function(e) {
        res$status <- 400L
        # base stop(): this message becomes an HTTP response body, so it must
        # stay plain text rather than picking up cli's bullets and styling
        stop(e$message)
      })
      dt <- .parse_datetime_param(datetime)

      result <- .db_search_items(
        con,
        bbox = bbox_parsed,
        dt_start = dt$start,
        dt_end = dt$end,
        single_dt = dt$single_dt,
        collections = collectionId,
        limit = limit,
        offset = offset
      )

      .feature_collection(
        features = lapply(result$items, prepare_item),
        matched = result$matched,
        returned = length(result$items),
        links = .pagination_links(
          base_url = paste0(base_url, "/collections/", collectionId, "/items"),
          offset = offset,
          limit = limit,
          matched = result$matched,
          extra_query = .query_string(bbox = bbox, datetime = datetime)
        )
      )
    }
  )

  # Get a single item using dynamic routing on collectionId and itemId
  # GET /collections/{collectionId}/items/{itemId}
  pr <- plumber::pr_get(
    pr,
    "/collections/<collectionId>/items/<itemId>",
    function(req, res, collectionId, itemId) {
      item <- .db_get_item(con, collectionId, itemId)
      if (is.null(item)) {
        return(.not_found(res, "Item not found"))
      }
      prepare_item(item)
    }
  )

  # Search items (GET)
  # @param bbox Bounding box: west,south,east,north
  # @param datetime ISO 8601 datetime or range
  # @param collections Collection ID(s), comma-separated
  # @param ids Item ID(s), comma-separated
  # @param limit Max results (default 10, max 10000)
  # @param offset Zero-based page offset
  # @serializer json list(auto_unbox = TRUE, null = "null", na = "null")
  # GET /search
  pr <- plumber::pr_get(
    pr,
    "/search",
    function(
      req,
      res,
      bbox = "",
      datetime = "",
      collections = "",
      ids = "",
      limit = 10L,
      offset = 0L
    ) {
      limit <- min(as.integer(limit), 10000L)
      offset <- max(as.integer(offset), 0L)
      bbox <- if (nzchar(bbox)) bbox else NULL
      datetime <- if (nzchar(datetime)) datetime else NULL

      collections <- .split_param(collections)
      ids <- .split_param(ids)

      bbox_parsed <- tryCatch(.parse_bbox_param(bbox), error = function(e) {
        res$status <- 400L
        # base stop(): this message becomes an HTTP response body, so it must
        # stay plain text rather than picking up cli's bullets and styling
        stop(e$message)
      })
      dt <- .parse_datetime_param(datetime)

      result <- .db_search_items(
        con,
        bbox = bbox_parsed,
        dt_start = dt$start,
        dt_end = dt$end,
        single_dt = dt$single_dt,
        collections = collections,
        ids = ids,
        limit = limit,
        offset = offset
      )

      .feature_collection(
        features = lapply(result$items, prepare_item),
        matched = result$matched,
        returned = length(result$items),
        links = .pagination_links(
          base_url = paste0(base_url, "/search"),
          offset = offset,
          limit = limit,
          matched = result$matched,
          extra_query = .query_string(
            bbox = bbox,
            datetime = datetime,
            collections = if (!is.null(collections)) {
              paste(collections, collapse = ",")
            } else {
              NULL
            },
            ids = if (!is.null(ids)) paste(ids, collapse = ",") else NULL
          )
        )
      )
    }
  )

  # POST /search
  pr <- plumber::pr_post(
    pr,
    "/search",
    function(req, res) {
      body <- req$body %||% list()

      bbox_raw <- body$bbox
      bbox_parsed <- if (!is.null(bbox_raw)) {
        b <- tryCatch(as.numeric(unlist(bbox_raw)), error = function(e) {
          res$status <- 400L
          cli::cli_abort("'bbox' must be a JSON array of four numbers")
        })
        if (length(b) != 4L) {
          res$status <- 400L
          return(.error_body(400L, "'bbox' must have exactly four elements"))
        }
        b
      } else {
        NULL
      }

      datetime <- body$datetime %||% NULL
      collections <- .as_char_vec(body$collections)
      ids <- .as_char_vec(body$ids)
      limit <- min(as.integer(body$limit %||% 10L), 10000L)
      offset <- max(as.integer(body$offset %||% 0L), 0L)
      properties <- body$properties %||% NULL

      dt <- .parse_datetime_param(datetime)

      result <- .db_search_items(
        con,
        bbox = bbox_parsed,
        dt_start = dt$start,
        dt_end = dt$end,
        single_dt = dt$single_dt,
        collections = collections,
        ids = ids,
        properties = properties,
        limit = limit,
        offset = offset
      )

      .feature_collection(
        features = lapply(result$items, prepare_item),
        matched = result$matched,
        returned = length(result$items),
        links = .pagination_links(
          base_url = paste0(base_url, "/search"),
          offset = offset,
          limit = limit,
          matched = result$matched
        )
      )
    },
    parsers = "json"
  )

  # ---- GET /sign ---- (only registered when a sign_fn is provided)
  if (!is.null(sign_fn)) {
    pr <- plumber::pr_get(pr, "/sign", function(req, res, href = "") {
      if (!nzchar(href)) {
        res$status <- 400L
        return(.error_body(400L, "Missing required query parameter: href"))
      }
      signed <- tryCatch(
        sign_fn(href),
        error = function(e) {
          res$status <- 500L
          .error_body(500L, conditionMessage(e))
        }
      )
      if (is.list(signed)) {
        return(signed)
      }
      list(href = signed)
    })
  }

  pr
}
