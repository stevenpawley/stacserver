# Plumber-based STAC API router.
# Creates an OGC API - Features / STAC API 1.0 compliant plumber router backed
# by the PostgreSQL database set up with stac_db_setup().

#' Create a plumber router serving a minimal STAC API
#'
#' Returns a `plumber` router pre-wired with the following endpoints:
#'
#' | Method | Path | Description |
#' |--------|------|-------------|
#' | GET | `/catalog` | Landing page (root catalog) |
#' | GET | `/catalog/conformance` | Conformance classes |
#' | GET | `/catalog/collections` | List all collections |
#' | GET | `/catalog/collections/{collectionId}` | Single collection |
#' | GET | `/catalog/collections/{collectionId}/items` | Items in a collection |
#' | GET | `/catalog/collections/{collectionId}/items/{itemId}` | Single item |
#' | GET | `/catalog/search` | Search items (GET form) |
#' | POST | `/catalog/search` | Search items (POST / JSON body) |
#'
#' **Search parameters** (GET query string or POST JSON body):
#' * `bbox` - comma-separated `west,south,east,north` (GET) or array (POST);
#'   the six-element form `west,south,min_elevation,east,north,max_elevation`
#'   is also accepted, and a box whose west edge exceeds its east edge is
#'   treated as crossing the antimeridian
#' * `datetime` - ISO 8601 value or range `start/end`; use `..` for open end
#' * `collections` - collection ID(s) to filter
#' * `ids` - item ID(s) to filter
#' * `limit` - max results per page (default 10, max 10 000)
#' * `offset` - zero-based page offset (default 0)
#' * `query` - (POST only) property filters in the form defined by the STAC API
#'   Query extension, covering any item property including extension fields
#'   such as `"eo:cloud_cover"`, `"sci:doi"` or `"classification:classes"`.
#'   A bare value means equality; an object selects operators:
#'   `{"eo:cloud_cover": {"lt": 10}}`. Supported operators are `eq`, `neq`,
#'   `lt`, `lte`, `gt`, `gte`, `startsWith`, `endsWith`, `contains` and `in`.
#'   `properties` is accepted as an alias for backwards compatibility.
#'
#' # Connections
#'
#' `con` is held for the lifetime of the router. A bare [DBI::dbConnect()]
#' connection will eventually be closed by the server or a connection pooler,
#' after which every request fails until the process restarts, so for anything
#' long-running pass a `pool::dbPool()` object instead — the `.db_*` helpers
#' work with either.
#'
#' # Access control
#'
#' The router performs no authentication of its own: every request it receives
#' is served. Access must be enforced in front of it. If deploying on Posit
#' Connect, access is controlled by the caller's API key before the request
#' reaches this process, and callers authenticate to Connect itself:
#'
#' ```
#' curl -H "Authorization: Key <connect-api-key>" \
#'      https://connect.example.com/content/<guid>/catalog/collections
#' ```
#'
#' # Cross-origin requests
#'
#' `cors_origins` controls the `Access-Control-Allow-Origin` header, which
#' decides whether JavaScript running on *another* website may read this API's
#' responses. It is not access control: the request still reaches the server
#' and is served either way, and non-browser clients — `rstac`, GDAL, QGIS,
#' Python — ignore the header entirely. It only stops a page the user happens
#' to be visiting from reading the catalog on their behalf. Name an origin only
#' for a browser app hosted elsewhere by:
#'
#' ```r
#' stac_api_router(con, cors_origins = "https://example.com")
#' ```
#' @param con A DBI connection, or a `pool::dbPool()` object.
#' @param base_url Public URL of the deployed Plumber content (no trailing
#'   slash). The STAC API is served under `/catalog`; this URL is used to build
#'   its public links, e.g. `https://connect.example.com/stac`.
#' @param title Human-readable API title.
#' @param description API description.
#' @param sign_fn A function `function(href)` that accepts an unsigned asset
#'   href and returns a signed href string. When non-`NULL`, asset hrefs in
#'   every item response are signed before being returned. Pass
#'   [azure_signer()] to sign Azure Blob Storage hrefs with a managed identity,
#'   or supply your own function for another backend. Default `NULL`
#'   (no signing). When enabled, all router responses include
#'   `Cache-Control: private, no-store` to prevent caching temporary asset
#'   credentials.
#' @param cors_origins Origins permitted to read responses from browser
#'   JavaScript, as a character vector of `scheme://host[:port]` values with no
#'   path, e.g. `"https://browser.example.com"`. `"*"` allows every origin.
#'   Default `NULL` sends no CORS headers at all. See *Cross-origin requests*.
#' @return A `plumber` router object.
#' @export
stac_api_router <- function(
  con,
  base_url = "http://localhost:8000",
  title = "STAC API",
  description = "A minimal STAC API served by stacserver",
  sign_fn = NULL,
  cors_origins = NULL
) {
  cors_origins <- .check_cors_origins(cors_origins)
  content_url <- sub("/+$", "", base_url)
  api_url <- paste0(content_url, "/catalog")

  # Custom serializer (how results are returned back to the client)
  # JSON is default but for STAC API compliance we are altering the
  # defaults to unwrap single element R vectors and provide explicit
  # nulls.
  pr <- plumber::pr() |>
    plumber::pr_set_serializer(.stac_serializer())

  # Use the caller-provided title and description in Swagger's generated spec.
  pr <- plumber::pr_set_api_spec(pr, function(spec) {
    spec$info$title <- title
    spec$info$description <- description
    spec
  })

  if (!is.null(sign_fn)) {
    # Set this before routing, prevents caching
    pr <- plumber::pr_filter(pr, "signed_response_cache", function(req, res) {
      res$setHeader("Cache-Control", "private, no-store")
      plumber::forward()
    })
  }

  # CORS
  if (!is.null(cors_origins)) {
    pr <- plumber::pr_filter(pr, "cors", .cors_filter(cors_origins))
  }

  # Inject standard STAC navigation links and optionally sign asset hrefs
  prepare_item <- function(item) {
    item <- .inject_item_links(item, api_url)
    if (!is.null(sign_fn)) {
      item <- .sign_item_assets(item, sign_fn)
    }
    item
  }

  # Navigation links injected into every collection response. They replace any
  # link of the same rel stored in the database, which for a catalog also
  # published statically points at files rather than at this API.
  collection_links <- function(cid) {
    list(
      .link("self", paste0(api_url, "/collections/", cid), "application/json"),
      .link("root", api_url, "application/json"),
      .link("parent", api_url, "application/json"),
      .link("items", paste0(api_url, "/collections/", cid, "/items"), "application/geo+json")
    )
  }
  collection_rels <- c("self", "root", "parent", "items")

  # STAC API spec requires the following `rel` types:
  # - self and root: required by OGC API Features on every response
  # - conformance: required by OGC API Features so clients can find /conformance
  # - data: required by OGC API Features to point to /collections
  # - service-desc: required by the oas30 conformance class, pointing at the
  #   OpenAPI description plumber generates
  # - search (x2): required by the STAC API Item Search spec, one per supported method

  # Landing page (catalog). Leave / unclaimed so Plumber serves Swagger UI
  # there when this router is deployed to Posit Connect.
  # GET /catalog
  pr <- plumber::pr_get(pr, "/catalog", function(req, res) {
    list(
      type = "Catalog",
      stac_version = "1.0.0",
      id = "stac-api",
      title = title,
      description = description,
      conformsTo = .stac_conformance_uris(),
      links = .landing_links(api_url, docs_url = content_url)
    )
  })

  # GET /catalog/conformance
  pr <- plumber::pr_get(pr, "/catalog/conformance", function(req, res) {
    list(conformsTo = .stac_conformance_uris())
  })

  # List all collections
  # GET /catalog/collections
  pr <- plumber::pr_get(pr, "/catalog/collections", function(req, res) {
    collections <- .db_get_all_collections(con)
    collections <- lapply(collections, function(col) {
      col$links <- .merge_links(
        col$links,
        new_links = collection_links(col$id),
        override = collection_rels
      )
      col
    })

    list(
      collections = collections,
      links = list(
        .link("self", paste0(api_url, "/collections"), "application/json"),
        .link("root", api_url, "application/json")
      )
    )
  })

  # Get a single collection using dynamic routing for the collectionId
  # GET /collections/{collectionId}
  pr <- plumber::pr_get(
    pr,
    "/catalog/collections/<collectionId>",
    function(req, res, collectionId) {
      col <- .db_get_collection(con, collectionId)
      if (is.null(col)) {
        return(.not_found(res, "Collection not found"))
      }

      col$links <- .merge_links(
        col$links,
        new_links = collection_links(collectionId),
        override = collection_rels
      )
      col
    }
  )

  # List items in a collection (dynamic routing for collectionId)
  # GET /collections/{collectionId}/items
  pr <- plumber::pr_get(
    pr,
    "/catalog/collections/<collectionId>/items",
    function(
      req,
      res,
      collectionId,
      bbox = "",
      datetime = "",
      limit = "",
      offset = ""
    ) {
      .with_bad_request(res, {
        if (is.null(.db_get_collection(con, collectionId))) {
          return(.not_found(res, "Collection not found"))
        }

        limit <- .parse_int_param(limit, "limit", 10L, min = 1L, max = 10000L)
        offset <- .parse_int_param(offset, "offset", 0L, min = 0L)
        bbox <- if (nzchar(bbox)) bbox else NULL
        datetime <- if (nzchar(datetime)) datetime else NULL

        bbox_parsed <- .parse_bbox_param(bbox)
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
            base_url = paste0(api_url, "/collections/", collectionId, "/items"),
            offset = offset,
            limit = limit,
            matched = result$matched,
            extra_query = .query_string(bbox = bbox, datetime = datetime)
          )
        )
      })
    }
  )

  # Get a single item using dynamic routing on collectionId and itemId
  # GET /collections/{collectionId}/items/{itemId}
  pr <- plumber::pr_get(
    pr,
    "/catalog/collections/<collectionId>/items/<itemId>",
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
    "/catalog/search",
    function(
      req,
      res,
      bbox = "",
      datetime = "",
      collections = "",
      ids = "",
      limit = "",
      offset = ""
    ) {
      .with_bad_request(res, {
        limit <- .parse_int_param(limit, "limit", 10L, min = 1L, max = 10000L)
        offset <- .parse_int_param(offset, "offset", 0L, min = 0L)
        bbox <- if (nzchar(bbox)) bbox else NULL
        datetime <- if (nzchar(datetime)) datetime else NULL

        collections <- .split_param(collections)
        ids <- .split_param(ids)

        bbox_parsed <- .parse_bbox_param(bbox)
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
            base_url = paste0(api_url, "/search"),
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
      })
    }
  )

  # POST /search
  pr <- plumber::pr_post(
    pr,
    "/catalog/search",
    function(req, res) {
      .with_bad_request(res, {
        body <- req$body %||% list()

        bbox_parsed <- .parse_bbox_body(body$bbox)
        if (!is.null(bbox_parsed)) {
          bbox_parsed <- .validate_bbox(bbox_parsed)
        }

        datetime <- body$datetime %||% NULL
        collections <- .as_char_vec(body$collections)
        ids <- .as_char_vec(body$ids)
        limit <- .parse_int_param(
          body$limit,
          "limit",
          10L,
          min = 1L,
          max = 10000L
        )
        offset <- .parse_int_param(body$offset, "offset", 0L, min = 0L)
        query <- body$query %||% NULL

        dt <- .parse_datetime_param(datetime)

        result <- .db_search_items(
          con,
          bbox = bbox_parsed,
          dt_start = dt$start,
          dt_end = dt$end,
          single_dt = dt$single_dt,
          collections = collections,
          ids = ids,
          query = query,
          limit = limit,
          offset = offset
        )

        .feature_collection(
          features = lapply(result$items, prepare_item),
          matched = result$matched,
          returned = length(result$items),
          links = .pagination_links(
            base_url = paste0(api_url, "/search"),
            offset = offset,
            limit = limit,
            matched = result$matched
          )
        )
      })
    },
    parsers = "json"
  )

  pr
}
