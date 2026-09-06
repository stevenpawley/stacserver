#' Sign all asset hrefs in a STAC Item list.
#'
#' Signing failures emit a warning and leave the href unchanged rather than
#' failing the whole request.
#'
#' @param item A STAC Item as a plain list (as returned from the database).
#' @param sign_fn A function `function(href)` returning a signed href string.
#' @return The item with signed asset hrefs.
#' @noRd
.sign_item_assets <- function(item, sign_fn) {
  if (is.null(item$assets) || length(item$assets) == 0) return(item)
  item$assets <- lapply(item$assets, function(a) {
    if (!is.null(a$href)) {
      a$href <- tryCatch(
        sign_fn(a$href),
        error = function(e) {
          cli::cli_warn(
            "Asset signing failed for '{a$href}': {conditionMessage(e)}"
          )
          a$href
        }
      )
    }
    a
  })
  item
}

#' STAC API conformance class URIs.
#'
#' Returns the list of OGC and STAC conformance class URIs declared by this
#' API. These are included in the landing page (`conformsTo`) and the
#' `/conformance` endpoint so that clients can discover which capabilities
#' (core, item search, OGC Features, GeoJSON) are supported.
#'
#' @return A character list of conformance class URI strings.
#' @noRd
.stac_conformance_uris <- function() {
  # Only classes that are actually implemented belong here: a client trusts
  # this list to decide what it may send. The versions match the STAC version
  # stamped on the objects served (1.1.0).
  list(
    "https://api.stacspec.org/v1.1.0/core",
    "https://api.stacspec.org/v1.1.0/collections",
    "https://api.stacspec.org/v1.1.0/item-search",
    "https://api.stacspec.org/v1.1.0/item-search#query",
    "https://api.stacspec.org/v1.1.0/ogcapi-features",
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
  )
}

#' Build a STAC link object.
#'
#' A STAC link object expresses a typed relationship between the current
#' resource and another URL. Every STAC object (Catalog, Collection, Item)
#' carries a `links` array of these objects, allowing clients to navigate the
#' API by following link relations rather than constructing URLs themselves
#' (HATEOAS). Common `rel` values include `"self"`, `"root"`, `"parent"`,
#' `"collection"`, `"items"`, and `"search"`.
#'
#' @param rel Link relation type (e.g. `"self"`, `"root"`, `"items"`).
#' @param href Target URL.
#' @param type Optional media type of the linked resource (e.g.
#'   `"application/json"`, `"application/geo+json"`).
#' @param method Optional HTTP method (e.g. `"GET"`, `"POST"`); used to
#'   distinguish multiple links with the same `rel` but different methods.
#' @param body Optional request body to send when following the link. Only
#'   meaningful alongside `method = "POST"`.
#' @param merge Optional flag saying whether `body` is a fragment to merge into
#'   the original request body (`TRUE`) or the whole body (`FALSE`).
#' @return A named list representing a single STAC link object.
#' @noRd
.link <- function(
  rel,
  href,
  type = NULL,
  method = NULL,
  body = NULL,
  merge = NULL
) {
  lnk <- list(rel = rel, href = href)
  if (!is.null(type)) {
    lnk$type <- type
  }
  if (!is.null(method)) {
    lnk$method <- method
  }
  if (!is.null(body)) {
    lnk$body <- body
  }
  if (!is.null(merge)) {
    lnk$merge <- merge
  }
  lnk
}

#' Merge two lists of STAC link objects, avoiding duplicates.
#'
#' Appends links from `new_links` to `existing`, skipping any link whose
#' `rel` and `href` combination already appears in `existing`. This preserves
#' links stored on an item or collection in the database while injecting
#' standard navigation links without creating duplicates.
#'
#' Relations named in `override` are dropped from `existing` first. A catalog
#' built for static hosting stores its own `self` and `root` links pointing at
#' files rather than at this API; keeping both would leave the response with
#' two `self` links, which the STAC specification does not allow.
#'
#' @param existing A list of existing link objects, or `NULL`.
#' @param new_links A list of link objects to append.
#' @param override Character vector of `rel` values that the new links replace
#'   rather than supplement.
#' @return A combined list of link objects with no duplicate `rel`/`href` pairs.
#' @noRd
.merge_links <- function(existing, new_links, override = character(0)) {
  existing <- existing %||% list()
  if (length(override) > 0 && length(existing) > 0) {
    keep <- !vapply(
      existing,
      function(l) isTRUE(l$rel %in% override),
      logical(1)
    )
    existing <- existing[keep]
  }
  keys <- vapply(existing, function(l) paste0(l$rel, "|", l$href), character(1))
  for (lnk in new_links) {
    key <- paste0(lnk$rel, "|", lnk$href)
    if (!key %in% keys) {
      existing <- c(existing, list(lnk))
      keys <- c(keys, key)
    }
  }
  existing
}

#' Build a STAC GeoJSON FeatureCollection response.
#'
#' Wraps a list of STAC Item feature objects into a GeoJSON FeatureCollection
#' with pagination metadata. `numberMatched` is the total number of items
#' satisfying the query (before pagination); `numberReturned` is the count in
#' this page.
#'
#' @param features A list of GeoJSON Feature objects (STAC Items).
#' @param matched Total number of items matching the query.
#' @param returned Number of items in this response page.
#' @param links A list of STAC link objects for pagination (`self`, `next`,
#'   `prev`).
#' @return A named list representing a GeoJSON FeatureCollection.
#' @noRd
.feature_collection <- function(features, matched, returned, links = list()) {
  list(
    type = "FeatureCollection",
    features = features,
    numberMatched = matched,
    numberReturned = returned,
    links = links
  )
}

#' Build pagination links for a STAC search or items response.
#'
#' Constructs the `self`, `next`, and `prev` link objects used in a
#' FeatureCollection response. `next` is omitted when the current page reaches
#' the end of results (`offset + limit >= matched`); `prev` is omitted on the
#' first page (`offset == 0`).
#'
#' A search made with POST cannot be paged with a URL: its filters live in the
#' request body, and a link carrying only `?limit=&offset=` would drop them and
#' page through the whole catalog instead. For those the links carry
#' `method = "POST"` and the complete body for the page, with `merge = FALSE`
#' saying that the body replaces the original rather than being merged into it.
#'
#' @param base_url Base URL of the endpoint (no query string).
#' @param offset Zero-based index of the first item on the current page.
#' @param limit Maximum number of items per page.
#' @param matched Total number of items matching the query.
#' @param extra_query Optional pre-encoded query string fragment (without
#'   leading `?` or `&`) for additional filter parameters such as `bbox` or
#'   `datetime`. Used for GET paging only.
#' @param body Search parameters of the originating POST body, excluding
#'   `limit` and `offset`, which each link sets for its own page. `NULL` (the
#'   default) produces GET-style links built from `extra_query`.
#' @return A list of STAC link objects.
#' @noRd
.pagination_links <- function(
  base_url,
  offset,
  limit,
  matched,
  extra_query = "",
  body = NULL
) {
  # The base query string always carries limit and offset, so any additional
  # filters are appended with "&" and an absent filter appends nothing at all.
  sep <- if (nzchar(extra_query)) "&" else ""

  page_link <- function(rel, page_offset) {
    if (!is.null(body)) {
      return(.link(
        rel,
        base_url,
        "application/geo+json",
        method = "POST",
        body = c(body, list(limit = limit, offset = page_offset)),
        merge = FALSE
      ))
    }
    .link(
      rel,
      paste0(
        base_url,
        "?limit=",
        limit,
        "&offset=",
        page_offset,
        sep,
        extra_query
      ),
      "application/geo+json"
    )
  }

  links <- list(page_link("self", offset))
  if (offset + limit < matched) {
    links <- c(links, list(page_link("next", offset + limit)))
  }
  if (offset > 0L) {
    links <- c(links, list(page_link("prev", max(0L, offset - limit))))
  }
  links
}

#' Inject standard navigation links into a STAC Item.
#'
#' Adds `self`, `root`, `collection`, and `parent` links to an item using
#' `.merge_links()` so that any links already stored on the item are preserved.
#'
#' @param item A STAC Item list (must have `$id` and `$collection` fields).
#' @param base_url Base URL of the API (no trailing slash).
#' @return The item with navigation links added to `$links`.
#' @noRd
.inject_item_links <- function(item, base_url) {
  cid <- item$collection
  iid <- item$id
  item$links <- .merge_links(
    item$links,
    override = c("self", "root", "collection", "parent"),
    new_links = list(
      .link(
        "self",
        paste0(base_url, "/collections/", cid, "/items/", iid),
        "application/geo+json"
      ),
      .link("root", base_url, "application/json"),
      .link(
        "collection",
        paste0(base_url, "/collections/", cid),
        "application/json"
      ),
      .link(
        "parent",
        paste0(base_url, "/collections/", cid),
        "application/json"
      )
    )
  )
  item
}

#' Set a 404 response and return an error body.
#'
#' @param res A plumber response object.
#' @param msg Human-readable description of what was not found.
#' @return A named list error body.
#' @noRd
.not_found <- function(res, msg) {
  res$status <- 404L
  .error_body(404L, msg)
}

#' Build a standard API error response body.
#'
#' @param code Integer HTTP status code.
#' @param description Human-readable error message.
#' @return A named list with `code` and `description` fields.
#' @noRd
.error_body <- function(code, description) {
  list(code = code, description = description)
}

#' Build a URL query string from named arguments.
#'
#' Encodes non-NULL, non-empty named arguments as a `key=value` query string
#' fragment (without a leading `?`). Values are percent-encoded via
#' [utils::URLencode()].
#'
#' @param ... Named character values. `NULL` and zero-length strings are
#'   silently dropped.
#' @return A single string of the form `"key1=val1&key2=val2"`, or `""`
#'   if all arguments are dropped.
#' @noRd
.query_string <- function(...) {
  args <- Filter(function(v) !is.null(v) && nzchar(v), list(...))
  if (length(args) == 0) {
    return("")
  }
  paste(
    mapply(
      function(k, v) {
        paste0(k, "=", utils::URLencode(as.character(v), reserved = TRUE))
      },
      names(args),
      args
    ),
    collapse = "&"
  )
}

#' Split a comma-separated query parameter into a character vector.
#'
#' Handles the two forms a repeated parameter can arrive in from Plumber:
#' a single comma-separated string (GET query string) or a character vector
#' of length > 1 (repeated keys). Returns `NULL` for absent or empty values.
#'
#' @param x A character scalar or vector, or `NULL`.
#' @return A character vector, or `NULL` if the input is absent or blank.
#' @noRd
.split_param <- function(x) {
  if (is.null(x) || (length(x) == 1 && !nzchar(x))) {
    return(NULL)
  }
  if (length(x) > 1) {
    return(as.character(x))
  }
  unlist(strsplit(x, ",", fixed = TRUE))
}

#' Coerce a JSON array field to a character vector.
#'
#' When a JSON array is parsed from a POST body it arrives as a list.
#' This function flattens and coerces it to a character vector, returning
#' `NULL` for absent or empty inputs.
#'
#' @param x A list, character vector, or `NULL`.
#' @return A character vector, or `NULL` if the result would be empty.
#' @noRd
.as_char_vec <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  v <- as.character(unlist(x))
  if (length(v) == 0) NULL else v
}

#' Signal a bad-request condition.
#'
#' Raises a condition that `.with_bad_request()` converts into a 400 response.
#' The message is used verbatim as the response body, so it stays plain text
#' rather than picking up cli's bullets and styling.
#'
#' @param msg Human-readable description of what is wrong with the request.
#' @noRd
.abort_bad_request <- function(msg) {
  stop(structure(
    class = c("stacserver_bad_request", "error", "condition"),
    list(message = msg, call = NULL)
  ))
}

#' Rebuild the search parameters of a POST body for its paging links.
#'
#' Collects the filters a POST search was made with, dropping those it did not
#' use, so that `.pagination_links()` can send them again for the next page.
#' `limit` and `offset` are left out: each link sets its own.
#'
#' Values that JSON requires to be arrays are wrapped in a list, because the
#' response serializer unboxes length-one vectors and a single collection id
#' would otherwise be written back as a string.
#'
#' @param bbox Parsed `bbox` numeric vector, or `NULL`.
#' @param intersects Parsed `intersects` geometry, or `NULL`.
#' @param datetime Raw `datetime` string, or `NULL`.
#' @param collections,ids Character vectors, or `NULL`.
#' @param query Parsed Query extension object, or `NULL`.
#' @return A named list of search parameters, possibly empty.
#' @noRd
.search_body <- function(
  bbox = NULL,
  intersects = NULL,
  datetime = NULL,
  collections = NULL,
  ids = NULL,
  query = NULL
) {
  body <- list()
  if (!is.null(bbox)) {
    body$bbox <- as.list(bbox)
  }
  if (!is.null(intersects)) {
    body$intersects <- intersects
  }
  if (!is.null(datetime)) {
    body$datetime <- datetime
  }
  if (!is.null(collections)) {
    body$collections <- as.list(collections)
  }
  if (!is.null(ids)) {
    body$ids <- as.list(ids)
  }
  if (!is.null(query)) {
    body$query <- query
  }
  body
}

#' Reject a search that carries both spatial filters.
#'
#' STAC API forbids `bbox` and `intersects` in the same request: they are two
#' ways of asking the same question and the specification does not say what
#' their combination would mean. Rejecting is safer than picking one, which
#' would answer a search the client did not ask for.
#'
#' @param bbox Parsed `bbox`, or `NULL`.
#' @param intersects Parsed `intersects` geometry, or `NULL`.
#' @return `NULL`, invisibly.
#' @noRd
.check_spatial_filters <- function(bbox, intersects) {
  if (!is.null(bbox) && !is.null(intersects)) {
    .abort_bad_request(
      "Only one of 'bbox' and 'intersects' may be supplied"
    )
  }
  invisible(NULL)
}

#' Run a request handler, turning bad-request conditions into 400 responses.
#'
#' Plumber's default error handler replaces a handler's error message with a
#' generic body, so a validation failure has to be turned into a normal return
#' value rather than propagated as an error.
#'
#' @param res A plumber response object.
#' @param expr Handler body to evaluate.
#' @return The value of `expr`, or a 400 error body.
#' @noRd
.with_bad_request <- function(res, expr) {
  tryCatch(
    expr,
    stacserver_bad_request = function(e) {
      res$status <- 400L
      .error_body(400L, conditionMessage(e))
    }
  )
}

#' Parse and validate an integer query parameter.
#'
#' Rejects values that are not integers rather than letting `as.integer()`
#' produce `NA`: an `NA` limit reaches PostgreSQL as `LIMIT NULL`, which means
#' no limit at all and would return the entire result set.
#'
#' @param x Raw parameter value.
#' @param name Parameter name, used in the error message.
#' @param default Value to use when `x` is absent or blank.
#' @param min,max Inclusive bounds. Values outside them are rejected.
#' @return An integer scalar.
#' @noRd
.parse_int_param <- function(x, name, default, min = NULL, max = NULL) {
  if (is.null(x) || (length(x) == 1L && is.character(x) && !nzchar(x))) {
    return(as.integer(default))
  }
  if (length(x) != 1L) {
    .abort_bad_request(sprintf("'%s' must be a single integer", name))
  }

  n <- suppressWarnings(as.numeric(x))
  if (is.na(n) || n != trunc(n)) {
    .abort_bad_request(sprintf("'%s' must be an integer, got '%s'", name, x))
  }
  if (!is.null(min) && n < min) {
    .abort_bad_request(sprintf("'%s' must be at least %d", name, as.integer(min)))
  }
  if (!is.null(max) && n > max) {
    .abort_bad_request(sprintf("'%s' must be at most %d", name, as.integer(max)))
  }

  as.integer(n)
}

#' Parse a bbox supplied as a JSON array in a POST body.
#'
#' @param x A parsed JSON array, or `NULL`.
#' @return A numeric vector of length 4 or 6, or `NULL`.
#' @noRd
.parse_bbox_body <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  # as.numeric() on a non-numeric string warns and returns NA rather than
  # erroring, so the NA check in .validate_bbox() is what rejects it.
  vals <- suppressWarnings(as.numeric(unlist(x)))
  vals
}

#' Build the link array for the API landing page.
#'
#' OGC API - Features requires `self`, `root`, `conformance` and `data` on the
#' landing page; the `oas30` conformance class requires `service-desc`; and the
#' STAC Item Search class requires one `search` link per supported method.
#'
#' @param base_url Base URL of the API (no trailing slash).
#' @return A list of STAC link objects.
#' @noRd
.landing_links <- function(base_url) {
  list(
    .link("self", base_url, "application/json"),
    .link("root", base_url, "application/json"),
    .link(
      "conformance",
      paste0(base_url, "/conformance"),
      "application/json"
    ),
    .link("data", paste0(base_url, "/collections"), "application/json"),
    .link(
      "service-desc",
      paste0(base_url, "/openapi.json"),
      "application/vnd.oai.openapi+json;version=3.0"
    ),
    .link("service-doc", paste0(base_url, "/__docs__/"), "text/html"),
    .link(
      "search",
      paste0(base_url, "/search"),
      "application/geo+json",
      method = "GET"
    ),
    .link(
      "search",
      paste0(base_url, "/search"),
      "application/geo+json",
      method = "POST"
    )
  )
}

#' The JSON serializer used for every response.
#'
#' STAC compliance needs single-element R vectors unwrapped and nulls written
#' explicitly. `digits = NA` keeps full numeric precision; jsonlite's default
#' of 4 decimal places would round coordinates to roughly 11 m as they are
#' written to the response.
#'
#' @return A plumber serializer.
#' @noRd
.stac_serializer <- function() {
  plumber::serializer_json(
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}
