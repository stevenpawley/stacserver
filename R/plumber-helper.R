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
  list(
    "https://api.stacspec.org/v1.0.0/core",
    "https://api.stacspec.org/v1.0.0/item-search",
    "https://api.stacspec.org/v1.0.0/item-search#fields",
    "https://api.stacspec.org/v1.0.0/ogcapi-features",
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
#' @return A named list representing a single STAC link object.
#' @noRd
.link <- function(rel, href, type = NULL, method = NULL) {
  lnk <- list(rel = rel, href = href)
  if (!is.null(type)) {
    lnk$type <- type
  }
  if (!is.null(method)) {
    lnk$method <- method
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
#' @param existing A list of existing link objects, or `NULL`.
#' @param new_links A list of link objects to append.
#' @return A combined list of link objects with no duplicate `rel`/`href` pairs.
#' @noRd
.merge_links <- function(existing, new_links) {
  existing <- existing %||% list()
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
#' this page. The `context` field repeats these counts for compatibility with
#' the STAC API Context extension.
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
    context = list(returned = returned, matched = matched),
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
#' @param base_url Base URL of the endpoint (no query string).
#' @param offset Zero-based index of the first item on the current page.
#' @param limit Maximum number of items per page.
#' @param matched Total number of items matching the query.
#' @param extra_query Optional pre-encoded query string fragment (without
#'   leading `?` or `&`) for additional filter parameters such as `bbox` or
#'   `datetime`.
#' @return A list of STAC link objects.
#' @noRd
.pagination_links <- function(
  base_url,
  offset,
  limit,
  matched,
  extra_query = ""
) {
  sep <- if (nzchar(extra_query)) "&" else "?"
  links <- list(
    .link(
      "self",
      paste0(base_url, "?limit=", limit, "&offset=", offset, sep, extra_query),
      "application/geo+json"
    )
  )
  if (offset + limit < matched) {
    links <- c(
      links,
      list(.link(
        "next",
        paste0(
          base_url,
          "?limit=",
          limit,
          "&offset=",
          offset + limit,
          sep,
          extra_query
        ),
        "application/geo+json"
      ))
    )
  }
  if (offset > 0L) {
    links <- c(
      links,
      list(.link(
        "prev",
        paste0(
          base_url,
          "?limit=",
          limit,
          "&offset=",
          max(0L, offset - limit),
          sep,
          extra_query
        ),
        "application/geo+json"
      ))
    )
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
    list(
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
