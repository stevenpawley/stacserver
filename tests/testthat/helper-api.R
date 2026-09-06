# Helpers for the HTTP-level tests.
#
# These drive the router through `call(req)`, the Rook entry point httpuv
# itself invokes, rather than calling the handler helpers directly. Routing,
# filters, body parsing and the response serializer are therefore all covered:
# a route registered with the wrong parameter name, a filter that swallows a
# request, or a body that fails to serialize is invisible to every other test
# in this suite.
#
# What they skip is the socket, which belongs to httpuv and not to this
# package. In exchange they need no second process, no port and no HTTP
# client, so they are as quick and as reproducible as the rest of the suite.
#
# The router needs a database, so these are skipped alongside the tests in
# test-db.R; see helper-db.R.

# The base URL the fixture router is built with. Responses are asserted
# against it, so links that are built from something else stand out.
api_base_url <- "http://api.example.com"

# A rook.input over a request body, supporting the three operations plumber
# uses to read one.
api_rook_input <- function(text) {
  bytes <- charToRaw(text %||% "")
  pos <- 0L
  list(
    read = function(length = -1L) {
      n <- if (length < 0L) {
        length(bytes) - pos
      } else {
        min(length, length(bytes) - pos)
      }
      out <- bytes[seq_len(n) + pos]
      pos <<- pos + n
      out
    },
    read_lines = function(n = -1L) {
      if (length(bytes) == 0L) {
        return(character(0))
      }
      strsplit(rawToChar(bytes), "\n", fixed = TRUE)[[1]]
    },
    rewind = function() {
      pos <<- 0L
      invisible(NULL)
    }
  )
}

# Build the request environment plumber expects.
api_request <- function(method, path, query = "", body = NULL) {
  text <- if (is.null(body)) {
    NULL
  } else if (is.character(body)) {
    # Sent verbatim, so that a malformed body can be tested
    body
  } else {
    as.character(jsonlite::toJSON(
      body,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ))
  }

  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- toupper(method)
  req$PATH_INFO <- path
  req$QUERY_STRING <- query
  req$SCRIPT_NAME <- ""
  req$SERVER_NAME <- "127.0.0.1"
  req$rook.input <- api_rook_input(text)
  if (!is.null(text)) {
    req$CONTENT_TYPE <- "application/json"
    req$HTTP_CONTENT_TYPE <- "application/json"
  }
  req
}

# Send a request and return the status, headers and decoded body. `text` keeps
# the response exactly as it went over the wire, for assertions about the
# serialization itself.
api_call <- function(pr, req) {
  res <- pr$call(req)
  text <- if (is.raw(res$body)) rawToChar(res$body) else as.character(res$body)
  list(
    status = res$status,
    headers = res$headers,
    text = text,
    body = tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      error = function(e) NULL
    )
  )
}

# Encode named arguments as a query string, dropping those that are NULL.
api_query <- function(...) {
  args <- Filter(Negate(is.null), list(...))
  if (length(args) == 0L) {
    return("")
  }
  paste(
    mapply(
      function(k, v) paste0(k, "=", utils::URLencode(as.character(v), reserved = TRUE)),
      names(args),
      args
    ),
    collapse = "&"
  )
}

api_get <- function(pr, path, ...) {
  api_call(pr, api_request("GET", path, api_query(...)))
}

api_post <- function(pr, path, body = NULL) {
  api_call(pr, api_request("POST", path, body = body))
}

# Follow a link from a response the way a client does: using the href, method
# and body the link itself declares rather than rebuilding the request by
# hand. This is what makes the paging tests meaningful, because they exercise
# the links the API actually emits.
api_follow <- function(pr, link) {
  path <- sub(api_base_url, "", link$href, fixed = TRUE)
  query <- ""
  if (grepl("?", path, fixed = TRUE)) {
    query <- sub("^[^?]*\\?", "", path)
    path <- sub("\\?.*$", "", path)
  }
  if (identical(link$method %||% "GET", "POST")) {
    return(api_call(pr, api_request("POST", path, query, body = link$body)))
  }
  api_call(pr, api_request("GET", path, query))
}

# Pull the link with a given rel out of a response body.
api_link <- function(response, rel) {
  hit <- Filter(function(l) identical(l$rel, rel), response$body$links)
  if (length(hit) == 0L) NULL else hit[[1]]
}

api_ids <- function(response) {
  vapply(response$body$features, function(f) f$id, character(1))
}

# A router over a collection of four items: three inside the triangle used by
# the intersects tests, and one that only a bounding box would reach.
test_api <- function(con, cid, ...) {
  stac_db_insert_collection(con, test_collection(cid, bbox = c(-1, -1, 2, 2)))

  assets <- list(
    dem = list(
      href = "https://store.example.com/dem.tif",
      type = "image/tiff",
      roles = list("data")
    )
  )
  stac_db_insert_item(con, test_item("t1", cid, 0.5, 0.1, "2024-06-01T00:00:00Z", assets))
  stac_db_insert_item(con, test_item("t2", cid, 0.5, 0.3, "2024-06-02T00:00:00Z", assets))
  stac_db_insert_item(con, test_item("t3", cid, 0.5, 0.5, "2024-06-03T00:00:00Z", assets))
  stac_db_insert_item(con, test_item("corner", cid, 0.02, 0.95, "2024-06-04T00:00:00Z", assets))

  stac_api_router(con, base_url = api_base_url, ...)
}

# A triangle covering t1, t2 and t3 but not corner, which sits inside its
# bounding box but outside the triangle itself.
api_triangle <- function() {
  list(
    type = "Polygon",
    coordinates = list(list(
      list(0, 0),
      list(1, 0),
      list(0.5, 1),
      list(0, 0)
    ))
  )
}

api_triangle_json <- function() {
  as.character(jsonlite::toJSON(api_triangle(), auto_unbox = TRUE, digits = NA))
}
