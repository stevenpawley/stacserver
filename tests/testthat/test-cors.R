# CORS configuration. The filter is exercised directly rather than through a
# running server: it is an ordinary function of (req, res), so a stub response
# recording the headers it is given is enough.

fake_res <- function() {
  headers <- new.env(parent = emptyenv())
  list(
    headers = headers,
    setHeader = function(name, value) assign(name, value, envir = headers),
    status = 200L
  )
}

fake_req <- function(origin = NULL, method = "GET") {
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- method
  if (!is.null(origin)) {
    req$HTTP_ORIGIN <- origin
  }
  req
}

headers_of <- function(res) {
  as.list(res$headers)
}

test_that(".check_cors_origins accepts NULL, the wildcard and real origins", {
  expect_null(.check_cors_origins(NULL))
  expect_equal(.check_cors_origins("*"), "*")
  expect_equal(
    .check_cors_origins("https://Browser.Example.com"),
    "https://browser.example.com"
  )
  expect_equal(
    .check_cors_origins(c("https://a.example.com", "http://localhost:3000")),
    c("https://a.example.com", "http://localhost:3000")
  )
})

test_that(".check_cors_origins rejects an origin carrying a path", {
  # The mistake this catches would otherwise surface as an unexplained CORS
  # failure in the browser, because a page never sends its path as the origin.
  expect_error(
    .check_cors_origins("https://example.com/stac-browser"),
    "Not an origin"
  )
  expect_error(.check_cors_origins("https://example.com/"), "Not an origin")
  expect_error(.check_cors_origins("example.com"), "Not an origin")
})

test_that(".check_cors_origins refuses the wildcard mixed with named origins", {
  expect_error(
    .check_cors_origins(c("*", "https://example.com")),
    "cannot combine"
  )
  expect_error(.check_cors_origins(character(0)), "must be NULL")
  expect_error(.check_cors_origins(42), "must be NULL")
  expect_error(.check_cors_origins(NA_character_), "must be NULL")
})

test_that("the wildcard filter allows every origin without varying", {
  filt <- .cors_filter("*")
  res <- fake_res()

  # forward() signals plumber to continue to the route; it errors outside a
  # request, which is exactly the point reached once the headers are set.
  try(filt(fake_req(origin = "https://anywhere.example.com"), res), silent = TRUE)
  h <- headers_of(res)

  expect_equal(h[["Access-Control-Allow-Origin"]], "*")
  expect_equal(h[["Access-Control-Allow-Methods"]], "GET, POST, OPTIONS")
  # A constant header does not vary by origin
  expect_null(h[["Vary"]])
})

test_that("an allow-listed origin is echoed back and marked as varying", {
  filt <- .cors_filter(c("https://browser.example.com", "http://localhost:3000"))

  res <- fake_res()
  try(filt(fake_req(origin = "https://browser.example.com"), res), silent = TRUE)
  h <- headers_of(res)

  # The origin itself, never "*", so the browser can also send credentials
  expect_equal(h[["Access-Control-Allow-Origin"]], "https://browser.example.com")
  # Without this a shared cache could hand this header to another origin
  expect_equal(h[["Vary"]], "Origin")

  res2 <- fake_res()
  try(filt(fake_req(origin = "http://localhost:3000"), res2), silent = TRUE)
  expect_equal(
    headers_of(res2)[["Access-Control-Allow-Origin"]],
    "http://localhost:3000"
  )
})

test_that("an origin that is not on the list gets no allow header", {
  filt <- .cors_filter("https://browser.example.com")

  res <- fake_res()
  try(filt(fake_req(origin = "https://evil.example.com"), res), silent = TRUE)
  h <- headers_of(res)

  expect_null(h[["Access-Control-Allow-Origin"]])
  # Still varies: the refusal is specific to this origin
  expect_equal(h[["Vary"]], "Origin")

  # A lookalike host must not match either
  res2 <- fake_res()
  try(filt(fake_req(origin = "https://browser.example.com.evil.com"), res2), silent = TRUE)
  expect_null(headers_of(res2)[["Access-Control-Allow-Origin"]])

  # Nor a request carrying no Origin at all
  res3 <- fake_res()
  try(filt(fake_req(), res3), silent = TRUE)
  expect_null(headers_of(res3)[["Access-Control-Allow-Origin"]])
})

test_that("an origin matches regardless of the case a browser sends", {
  filt <- .cors_filter(.check_cors_origins("https://Browser.Example.com"))
  res <- fake_res()
  try(filt(fake_req(origin = "https://browser.example.com"), res), silent = TRUE)
  expect_equal(
    headers_of(res)[["Access-Control-Allow-Origin"]],
    "https://browser.example.com"
  )
})

test_that("a preflight is answered directly rather than forwarded", {
  filt <- .cors_filter("https://browser.example.com")
  res <- fake_res()

  # No try(): a preflight returns instead of calling forward()
  out <- filt(
    fake_req(origin = "https://browser.example.com", method = "OPTIONS"),
    res
  )

  expect_equal(out, list())
  expect_equal(res$status, 200L)
  expect_equal(
    headers_of(res)[["Access-Control-Allow-Origin"]],
    "https://browser.example.com"
  )
})

filter_names <- function(pr) {
  vapply(pr$filters, function(f) f$name, character(1))
}

test_that("the router registers a cors filter only when origins are given", {
  skip_if_not_installed("plumber")

  # The default API sends no CORS headers and does not intercept OPTIONS.
  # con is never touched while the router is only being built.
  plain <- stac_api_router(con = NULL)
  expect_false("cors" %in% filter_names(plain))

  open <- stac_api_router(
    con = NULL,
    cors_origins = "https://browser.example.com"
  )
  expect_true("cors" %in% filter_names(open))

  # A bad origin is refused when the router is built, not on the first request
  expect_error(
    stac_api_router(con = NULL, cors_origins = "https://example.com/browser"),
    "Not an origin"
  )
})
