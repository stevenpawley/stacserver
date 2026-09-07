test_that("signed item responses prohibit caching across item and search routes", {
  item <- list(id = "one", collection = "demo",
               assets = list(data = list(href = "https://example.com/data")))
  local_mocked_bindings(
    .db_get_collection = function(...) list(id = "demo"),
    .db_get_item = function(...) item,
    .db_search_items = function(...) list(items = list(item), matched = 1L)
  )

  request <- function(router, path, method = "GET") {
    req <- new.env()
    req$REQUEST_METHOD <- method
    req$PATH_INFO <- path
    req$QUERY_STRING <- ""
    req$rook.input <- list(read = function(...) raw())
    router$call(req)
  }

  signed <- stac_api_router(NULL, sign_fn = function(href) paste0(href, "?sig=test"))
  plain <- stac_api_router(NULL)
  for (path in c("/collections/demo/items/one", "/collections/demo/items", "/search")) {
    response <- request(signed, path)
    expect_identical(response$status, 200L)
    expect_identical(response$headers[["Cache-Control"]], "private, no-store")
    expect_match(as.character(response$body), "sig=test", fixed = TRUE)
    expect_null(request(plain, path)$headers[["Cache-Control"]])
  }
  response <- request(signed, "/search", "POST")
  expect_identical(response$status, 200L)
  expect_identical(response$headers[["Cache-Control"]], "private, no-store")
  expect_match(as.character(response$body), "sig=test", fixed = TRUE)

  failing <- stac_api_router(NULL, sign_fn = function(href) stop("failed"))
  expect_warning(response <- request(failing, "/collections/demo/items/one"),
                 "Asset signing failed")
  expect_identical(response$status, 200L)
  expect_identical(response$headers[["Cache-Control"]], "private, no-store")
})
