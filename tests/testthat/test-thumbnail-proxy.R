test_that("thumbnail proxy mode rewrites only thumbnail asset hrefs", {
  item <- list(
    id = "item one",
    collection = "demo",
    assets = list(
      thumbnail = list(
        href = "https://storage.example/thumb.png",
        type = "image/png",
        roles = list("thumbnail")
      ),
      data = list(href = "https://storage.example/data.tif")
    )
  )
  local_mocked_bindings(
    .db_get_item = function(...) item,
    .db_get_collection = function(...) list(id = "demo")
  )

  request <- function(router, path) {
    req <- new.env()
    req$REQUEST_METHOD <- "GET"
    req$PATH_INFO <- path
    req$QUERY_STRING <- ""
    req$rook.input <- list(read = function(...) raw())
    router$call(req)
  }

  router <- stac_api_router(
    NULL,
    base_url = "https://api.example.test",
    sign_fn = function(href) paste0(href, "?sig=server-only"),
    proxy_thumbnails = TRUE
  )
  response <- request(router, "/collections/demo/items/item%20one")
  body_text <- if (is.raw(response$body)) rawToChar(response$body) else response$body
  body <- jsonlite::fromJSON(body_text, simplifyVector = FALSE)

  expect_equal(
    body$assets$thumbnail$href,
    "https://api.example.test/thumbnails/demo/item%20one/thumbnail"
  )
  expect_equal(
    body$assets$data$href,
    "https://storage.example/data.tif?sig=server-only"
  )
})

test_that("thumbnail proxy rejects non-thumbnail assets", {
  item <- list(
    id = "one", collection = "demo",
    assets = list(data = list(href = "https://storage.example/data.tif"))
  )
  local_mocked_bindings(.db_get_item = function(...) item)
  res <- new.env()
  res$status <- 200L
  res$setHeader <- function(...) NULL

  result <- .serve_thumbnail(NULL, res, "demo", "one", "data", identity)
  expect_equal(res$status, 404L)
  expect_equal(result$code, 404L)
})
