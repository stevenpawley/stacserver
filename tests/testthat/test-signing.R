# Asset href signing. The cloud backends are not exercised; the signing hook
# itself is, with a stub sign_fn.

test_that(".sign_item_assets rewrites every asset href", {
  item <- list(
    id = "dem-001",
    assets = list(
      dem = list(href = "s3://bucket/dem.tif", type = "image/tiff"),
      thumb = list(href = "s3://bucket/thumb.png", type = "image/png")
    )
  )
  signed <- .sign_item_assets(item, function(href) paste0(href, "?sig=abc"))

  expect_equal(signed$assets$dem$href, "s3://bucket/dem.tif?sig=abc")
  expect_equal(signed$assets$thumb$href, "s3://bucket/thumb.png?sig=abc")
  # Other asset fields are untouched
  expect_equal(signed$assets$dem$type, "image/tiff")
})

test_that(".sign_item_assets leaves an item with no assets alone", {
  expect_equal(.sign_item_assets(list(id = "a"), identity), list(id = "a"))
  item <- list(id = "a", assets = list())
  expect_equal(.sign_item_assets(item, identity), item)
})

test_that(".sign_item_assets warns and keeps the original href when signing fails", {
  item <- list(assets = list(dem = list(href = "s3://bucket/dem.tif")))
  expect_warning(
    signed <- .sign_item_assets(item, function(href) stop("no credentials")),
    "Asset signing failed"
  )
  expect_equal(signed$assets$dem$href, "s3://bucket/dem.tif")
})

test_that(".sign_item_assets skips an asset with no href", {
  item <- list(assets = list(dem = list(title = "No href here")))
  signed <- .sign_item_assets(item, function(href) "signed")
  expect_null(signed$assets$dem$href)
  expect_equal(signed$assets$dem$title, "No href here")
})

test_that("signing failure warnings do not disclose URLs or backend credentials", {
  href <- "https://user:password@acct.blob.core.windows.net/private/dem.tif?sig=secret-sas"
  item <- list(assets = list(dem = list(href = href)))
  warnings <- character()

  signed <- withCallingHandlers(
    .sign_item_assets(item, function(href) {
      stop(paste("Request failed:", href, "Authorization: Bearer secret-token"))
    }),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_match(warnings, "Asset signing failed")
  for (sensitive in c(href, "password", "private/dem.tif", "secret-sas",
                      "Authorization", "secret-token", "Request failed")) {
    expect_false(grepl(sensitive, warnings, fixed = TRUE))
  }
  expect_identical(signed, item)
})
