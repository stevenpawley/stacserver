# Azure signing. The Azure services are never contacted: the AzureStor and
# AzureAuth entry points are mocked so the caching behaviour can be asserted
# by counting how often a delegation key is requested.

fake_azure <- function(counter) {
  testthat::local_mocked_bindings(
    get_managed_token = function(...) {
      structure(list(), class = c("dummy_token", "R6"))
    },
    .package = "AzureAuth",
    .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    storage_endpoint = function(endpoint, ...) {
      structure(
        list(url = endpoint),
        class = c("blob_endpoint", "storage_endpoint")
      )
    },
    get_user_delegation_key = function(...) {
      counter$keys <- counter$keys + 1L
      structure(list(), class = "user_delegation_key")
    },
    get_user_delegation_sas = function(account, key, resource, ...) {
      counter$sas <- counter$sas + 1L
      paste0("sv=2024-01-01&sig=", counter$sas)
    },
    .package = "AzureStor",
    .env = parent.frame()
  )
}

endpoint <- "https://myaccount.blob.core.windows.net/"

test_that("azure_signer fetches one delegation key for many hrefs", {
  skip_if_not_installed("AzureStor")
  skip_if_not_installed("AzureAuth")

  counter <- new.env(parent = emptyenv())
  counter$keys <- 0L
  counter$sas <- 0L
  fake_azure(counter)

  sign <- azure_signer(endpoint = endpoint)
  hrefs <- sprintf("%sdata/dem-%02d.tif", endpoint, 1:20)
  signed <- vapply(hrefs, sign, character(1), USE.NAMES = FALSE)

  # The whole point: one round trip to Azure, not one per asset
  expect_equal(counter$keys, 1L)
  # But every href still gets its own SAS, computed locally
  expect_equal(counter$sas, 20L)
  expect_length(unique(signed), 20L)
  expect_true(all(grepl("?sv=2024-01-01&sig=", signed, fixed = TRUE)))
})

test_that("azure_signer renews the key before a SAS could outlive it", {
  skip_if_not_installed("AzureStor")
  skip_if_not_installed("AzureAuth")

  counter <- new.env(parent = emptyenv())
  counter$keys <- 0L
  counter$sas <- 0L
  fake_azure(counter)

  sign <- azure_signer(endpoint = endpoint, expiry_seconds = 3600)

  sign(paste0(endpoint, "a.tif"))
  expect_equal(counter$keys, 1L)

  # A second href reuses the key rather than fetching another
  sign(paste0(endpoint, "b.tif"))
  expect_equal(counter$keys, 1L)

  # Age the cached key so it has only seconds left. A fresh 1 h SAS would now
  # outlive it, so the signer has to replace it rather than sign with it.
  # Reaching into the closure avoids having to mock the clock.
  environment(sign)$key_expiry <- Sys.time() + 10
  sign(paste0(endpoint, "c.tif"))
  expect_equal(counter$keys, 2L)

  # And the replacement is good for a long while again
  sign(paste0(endpoint, "d.tif"))
  expect_equal(counter$keys, 2L)
})

test_that("a signer can be called directly for a single href", {
  skip_if_not_installed("AzureStor")
  skip_if_not_installed("AzureAuth")

  counter <- new.env(parent = emptyenv())
  counter$keys <- 0L
  counter$sas <- 0L
  fake_azure(counter)

  signed <- azure_signer(endpoint = endpoint)(paste0(endpoint, "dem.tif"))
  expect_match(signed, "^https://myaccount\\.blob\\.core\\.windows\\.net/dem\\.tif\\?sv=")
  expect_equal(counter$keys, 1L)
})

test_that("azure_signer refuses a configuration that could not cache", {
  skip_if_not_installed("AzureStor")
  skip_if_not_installed("AzureAuth")

  # A key no longer-lived than the SAS would be refetched every time
  expect_error(
    azure_signer(endpoint = endpoint, expiry_seconds = 3600, key_lifetime_seconds = 3600),
    "must be greater than"
  )
  expect_error(
    azure_signer(endpoint = endpoint, key_lifetime_seconds = 8 * 24 * 3600),
    "seven days"
  )
  expect_error(
    azure_signer(endpoint = endpoint, expiry_seconds = 0),
    "positive number"
  )
})

test_that("azure_signer checks its configuration when it is built", {
  skip_if_not_installed("AzureStor")
  skip_if_not_installed("AzureAuth")

  # Failing at startup beats failing once per asset at request time
  withr::with_envvar(c(AZURE_STORAGE_ENDPOINT = ""), {
    expect_error(azure_signer(), "endpoint' is empty")
  })
})

test_that(".key_still_usable keeps a SAS inside its key's lifetime", {
  now <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

  expect_false(.key_still_usable(NULL, now, 3600))
  # Two hours left, one hour needed
  expect_true(.key_still_usable(now + 7200, now, 3600))
  # Exactly the SAS lifetime left: too tight once the margin is applied
  expect_false(.key_still_usable(now + 3600, now, 3600))
  expect_false(.key_still_usable(now + 3660, now, 3600))
  expect_true(.key_still_usable(now + 3721, now, 3600))
  # Already expired
  expect_false(.key_still_usable(now - 10, now, 3600))
})

test_that(".azure_normalise_href collapses double slashes but keeps the scheme", {
  expect_equal(
    .azure_normalise_href("https://acct.blob.core.windows.net//container//a.tif"),
    "https://acct.blob.core.windows.net/container/a.tif"
  )
  expect_equal(
    .azure_normalise_href("https://acct.blob.core.windows.net/a.tif"),
    "https://acct.blob.core.windows.net/a.tif"
  )
})

test_that(".azure_blob_path strips the endpoint prefix", {
  expect_equal(
    .azure_blob_path("https://acct.blob.core.windows.net/c/a.tif", "https://acct.blob.core.windows.net/"),
    "c/a.tif"
  )
  # A trailing slash on the endpoint is optional
  expect_equal(
    .azure_blob_path("https://acct.blob.core.windows.net/c/a.tif", "https://acct.blob.core.windows.net"),
    "c/a.tif"
  )
})
