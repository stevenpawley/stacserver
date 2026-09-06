.azure_require_packages <- function() {
  if (!requireNamespace("AzureStor", quietly = TRUE)) {
    cli::cli_abort("Package 'AzureStor' is required for asset signing.")
  }
  if (!requireNamespace("AzureAuth", quietly = TRUE)) {
    cli::cli_abort("Package 'AzureAuth' is required for asset signing.")
  }
  invisible(TRUE)
}

.azure_check_endpoint <- function(endpoint) {
  if (!nzchar(endpoint)) {
    cli::cli_abort(
      "'endpoint' is empty. Set AZURE_STORAGE_ENDPOINT or pass it directly."
    )
  }
  endpoint
}

# Normalise double slashes in an href, preserving the "://" after the scheme.
.azure_normalise_href <- function(href) {
  href <- gsub("://", "\001", href, fixed = TRUE)
  href <- gsub("//+", "/", href)
  gsub("\001", "://", href, fixed = TRUE)
}

# Strip the endpoint prefix from an href to leave container/blobpath.
.azure_blob_path <- function(href, endpoint) {
  blob_path <- sub(paste0("^", sub("/+$", "", endpoint), "/*"), "", href)
  gsub("//+", "/", blob_path)
}

# Obtain an AAD token, refreshing a supplied one that has gone stale so that a
# long-lived signer keeps working.
.azure_token <- function(supplied = NULL) {
  if (is.null(supplied)) {
    return(AzureAuth::get_managed_token("https://storage.azure.com/"))
  }
  if (inherits(supplied, "R6") && is.function(supplied$validate)) {
    valid <- tryCatch(isTRUE(supplied$validate()), error = function(e) TRUE)
    if (!valid && is.function(supplied$refresh)) {
      tryCatch(supplied$refresh(), error = function(e) NULL)
    }
  }
  supplied
}

# TRUE when a cached delegation key can still sign a SAS that expires
# `expiry_seconds` from `now`. A SAS must not outlive the key that signed it,
# so the key is replaced before that could happen.
.key_still_usable <- function(key_expiry, now, expiry_seconds, margin = 60) {
  if (is.null(key_expiry)) {
    return(FALSE)
  }
  as.numeric(difftime(key_expiry, now, units = "secs")) >
    expiry_seconds + margin
}

#' Create a reusable Azure Blob Storage signing function
#'
#' Returns a `function(href)` suitable for the `sign_fn` argument of
#' [stac_api_router()]. The returned function fetches the user delegation key
#' **once** and reuses it for every href it signs, refreshing only when the key
#' is close enough to expiry that it could no longer cover a new signature.
#'
#' This matters for a server. Minting a user delegation key is a network round
#' trip to Azure, and the router signs every asset href in every response, so
#' an items page holding ten items with four assets apiece would otherwise make
#' forty such calls before it could reply. Computing the SAS itself is local,
#' so only the key needs caching.
#'
#' @param endpoint Full blob service URL, e.g.
#'   `"https://myaccount.blob.core.windows.net/"`. Defaults to the
#'   `AZURE_STORAGE_ENDPOINT` environment variable.
#' @param expiry_seconds Lifetime of each signed URL in seconds (default 3600).
#' @param key_lifetime_seconds How long each cached user delegation key is
#'   requested for (default 24 hours). Azure caps this at seven days, and it
#'   must exceed `expiry_seconds`, otherwise every signature would need a fresh
#'   key and the cache would buy nothing.
#' @param token An Azure AD token from [AzureAuth::get_managed_token()] or
#'   [AzureAuth::get_azure_token()]. Defaults to `NULL`, meaning a managed
#'   identity token is obtained when a key is needed — the usual choice on
#'   Azure-hosted infrastructure. For service principal auth, obtain a token
#'   with [AzureAuth::get_azure_token()] and pass it here; a supplied token is
#'   refreshed when it goes stale.
#' @return A function of one argument (`href`) returning a signed URL. Call it
#'   directly to sign a single href: `azure_signer()(href)`.
#' @export
#' @examples
#' \dontrun{
#' router <- stac_api_router(
#'   con,
#'   base_url = "https://stac.example.com",
#'   sign_fn = azure_signer(expiry_seconds = 3600)
#' )
#' }
azure_signer <- function(
  endpoint = Sys.getenv("AZURE_STORAGE_ENDPOINT"),
  expiry_seconds = 3600L,
  key_lifetime_seconds = 24L * 3600L,
  token = NULL
) {
  # Configuration is checked when the signer is built rather than on the first
  # request, so a misconfigured deployment fails at startup.
  .azure_require_packages()
  endpoint <- .azure_check_endpoint(endpoint)

  expiry_seconds <- as.numeric(expiry_seconds)
  key_lifetime_seconds <- as.numeric(key_lifetime_seconds)

  if (is.na(expiry_seconds) || expiry_seconds <= 0) {
    cli::cli_abort("'expiry_seconds' must be a positive number.")
  }
  if (key_lifetime_seconds > 7 * 24 * 3600) {
    cli::cli_abort(
      "Azure caps a user delegation key at seven days, so
       'key_lifetime_seconds' cannot exceed {7 * 24 * 3600}."
    )
  }
  if (key_lifetime_seconds <= expiry_seconds) {
    cli::cli_abort(c(
      "'key_lifetime_seconds' must be greater than 'expiry_seconds'.",
      i = "Otherwise every signature would need its own delegation key, which
           is what this function exists to avoid."
    ))
  }

  supplied_token <- token
  endp <- NULL
  key <- NULL
  key_expiry <- NULL

  ensure_key <- function(now) {
    if (!is.null(key) && .key_still_usable(key_expiry, now, expiry_seconds)) {
      return(invisible(FALSE))
    }
    tok <- .azure_token(supplied_token)
    endp <<- AzureStor::storage_endpoint(endpoint, token = tok)
    new_expiry <- now + key_lifetime_seconds
    # The argument names matter: get_user_delegation_key() takes key_start and
    # key_expiry, and silently ignores anything else through its dots.
    key <<- AzureStor::get_user_delegation_key(
      endp,
      key_start = now - 300,
      key_expiry = new_expiry
    )
    key_expiry <<- new_expiry
    invisible(TRUE)
  }

  function(href) {
    now <- Sys.time()
    ensure_key(now)

    href <- .azure_normalise_href(href)
    sas_token <- AzureStor::get_user_delegation_sas(
      account = endp,
      key = key,
      resource = .azure_blob_path(href, endpoint),
      start = now - 300,
      expiry = now + expiry_seconds,
      permissions = "r",
      resource_type = "b"
    )

    paste0(href, "?", sas_token)
  }
}
