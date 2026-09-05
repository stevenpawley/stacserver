#' Sign an Azure Blob Storage href using Azure AD authentication.
#'
#' Generates a short-lived user delegation SAS token using an Azure AD token.
#' Suitable for passing directly as the `sign_fn` argument of
#' [stac_api_router()].
#'
#' @param href Unsigned Azure Blob Storage URL.
#' @param endpoint Full blob service URL, e.g.
#'   `"https://myaccount.blob.core.windows.net/"`. Defaults to the
#'   `AZURE_STORAGE_ENDPOINT` environment variable.
#' @param expiry_seconds Lifetime of the signed URL in seconds (default 3600).
#' @param token An Azure AD token obtained from [AzureAuth::get_managed_token()]
#'   or [AzureAuth::get_azure_token()]. Defaults to a managed identity token,
#'   which works on Azure-hosted infrastructure (VMs, App Service, Container
#'   Apps). For service principal auth, obtain a token with
#'   `AzureAuth::get_azure_token()` and capture it in a closure:
#'   `\(href) sign_azure_ad(href, token = my_token)`.
#' @return A signed URL string with a SAS token appended.
#' @export
sign_azure_ad <- function(
  href,
  endpoint = Sys.getenv("AZURE_STORAGE_ENDPOINT"),
  expiry_seconds = 3600L,
  token = AzureAuth::get_managed_token("https://storage.azure.com/")
) {
  if (!requireNamespace("AzureStor", quietly = TRUE)) {
    cli::cli_abort("Package 'AzureStor' is required for asset signing.")
  }
  if (!requireNamespace("AzureAuth", quietly = TRUE)) {
    cli::cli_abort("Package 'AzureAuth' is required for asset signing.")
  }
  if (!nzchar(endpoint)) {
    cli::cli_abort(
      "'endpoint' is empty. Set AZURE_STORAGE_ENDPOINT or pass it directly."
    )
  }

  # Normalise double slashes that can appear in href (but preserve ://)
  href <- gsub("://", "\001", href, fixed = TRUE)
  href <- gsub("//+", "/", href)
  href <- gsub("\001", "://", href, fixed = TRUE)

  start_time  <- Sys.time() - 300
  expiry_time <- Sys.time() + expiry_seconds

  endp <- AzureStor::storage_endpoint(endpoint, token = token)

  userkey <- AzureStor::get_user_delegation_key(
    endp,
    start  = start_time,
    expiry = expiry_time
  )

  # Strip the endpoint prefix to get container/blobpath
  blob_path <- sub(
    paste0("^", sub("/+$", "", endpoint), "/*"),
    "",
    href
  )
  blob_path <- gsub("//+", "/", blob_path)

  sas_token <- AzureStor::get_user_delegation_sas(
    account       = endp,
    key           = userkey,
    resource      = blob_path,
    expiry        = expiry_time,
    permissions   = "r",
    resource_type = "b"
  )

  paste0(href, "?", sas_token)
}

#' Sign a Google Cloud Storage href using Application Default Credentials.
#'
#' Generates a short-lived V4 signed URL for a GCS object. Authentication is
#' handled by `googleCloudStorageR` / `googleAuthR` — call
#' `googleCloudStorageR::gcs_auth()` (or set `GOOGLE_APPLICATION_CREDENTIALS`)
#' before use. On GCE the metadata server is used automatically. Suitable for
#' passing directly as the `sign_fn` argument of [stac_api_router()].
#'
#' @param href Unsigned GCS URL. Accepts both `gs://bucket/object` and
#'   `https://storage.googleapis.com/bucket/object` forms.
#' @param expiry_seconds Lifetime of the signed URL in seconds (default 3600).
#' @return A signed URL string.
#' @export
sign_gcp <- function(href, expiry_seconds = 3600L) {
  if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) {
    cli::cli_abort(
      "Package 'googleCloudStorageR' is required for GCP asset signing."
    )
  }

  # Parse bucket and object from gs:// or https://storage.googleapis.com/ URLs
  path   <- sub("^gs://", "", href)
  path   <- sub("^https://storage\\.googleapis\\.com/", "", path)
  bucket <- sub("/.*", "", path)
  object <- sub("^[^/]+/", "", path)

  meta_obj <- googleCloudStorageR::gcs_get_object(
    object_name = object,
    bucket      = bucket,
    meta        = TRUE
  )

  googleCloudStorageR::gcs_signed_url(
    meta_obj,
    expiration_ts = Sys.time() + expiry_seconds
  )
}

#' Sign an AWS S3 href using a presigned URL.
#'
#' Generates a short-lived presigned GET URL for an S3 object using
#' `paws.storage`. Authentication follows the standard AWS credential chain:
#' environment variables (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,
#' `AWS_SESSION_TOKEN`), `~/.aws/credentials`, or an IAM instance profile
#' (EC2, ECS, Lambda). Suitable for passing directly as the `sign_fn`
#' argument of [stac_api_router()].
#'
#' @param href Unsigned S3 URL. Accepts `s3://bucket/key`, virtual-hosted
#'   style (`https://bucket.s3.region.amazonaws.com/key`), and path style
#'   (`https://s3.region.amazonaws.com/bucket/key`).
#' @param expiry_seconds Lifetime of the presigned URL in seconds
#'   (default 3600).
#' @param region AWS region. Defaults to the `AWS_DEFAULT_REGION` environment
#'   variable, falling back to `"us-east-1"`.
#' @return A presigned URL string.
#' @export
sign_aws_s3 <- function(
  href,
  expiry_seconds = 3600L,
  region = Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")
) {
  if (!requireNamespace("paws.storage", quietly = TRUE)) {
    cli::cli_abort(
      "Package 'paws.storage' is required for AWS S3 asset signing."
    )
  }

  if (startsWith(href, "s3://")) {
    path   <- sub("^s3://", "", href)
    bucket <- sub("/.*", "", path)
    key    <- sub("^[^/]+/", "", path)
  } else if (grepl("\\.s3[.-].*\\.amazonaws\\.com", href)) {
    # Virtual-hosted: https://bucket.s3[.region].amazonaws.com/key
    bucket <- sub("\\..+", "", sub("^https://", "", href))
    key    <- sub("^https://[^/]+/", "", href)
  } else {
    # Path style: https://s3[.region].amazonaws.com/bucket/key
    path   <- sub("^https://s3[^/]*/", "", href)
    bucket <- sub("/.*", "", path)
    key    <- sub("^[^/]+/", "", path)
  }

  svc <- paws.storage::s3(config = list(region = region))

  svc$generate_presigned_url(
    client_method = "get_object",
    params        = list(Bucket = bucket, Key = key),
    expires_in    = expiry_seconds
  )
}
