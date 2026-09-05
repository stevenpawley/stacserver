# Sign an Azure Blob Storage href using Azure AD authentication.

Generates a short-lived user delegation SAS token using an Azure AD
token. Suitable for passing directly as the `sign_fn` argument of
[`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md).

## Usage

``` r
sign_azure_ad(
  href,
  endpoint = Sys.getenv("AZURE_STORAGE_ENDPOINT"),
  expiry_seconds = 3600L,
  token = AzureAuth::get_managed_token("https://storage.azure.com/")
)
```

## Arguments

- href:

  Unsigned Azure Blob Storage URL.

- endpoint:

  Full blob service URL, e.g.
  `"https://myaccount.blob.core.windows.net/"`. Defaults to the
  `AZURE_STORAGE_ENDPOINT` environment variable.

- expiry_seconds:

  Lifetime of the signed URL in seconds (default 3600).

- token:

  An Azure AD token obtained from
  [`AzureAuth::get_managed_token()`](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
  or
  [`AzureAuth::get_azure_token()`](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html).
  Defaults to a managed identity token, which works on Azure-hosted
  infrastructure (VMs, App Service, Container Apps). For service
  principal auth, obtain a token with
  [`AzureAuth::get_azure_token()`](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
  and capture it in a closure:
  `\(href) sign_azure_ad(href, token = my_token)`.

## Value

A signed URL string with a SAS token appended.
