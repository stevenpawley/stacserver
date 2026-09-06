# Create a reusable Azure Blob Storage signing function

Returns a `function(href)` suitable for the `sign_fn` argument of
[`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md).
The returned function fetches the user delegation key **once** and
reuses it for every href it signs, refreshing only when the key is close
enough to expiry that it could no longer cover a new signature.

## Usage

``` r
azure_signer(
  endpoint = Sys.getenv("AZURE_STORAGE_ENDPOINT"),
  expiry_seconds = 3600L,
  key_lifetime_seconds = 24L * 3600L,
  token = NULL
)
```

## Arguments

- endpoint:

  Full blob service URL, e.g.
  `"https://myaccount.blob.core.windows.net/"`. Defaults to the
  `AZURE_STORAGE_ENDPOINT` environment variable.

- expiry_seconds:

  Lifetime of each signed URL in seconds (default 3600).

- key_lifetime_seconds:

  How long each cached user delegation key is requested for (default 24
  hours). Azure caps this at seven days, and it must exceed
  `expiry_seconds`, otherwise every signature would need a fresh key and
  the cache would buy nothing.

- token:

  An Azure AD token from
  [`AzureAuth::get_managed_token()`](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
  or
  [`AzureAuth::get_azure_token()`](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html).
  Defaults to `NULL`, meaning a managed identity token is obtained when
  a key is needed — the usual choice on Azure-hosted infrastructure. For
  service principal auth, obtain a token with
  [`AzureAuth::get_azure_token()`](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
  and pass it here; a supplied token is refreshed when it goes stale.

## Value

A function of one argument (`href`) returning a signed URL. Call it
directly to sign a single href: `azure_signer()(href)`.

## Details

This matters for a server. Minting a user delegation key is a network
round trip to Azure, and the router signs every asset href in every
response, so an items page holding ten items with four assets apiece
would otherwise make forty such calls before it could reply. Computing
the SAS itself is local, so only the key needs caching.

Each signature is read-only, scoped to the single blob it names, valid
for `expiry_seconds`, and restricted to HTTPS. An href that does not
point at `endpoint` is returned unchanged, so a catalog holding a
mixture of private blobs and public URLs is left alone where it should
be.

## Examples

``` r
if (FALSE) { # \dontrun{
router <- stac_api_router(
  con,
  base_url = "https://stac.example.com",
  sign_fn = azure_signer(expiry_seconds = 3600)
)
} # }
```
