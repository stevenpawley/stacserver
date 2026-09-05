# Sign a Google Cloud Storage href using Application Default Credentials.

Generates a short-lived V4 signed URL for a GCS object. Authentication
is handled by `googleCloudStorageR` / `googleAuthR` — call
[`googleCloudStorageR::gcs_auth()`](https://cloudyr.github.io/googleCloudStorageR//reference/gcs_auth.html)
(or set `GOOGLE_APPLICATION_CREDENTIALS`) before use. On GCE the
metadata server is used automatically. Suitable for passing directly as
the `sign_fn` argument of
[`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md).

## Usage

``` r
sign_gcp(href, expiry_seconds = 3600L)
```

## Arguments

- href:

  Unsigned GCS URL. Accepts both `gs://bucket/object` and
  `https://storage.googleapis.com/bucket/object` forms.

- expiry_seconds:

  Lifetime of the signed URL in seconds (default 3600).

## Value

A signed URL string.
