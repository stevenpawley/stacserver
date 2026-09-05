# Sign an AWS S3 href using a presigned URL.

Generates a short-lived presigned GET URL for an S3 object using
`paws.storage`. Authentication follows the standard AWS credential
chain: environment variables (`AWS_ACCESS_KEY_ID`,
`AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`), `~/.aws/credentials`, or
an IAM instance profile (EC2, ECS, Lambda). Suitable for passing
directly as the `sign_fn` argument of
[`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md).

## Usage

``` r
sign_aws_s3(
  href,
  expiry_seconds = 3600L,
  region = Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")
)
```

## Arguments

- href:

  Unsigned S3 URL. Accepts `s3://bucket/key`, virtual-hosted style
  (`https://bucket.s3.region.amazonaws.com/key`), and path style
  (`https://s3.region.amazonaws.com/bucket/key`).

- expiry_seconds:

  Lifetime of the presigned URL in seconds (default 3600).

- region:

  AWS region. Defaults to the `AWS_DEFAULT_REGION` environment variable,
  falling back to `"us-east-1"`.

## Value

A presigned URL string.
