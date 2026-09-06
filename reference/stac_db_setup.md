# Create the STAC database schema

Idempotently creates the `stac_collections` and `stac_items` tables and
all required indexes. Requires the PostGIS extension to be available.

## Usage

``` r
stac_db_setup(con)
```

## Arguments

- con:

  A DBI connection, or a
  [`pool::dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md)
  object. A pool is recommended for long-running servers — see
  [`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md).

## Value

`con`, invisibly.

## Details

`CREATE EXTENSION postgis` needs elevated privileges. On managed
PostgreSQL services PostGIS is usually pre-installed and the connecting
role is not a superuser, so a failure to create the extension is
tolerated as long as PostGIS is actually present; only a genuinely
missing PostGIS aborts.
