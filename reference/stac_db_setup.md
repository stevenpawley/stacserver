# Create the STAC database schema

Idempotently creates the `stac_collections` and `stac_items` tables and
all required indexes. Requires the PostGIS extension to be available.

## Usage

``` r
stac_db_setup(con)
```

## Arguments

- con:

  A DBI connection.

## Value

`con`, invisibly.
