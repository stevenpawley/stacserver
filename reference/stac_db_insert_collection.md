# Insert or update a STAC Collection in the database

Insert or update a STAC Collection in the database

## Usage

``` r
stac_db_insert_collection(con, collection)
```

## Arguments

- con:

  A DBI connection.

- collection:

  A
  [`stacbuildr::stac_collection()`](https://stevenpawley.github.io/stacbuildr/reference/stac_collection.html)
  object.

## Value

`collection`, invisibly.
