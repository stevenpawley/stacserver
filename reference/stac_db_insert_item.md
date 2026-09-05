# Insert or update a STAC Item in the database

Insert or update a STAC Item in the database

## Usage

``` r
stac_db_insert_item(con, item)
```

## Arguments

- con:

  A DBI connection.

- item:

  A
  [`stacbuildr::stac_item()`](https://stevenpawley.github.io/stacbuildr/reference/stac_item.html)
  object. Must have `item@collection` set.

## Value

`item`, invisibly.
