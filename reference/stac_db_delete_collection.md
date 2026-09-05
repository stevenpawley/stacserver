# Delete a STAC Collection and all its items from the database

Items are removed via the `ON DELETE CASCADE` foreign key.

## Usage

``` r
stac_db_delete_collection(con, id)
```

## Arguments

- con:

  A DBI connection.

- id:

  Collection ID.

## Value

`NULL`, invisibly.
