# Package index

## Overview

- [`stacserver`](https://stevenpawley.github.io/stacserver/reference/stacserver-package.md)
  [`stacserver-package`](https://stevenpawley.github.io/stacserver/reference/stacserver-package.md)
  : stacserver: Serve SpatioTemporal Asset Catalogs over a STAC API

## Database Backend

Set up and manage a PostgreSQL/PostGIS database backing store for STAC
Collections and Items.

- [`stac_db_setup()`](https://stevenpawley.github.io/stacserver/reference/stac_db_setup.md)
  : Create the STAC database schema
- [`stac_db_insert_collection()`](https://stevenpawley.github.io/stacserver/reference/stac_db_insert_collection.md)
  : Insert or update a STAC Collection in the database
- [`stac_db_insert_item()`](https://stevenpawley.github.io/stacserver/reference/stac_db_insert_item.md)
  : Insert or update a STAC Item in the database
- [`stac_db_delete_collection()`](https://stevenpawley.github.io/stacserver/reference/stac_db_delete_collection.md)
  : Delete a STAC Collection and all its items from the database
- [`stac_db_delete_item()`](https://stevenpawley.github.io/stacserver/reference/stac_db_delete_item.md)
  : Delete a STAC Item from the database

## STAC API Server

Serve a STAC API 1.0 compliant HTTP API via `plumber`, backed by the
PostgreSQL database.

- [`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md)
  : Create a plumber router serving a minimal STAC API

## Asset Signing

Rewrite asset hrefs into short-lived signed URLs for private object
storage on Azure Blob Storage. Pass the result to
`stac_api_router(sign_fn = )`.

- [`azure_signer()`](https://stevenpawley.github.io/stacserver/reference/azure_signer.md)
  : Create a reusable Azure Blob Storage signing function
