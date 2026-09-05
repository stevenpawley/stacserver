# Serving a STAC API

stacserver turns the catalog objects built by
[stacbuildr](https://github.com/stevenpawley/stacbuildr) into a live
[STAC API](https://github.com/radiantearth/stac-api-spec). Collections
and Items are stored in PostgreSQL/PostGIS as JSONB alongside indexed
geometry and datetime columns, and
[`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md)
serves them through a `plumber` router implementing the OGC API -
Features and STAC API 1.0 conformance classes.

Every chunk below requires a running PostgreSQL database, so none of
them are evaluated when the vignette is built. They are shown for
reference.

## What you need

``` r

install.packages(c("DBI", "RPostgres", "plumber"))
```

A PostgreSQL database with the PostGIS extension must be reachable.

## Build something to serve

Any stacbuildr Collection and Item will do. This vignette assumes you
have a `collection` and an `item` in hand:

``` r

library(stacbuildr)
library(stacserver)

collection <- stac_collection(
  id = "terrain",
  description = "Elevation data",
  license = "CC-BY-4.0",
  extent = stac_extent(
    spatial_bbox = list(c(-114.1, 51.0, -114.0, 51.1)),
    temporal_interval = list(list("2024-01-01T00:00:00Z", NULL))
  )
)

item <- stac_item(
  id = "dem-001",
  geometry = list(
    type = "Polygon",
    coordinates = list(list(
      c(-114.1, 51.0), c(-114.0, 51.0),
      c(-114.0, 51.1), c(-114.1, 51.1), c(-114.1, 51.0)
    ))
  ),
  bbox = c(-114.1, 51.0, -114.0, 51.1),
  datetime = "2024-06-01T00:00:00Z"
)
```

## Set up the database

[`stac_db_setup()`](https://stevenpawley.github.io/stacserver/reference/stac_db_setup.md)
creates tables and indexes inside an existing database; it cannot create
the database itself, because PostgreSQL only accepts `CREATE DATABASE`
from a connection to a *different* database. Create it once up front,
either from the shell:

``` sh
createdb stac
```

or from R, by connecting to the default `postgres` maintenance database:

``` r

library(DBI)

admin <- dbConnect(
  RPostgres::Postgres(),
  host = "localhost",
  port = 5432,
  dbname = "postgres",
  user = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASSWORD")
)

if (nrow(dbGetQuery(
  admin,
  "SELECT 1 FROM pg_database WHERE datname = 'stac'"
)) == 0) {
  dbExecute(admin, "CREATE DATABASE stac")
}

dbDisconnect(admin)
```

The role also needs permission to run `CREATE EXTENSION postgis`, which
[`stac_db_setup()`](https://stevenpawley.github.io/stacserver/reference/stac_db_setup.md)
issues on first use. A superuser connection, or a database created from
the `template_postgis` template, satisfies this.

Now connect to the `stac` database and create the schema:

``` r

con <- dbConnect(
  RPostgres::Postgres(),
  host = "localhost",
  port = 5432,
  dbname = "stac",
  user = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASSWORD")
)

stac_db_setup(con)
```

## Serve the assets over HTTP

An item written to a static catalog can carry a local filesystem path as
its asset `href`:
[`write_stac()`](https://stevenpawley.github.io/stacbuildr/reference/write_stac.html)
rewrites it relative to the item JSON and a local reader resolves it on
disk. That does **not** work for the API. A client such as QGIS resolves
a non-URL href against the API base URL, producing a nonsense address
like `http://127.0.0.1:3485/var/folders/.../dem.tif`, and the request
404s.

The STAC API serves JSON only - it never serves asset bytes. So assets
must already be fetchable over HTTP from somewhere else. For local
development, run a static file server rooted at the directory holding
the assets, leaving it running in a terminal:

``` sh
npx http-server "<asset directory>" -p 8000 --cors
```

`npx serve "<asset directory>" -l 8000 --cors` works equally well.

Three details are easy to get wrong here:

- **Serve the asset directory itself, not `$TMPDIR`.** R creates a
  per-session `Rtmp<XXXXXX>` directory beneath `$TMPDIR`, so a server
  rooted at `$TMPDIR` leaves the raster one path segment deeper than the
  href says, and every request 404s.
- **Use a server that honours `Range`.** GDAL reads remote rasters
  through `/vsicurl/`, which issues HTTP range requests to fetch only
  the tiles and overviews it needs. Python’s built-in `http.server`
  ignores `Range` - it answers `200` with the full body and advertises
  no `Accept-Ranges` - forcing the whole file over the wire on every
  read, which is ruinous for a cloud-optimized GeoTIFF. Both `npx`
  servers above return `206 Partial Content` correctly.
- **Keep the R session alive** if assets live in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). R deletes it on
  exit, taking the raster with it and leaving the stored hrefs pointing
  at nothing. For anything beyond a walkthrough, use a persistent
  directory.

`--cors` is what lets browser-based clients such as STAC Browser read
the assets; QGIS does not need it, but it costs nothing.

Point the asset at that server:

``` r

item <- add_asset(
  item,
  key = "dem",
  href = "http://127.0.0.1:8000/dem.tif",
  title = "Digital Elevation Model",
  type = "image/tiff; application=geotiff; profile=cloud-optimized",
  roles = "data"
)
```

Confirm the URL resolves before inserting - `curl -I <href>` should
return `200 OK`. In production, use blob storage URLs (S3, Azure Blob,
GCS) so clients fetch assets directly without going through the API or a
local file server.

## Insert a collection and item

Insert *after* fixing the href:
[`stac_db_insert_item()`](https://stevenpawley.github.io/stacserver/reference/stac_db_insert_item.md)
stores the item as-is, so an href inserted wrong stays wrong until you
re-insert. Both inserts upsert on `id`, so they are safe to re-run.

``` r

stac_db_insert_collection(con, collection)

item@collection <- "terrain"
stac_db_insert_item(con, item)
```

Items with extension metadata are stored as-is in JSONB, so no schema
changes are needed to support new STAC extensions.

## Run the STAC API locally

[`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md)
returns a plumber router pre-wired with all STAC endpoints.

``` r

library(plumber)

pr <- stac_api_router(
  con,
  base_url = "http://127.0.0.1:3485"
)

pr_run(pr, port = 3485)
```

The router exposes the STAC API core, item search and collections
endpoints:

| Endpoint | Purpose |
|----|----|
| `/` | Landing page with conformance links |
| `/conformance` | Conformance class declarations |
| `/collections` | List all collections |
| `/collections/{id}` | A single collection |
| `/collections/{id}/items` | Items in a collection, with `bbox`/`datetime`/`limit` |
| `/collections/{id}/items/{itemId}` | A single item |
| `/search` | Item search, `GET` and `POST` |

## Signing assets in private storage

When assets live in private object storage, pass a `sign_fn` to
[`stac_api_router()`](https://stevenpawley.github.io/stacserver/reference/stac_api_router.md).
The router calls it for every asset href it returns, so clients receive
short-lived signed URLs instead of unreachable ones.

``` r

pr <- stac_api_router(
  con,
  base_url = "https://stac.example.com",
  sign_fn = function(href) sign_aws_s3(href, expiry_seconds = 3600)
)
```

[`sign_azure_ad()`](https://stevenpawley.github.io/stacserver/reference/sign_azure_ad.md),
[`sign_gcp()`](https://stevenpawley.github.io/stacserver/reference/sign_gcp.md)
and
[`sign_aws_s3()`](https://stevenpawley.github.io/stacserver/reference/sign_aws_s3.md)
cover Azure Blob Storage, Google Cloud Storage and Amazon S3
respectively. Each requires its own backend package
(`AzureStor`/`AzureAuth`, `googleCloudStorageR`, `paws.storage`), which
are Suggests rather than hard dependencies.

## Deploying to Posit Connect

When deploying with multiple concurrent users, replace the single DBI
connection with a connection pool from the `pool` package. A pool
manages multiple connections and hands them out to simultaneous requests
without contention. Store credentials in environment variables rather
than hardcoding them - Posit Connect lets you set these per-deployment
under the Vars tab.

``` r

library(pool)
library(plumber)

pool <- dbPool(
  RPostgres::Postgres(),
  host = Sys.getenv("PG_HOST"),
  dbname = Sys.getenv("PG_DBNAME"),
  user = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASSWORD")
)

onStop(function() poolClose(pool))

pr <- stac_api_router(
  pool,
  base_url = Sys.getenv("STAC_BASE_URL") # e.g. "https://connect.example.com/stac"
)

pr_run(pr)
```
