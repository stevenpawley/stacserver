# stacserver

<!-- badges: start -->
[![R-CMD-check](https://github.com/stevenpawley/stacserver/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stevenpawley/stacserver/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**stacserver** serves [STAC (SpatioTemporal Asset Catalog)](https://stacspec.org/)
metadata as a [STAC API 1.0](https://github.com/radiantearth/stac-api-spec)
compliant HTTP service. Catalogs built with
[stacbuildr](https://github.com/stevenpawley/stacbuildr) are ingested into a
PostgreSQL/PostGIS database and exposed through a `plumber` router.

The split is deliberate: **stacbuildr builds and writes static catalogs**,
**stacserver serves them dynamically**. If you only need STAC JSON on disk or in
object storage, you do not need this package.

*Note* this package is in active development: breaking changes are expected and
there is no guarantee of compliance with the STAC API spec.

## Installation

```r
# install.packages("remotes")
remotes::install_github("stevenpawley/stacserver")
```

## Endpoints

| Endpoint | Purpose |
|----------|---------|
| `/` | Landing page with conformance links |
| `/conformance` | Conformance class declarations |
| `/collections` | List all collections |
| `/collections/{id}` | A single collection |
| `/collections/{id}/items` | Items in a collection (`bbox`, `datetime`, `limit`) |
| `/collections/{id}/items/{itemId}` | A single item |
| `/search` | Item search, `GET` and `POST` |

## Asset signing

When assets live in private object storage, pass a `sign_fn` to
`stac_api_router()` and the router rewrites every asset href it returns into a
short-lived signed URL:

```r
router <- stac_api_router(
  con,
  base_url = "https://stac.example.com",
  sign_fn = function(href) sign_aws_s3(href, expiry_seconds = 3600)
)
```

`sign_azure_ad()` (Azure Blob Storage), `sign_gcp()` (Google Cloud Storage) and
`sign_aws_s3()` (Amazon S3) are provided. Each needs its own backend package
(`AzureStor` + `AzureAuth`, `googleCloudStorageR`, `paws.storage`), which are
Suggests rather than hard dependencies.

stacserver serves a live [STAC API](https://github.com/radiantearth/stac-api-spec)
backed by a PostgreSQL database (with PostGIS). The API follows the OGC API –
Features and STAC API 1.0 specifications.

## Prerequisites

```r
install.packages(c("DBI", "RPostgres", "plumber"))
```

A PostgreSQL database with the PostGIS extension must be reachable.

## Set up the database

```r
library(stacserver)
library(stacbuildr)
library(DBI)

con <- dbConnect(
  RPostgres::Postgres(),
  host     = "localhost",
  dbname   = "stac",
  user     = "myuser",
  password = "mypassword"
)

# Create tables and indexes (idempotent — safe to run on every startup)
stac_db_setup(con)
```

## Ingest collections and items

`collection` and `item` below are objects built with
[stacbuildr](https://github.com/stevenpawley/stacbuildr).

```r
# Insert a collection
stac_db_insert_collection(con, collection)

# Items must reference their collection before ingestion
item@collection <- "sentinel-2-l2a"
stac_db_insert_item(con, item)

# Items with extension metadata are stored as-is in JSONB —
# no schema changes are needed for new extensions
item_with_extensions <- item |>
  add_eo_extension(bands = sentinel2_msi_bands(), cloud_cover = 4.1) |>
  add_scientific_extension(doi = "10.1000/xyz123")

item_with_extensions@collection <- "sentinel-2-l2a"
stac_db_insert_item(con, item_with_extensions)
```

## Launch the API

```r
router <- stac_api_router(
  con,
  base_url    = "http://localhost:8000",
  title       = "My STAC API",
  description = "Sentinel-2 imagery archive"
)

plumber::pr_run(router, port = 8000)
```

The router exposes these endpoints:

| Method | Path | Description |
|--------|------|-------------|
| GET | `/` | Landing page |
| GET | `/conformance` | Conformance classes |
| GET | `/collections` | All collections |
| GET | `/collections/{collectionId}` | Single collection |
| GET | `/collections/{collectionId}/items` | Paged items |
| GET | `/collections/{collectionId}/items/{itemId}` | Single item |
| GET | `/search` | Cross-collection search |
| POST | `/search` | Search with JSON body |

**Search parameters:** `bbox`, `datetime`, `collections`, `ids`, `limit`, `offset`.
The POST `/search` endpoint additionally accepts a `properties` object for
filtering on any item property, including extension fields:

```json
{
  "bbox": [-106, 39, -104, 41],
  "datetime": "2023-01-01T00:00:00Z/2023-12-31T23:59:59Z",
  "collections": ["sentinel-2-l2a"],
  "limit": 20,
  "properties": {
    "eo:cloud_cover": 4.1,
    "sci:doi": "10.1000/xyz123"
  }
}
```

## Authentication

The API requires an `Authorization: Key <api-key>` header on every request
(the same convention used by Posit Connect for programmatic API access). Key
validation is resolved in this order:

1. **`CONNECT_SERVER` env var set** — the key is validated live against
   Posit Connect's user API. This env var is always set automatically when the
   content is deployed on Connect.
2. **`STAC_API_KEY` env var set** — the key is compared to that static value.
   Useful for local development.
3. **Neither set** — any correctly-formatted header is accepted. Use only when
   Connect is enforcing authentication at the infrastructure level.

To disable authentication during development:

```r
router <- stac_api_router(con, require_auth = FALSE)
```

## Deploying to Posit Connect

The API can be deployed to [Posit Connect](https://posit.co/products/enterprise/connect/)
using the standard plumber deployment workflow. Create an entrypoint file
(e.g. `plumber.R`) in your project:

```r
# plumber.R
library(stacserver)
library(stacbuildr)
library(DBI)

con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST"),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

stac_db_setup(con)

stac_api_router(
  con,
  base_url = Sys.getenv("CONNECT_CONTENT_URL")
)
```

Then publish and set database credentials as environment variables in the
Connect dashboard. In the content's **Access** settings, set access to
**"All authenticated Posit Connect users"** (or a specific group) — Connect
will then validate API keys before requests reach the plumber process, and the
`CONNECT_SERVER` variable will be injected automatically.

Callers authenticate using their personal Connect API key:

```bash
curl -H "Authorization: Key <connect-api-key>" \
     https://connect.example.com/content/<id>/collections
```

## Dependencies

| Package | Role |
|---------|------|
| `stacbuildr` | STAC object classes and serialisation |
| `DBI` | Database interface |
| `plumber` | HTTP routing |
| `sf`, `geojsonsf` | Geometry handling and GeoJSON conversion |
| `jsonlite` | JSON serialisation |

Optional: `RPostgres` (PostgreSQL driver), `pool` (connection pooling),
`AzureStor` + `AzureAuth` / `googleCloudStorageR` / `paws.storage` (asset signing)

## References

- [STAC API spec](https://github.com/radiantearth/stac-api-spec)
- [STAC Specification](https://stacspec.org/)
- [stacbuildr](https://github.com/stevenpawley/stacbuildr)
