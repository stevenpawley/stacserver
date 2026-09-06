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
short-lived signed URL. `sign_fn` is any `function(href)` returning a signed
href, so a backend this package does not cover can be supplied directly.

Azure Blob Storage is covered out of the box by `azure_signer()`. Minting a
user delegation key is a network round trip, and the router signs every asset
href in every response, so an items page holding ten items with four assets
apiece would otherwise make forty calls to Azure before it could reply.
`azure_signer()` fetches one key and reuses it, renewing only when the key gets
close enough to expiry that it could no longer cover a new signature:

```r
router <- stac_api_router(
  con,
  base_url = "https://stac.example.com",
  sign_fn  = azure_signer(expiry_seconds = 3600)  # AZURE_STORAGE_ENDPOINT
)
```

It also validates its configuration when built rather than once per asset at
request time, so a missing endpoint fails at startup.

To sign a single href, call the signer directly: `azure_signer()(href)`.
`azure_signer()` needs `AzureStor` and `AzureAuth`, which are Suggests rather
than hard dependencies.

stacserver serves a live [STAC API](https://github.com/radiantearth/stac-api-spec)
backed by a PostgreSQL database (with PostGIS). The API follows the OGC API –
Features and STAC API 1.0 specifications.

## Prerequisites

```r
install.packages(c("DBI", "RPostgres", "plumber", "pool"))
```

A PostgreSQL database with the PostGIS extension must be reachable.
`stac_db_setup()` will create the PostGIS extension if the connecting role is
allowed to; on a managed database where PostGIS is already installed it carries
on regardless.

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

### Use a pool for a long-running server

A single `DBI` connection held for the lifetime of a server will eventually be
closed by PostgreSQL or a connection pooler, and every request afterwards fails
until the process restarts. For anything long-running, hand `stac_api_router()`
a pool instead — every function in this package accepts either:

```r
con <- pool::dbPool(
  RPostgres::Postgres(),
  host   = "localhost",
  dbname = "stac",
  user   = "myuser",
  password = "mypassword"
)
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

`bbox` takes the four-element `west,south,east,north` form or the six-element
`west,south,min_elevation,east,north,max_elevation` form, and a box whose west
edge is east of its east edge is treated as crossing the antimeridian.

The POST `/search` endpoint additionally accepts a `query` object implementing
the [STAC API Query extension](https://github.com/stac-api-extensions/query),
which filters on any item property including extension fields. A bare value
means equality; an object selects operators (`eq`, `neq`, `lt`, `lte`, `gt`,
`gte`, `startsWith`, `endsWith`, `contains`, `in`):

```json
{
  "bbox": [-106, 39, -104, 41],
  "datetime": "2023-01-01T00:00:00Z/2023-12-31T23:59:59Z",
  "collections": ["sentinel-2-l2a"],
  "limit": 20,
  "query": {
    "eo:cloud_cover": { "lt": 10 },
    "sci:doi": "10.1000/xyz123"
  }
}
```

## Access control

**The router performs no authentication of its own.** Every request it receives
is served, so access has to be enforced in front of it. Running it on an open
port publishes the whole catalog to anyone who can reach that port.

On Posit Connect, set the content's access to **"All authenticated users"** (or
a specific group) under the content's Access settings. Connect then validates
the caller's API key or session before the request reaches the plumber process.
Callers authenticate to Connect itself:

```bash
curl -H "Authorization: Key <connect-api-key>" \
     https://connect.example.com/content/<guid>/collections
```

Elsewhere, put the API behind a reverse proxy, API gateway, or similar that
authenticates requests before they arrive.

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
  # The URL clients use to reach this content, with no trailing slash
  base_url = "https://connect.example.com/stac"
)
```

`base_url` has to be set by hand. Connect serves content under a path prefix
and the plumber process only sees the path below it, so it cannot work out its
own public address. Every link in every response is built from this value, and
STAC clients navigate by following those links — get it wrong and paging and
item navigation break even though each endpoint answers correctly on its own.

Use whichever URL clients actually type: the vanity URL if the content has one
(set under **Content URL** in the content settings), otherwise
`https://connect.example.com/content/<guid>/`, dropping the trailing slash.
After the first deploy, request the landing page and check that the `self` link
matches the URL you used to reach it.

Then publish and set database credentials as environment variables in the
Connect dashboard.

In the content's **Access** settings, set access to **"All authenticated Posit
Connect users"** (or a specific group). This is what secures the API: Connect
validates each caller's key or session before the request reaches the plumber
process. Leaving the content readable by anyone publishes the entire catalog.

Callers authenticate using their personal Connect API key:

```bash
curl -H "Authorization: Key <connect-api-key>" \
     https://connect.example.com/stac/collections
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
`AzureStor` + `AzureAuth` (Azure asset signing)

## Testing

Most of the test suite runs without a database. The database-backed tests are
skipped unless `STACSERVER_TEST_PG` points at a PostgreSQL/PostGIS database the
tests may write to:

```bash
docker run -d --name stac-test -p 5432:5432 \
  -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=stac_test postgis/postgis:16-3.4

STACSERVER_TEST_PG=postgresql://postgres:postgres@localhost:5432/stac_test \
  Rscript -e 'devtools::test()'
```

Everything those tests create is namespaced by a random collection id and
removed afterwards.

## References

- [STAC API spec](https://github.com/radiantearth/stac-api-spec)
- [STAC Specification](https://stacspec.org/)
- [stacbuildr](https://github.com/stevenpawley/stacbuildr)
