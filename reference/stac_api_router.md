# Create a plumber router serving a minimal STAC API

Returns a `plumber` router pre-wired with the following endpoints:

## Usage

``` r
stac_api_router(
  con,
  base_url = "http://localhost:8000",
  title = "STAC API",
  description = "A minimal STAC API served by stacserver",
  sign_fn = NULL,
  cors_origins = NULL
)
```

## Arguments

- con:

  A DBI connection, or a
  [`pool::dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md)
  object.

- base_url:

  Base URL of the API (no trailing slash). Used in link hrefs.

- title:

  Human-readable API title.

- description:

  API description.

- sign_fn:

  A function `function(href)` that accepts an unsigned asset href and
  returns a signed href string. When non-`NULL`, asset hrefs in every
  item response are signed before being returned. Pass
  [`azure_signer()`](https://stevenpawley.github.io/stacserver/reference/azure_signer.md)
  to sign Azure Blob Storage hrefs with a managed identity, or supply
  your own function for another backend. Default `NULL` (no signing).
  When enabled, all router responses include
  `Cache-Control: private, no-store` to prevent caching temporary asset
  credentials.

- cors_origins:

  Origins permitted to read responses from browser JavaScript, as a
  character vector of `scheme://host[:port]` values with no path, e.g.
  `"https://browser.example.com"`. `"*"` allows every origin. Default
  `NULL` sends no CORS headers at all. See *Cross-origin requests*.

## Value

A `plumber` router object.

## Details

|  |  |  |
|----|----|----|
| Method | Path | Description |
| GET | `/` | Landing page (root catalog) |
| GET | `/conformance` | Conformance classes |
| GET | `/collections` | List all collections |
| GET | `/collections/{collectionId}` | Single collection |
| GET | `/collections/{collectionId}/items` | Items in a collection |
| GET | `/collections/{collectionId}/items/{itemId}` | Single item |
| GET | `/search` | Search items (GET form) |
| POST | `/search` | Search items (POST / JSON body) |

**Search parameters** (GET query string or POST JSON body):

- `bbox` - comma-separated `west,south,east,north` (GET) or array
  (POST); the six-element form
  `west,south,min_elevation,east,north,max_elevation` is also accepted,
  and a box whose west edge exceeds its east edge is treated as crossing
  the antimeridian

- `datetime` - ISO 8601 value or range `start/end`; use `..` for open
  end

- `collections` - collection ID(s) to filter

- `ids` - item ID(s) to filter

- `limit` - max results per page (default 10, max 10 000)

- `offset` - zero-based page offset (default 0)

- `query` - (POST only) property filters in the form defined by the STAC
  API Query extension, covering any item property including extension
  fields such as `"eo:cloud_cover"`, `"sci:doi"` or
  `"classification:classes"`. A bare value means equality; an object
  selects operators: `{"eo:cloud_cover": {"lt": 10}}`. Supported
  operators are `eq`, `neq`, `lt`, `lte`, `gt`, `gte`, `startsWith`,
  `endsWith`, `contains` and `in`. `properties` is accepted as an alias
  for backwards compatibility.

## Connections

`con` is held for the lifetime of the router. A bare
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
connection will eventually be closed by the server or a connection
pooler, after which every request fails until the process restarts, so
for anything long-running pass a
[`pool::dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md)
object instead — the `.db_*` helpers work with either.

## Access control

The router performs no authentication of its own: every request it
receives is served. Access must be enforced in front of it.

On Posit Connect, set the content's access to "All authenticated users"
or a named group. Connect then validates the caller's API key or session
before the request reaches this process, and callers authenticate to
Connect itself:

    curl -H "Authorization: Key <connect-api-key>" \
         https://connect.example.com/content/<guid>/collections

Setting that content to "Anyone - no login required", or running the
router without an authenticating proxy in front of it, publishes the
whole catalog to anyone who can reach the port.

## Cross-origin requests

`cors_origins` controls the `Access-Control-Allow-Origin` header, which
decides whether JavaScript running on *another* website may read this
API's responses. It is not access control: the request still reaches the
server and is served either way, and non-browser clients — `rstac`,
GDAL, QGIS, Python — ignore the header entirely. It only stops a page
the user happens to be visiting from reading the catalog on their
behalf.

The default of `NULL` sends no CORS headers, which is right for an API
consumed by those clients or by a browser app served from the same
origin. Name an origin only for a browser app hosted elsewhere:

    stac_api_router(con, cors_origins = "https://browser.example.com")

An origin is a scheme, host and optional port with no path, because that
is all a browser sends: a page at `https://example.com/browser` sends
the origin `https://example.com`.

This matters more when `sign_fn` is set, because responses then carry
live signed asset URLs. `"*"` lets any site on the internet read those,
which is only appropriate for a genuinely public catalog. Note also that
a cross-origin browser app cannot authenticate to Posit Connect:
preflight requests carry no credentials, so Connect rejects them before
this router sees them. Serving the browser app from the same origin as
the API avoids the problem entirely.
