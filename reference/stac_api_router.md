# Create a plumber router serving a minimal STAC API

Returns a `plumber` router pre-wired with the following endpoints:

## Usage

``` r
stac_api_router(
  con,
  base_url = "http://localhost:8000",
  title = "STAC API",
  description = "A minimal STAC API served by stacserver",
  sign_fn = NULL
)
```

## Arguments

- con:

  A DBI connection.

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
  [`sign_azure_ad()`](https://stevenpawley.github.io/stacserver/reference/sign_azure_ad.md)
  to use Azure AD / managed identity, or supply your own function for
  other auth methods (service principal, Planetary Computer signing
  proxy, etc.). Default `NULL` (no signing).

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

- `bbox` — comma-separated `west,south,east,north` (GET) or array (POST)

- `datetime` — ISO 8601 value or range `start/end`; use `..` for open
  end

- `collections` — collection ID(s) to filter

- `ids` — item ID(s) to filter

- `limit` — max results per page (default 10, max 10 000)

- `offset` — zero-based page offset (default 0)

- `properties` — (POST only) JSON object for property equality matching,
  supporting any item property including extension fields such as
  `"eo:cloud_cover"`, `"sci:doi"`, `"classification:classes"`, etc.
