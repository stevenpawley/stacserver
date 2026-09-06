# Manual exercises for a running stacserver API, using the rstac client.
#
#   install.packages("rstac")
#
# Start the API first, then work through these interactively. Each block is
# independent, so they can be run in any order.
#
#   library(stacserver); library(DBI)
#   con <- dbConnect(RPostgres::Postgres(), host = "localhost", dbname = "stac",
#                    user = "postgres")
#   plumber::pr_run(stac_api_router(con, base_url = "http://127.0.0.1:3485"),
#                   port = 3485)

library(rstac)

# base_url must match what the router was given, or the links rstac follows
# will point somewhere else
catalog <- stac("http://127.0.0.1:3485")


# ---- 1. Landing page ---------------------------------------------------------
# The root catalog, including the conformance classes the API advertises.

catalog |> get_request()


# ---- 2. Conformance ----------------------------------------------------------

catalog |> conformance() |> get_request()


# ---- 3. Collections ----------------------------------------------------------

catalog |> collections() |> get_request()

# A single collection by id
catalog |> collections("terrain") |> get_request()


# ---- 4. Items in a collection ------------------------------------------------
# Note the get_request(): collections("terrain") |> items() builds a query,
# and get_request() is what performs it.

items <- catalog |> collections("terrain") |> items() |> get_request()
items

items_length(items)   # items in this page
items_matched(items)  # items matching altogether

# Paging: items() returns one page, items_fetch() follows the "next" links
# until everything matched has been collected.
all_items <- catalog |> collections("terrain") |> items(limit = 2) |> get_request() |> items_fetch()
items_length(all_items)


# ---- 5. Read a single item ---------------------------------------------------

item <- catalog |> collections("terrain") |> items("dem-001") |> get_request()
item

item$id
item$collection
item$properties$datetime
item$geometry$type
unlist(item$bbox)
names(item$assets)


# ---- 6. Item accessors -------------------------------------------------------

items_assets(item)    # asset keys
assets_url(item)      # asset hrefs (already signed, if sign_fn is configured)
items_datetime(item)
items_bbox(item)

# Pull one property across every item in a collection
items_reap(all_items, field = c("properties", "eo:cloud_cover"))

# Which property fields exist
items_fields(all_items)


# ---- 7. Search: spatial ------------------------------------------------------
# bbox is c(xmin, ymin, xmax, ymax) in WGS84.

catalog |>
  stac_search(collections = "terrain", bbox = c(-114.25, 50.85, -114.10, 51.00)) |>
  get_request()

# A bbox that matches nothing returns an empty FeatureCollection, not an error
catalog |> stac_search(bbox = c(10, 10, 11, 11)) |> get_request() |> items_length()


# ---- 8. Search: temporal -----------------------------------------------------
# All four STAC interval forms are supported.

catalog |> stac_search(datetime = "2024-06-03T00:00:00Z") |> get_request() |> items_matched()

catalog |>
  stac_search(datetime = "2024-06-02T00:00:00Z/2024-06-04T00:00:00Z") |>
  get_request() |> items_matched()

catalog |> stac_search(datetime = "2024-06-04T00:00:00Z/..") |> get_request() |> items_matched()

catalog |> stac_search(datetime = "../2024-06-02T00:00:00Z") |> get_request() |> items_matched()


# ---- 9. Search: collections and ids ------------------------------------------

catalog |> stac_search(collections = c("terrain", "landcover")) |> get_request() |> items_matched()

catalog |> stac_search(ids = c("dem-001", "dem-002")) |> get_request() |> items_matched()


# ---- 10. Search: POST --------------------------------------------------------
# post_request() sends the same query as a JSON body instead of a query string.

catalog |>
  stac_search(collections = "terrain", limit = 3) |>
  post_request() |>
  items_length()


# ---- 11. Property filters (Query extension) ----------------------------------
# ext_query() needs post_request(); the API advertises
# https://api.stacspec.org/v1.1.0/item-search#query

catalog |> stac_search(collections = "terrain") |>
  ext_query("eo:cloud_cover" < 20) |> post_request() |> items_matched()

catalog |> stac_search(collections = "terrain") |>
  ext_query("eo:cloud_cover" >= 40) |> post_request() |> items_matched()

catalog |> stac_search(collections = "terrain") |>
  ext_query("platform" == "sentinel-2a") |> post_request() |> items_matched()

# Filters combine with AND
catalog |> stac_search(collections = "terrain") |>
  ext_query("eo:cloud_cover" < 40, "platform" == "sentinel-2a") |>
  post_request() |> items_matched()

# Extension fields with a colon in the name work like any other property
catalog |> stac_search() |> ext_query("gsd" == 10) |> post_request() |> items_matched()


# ---- 12. Paging behaviour ----------------------------------------------------
# items_next() follows the "next" link in the response rather than building a
# URL, so this exercises the links the API actually emits. (The API also takes
# an offset query parameter, but rstac's stac_search() does not expose one.)
#
# Pages must not overlap: ordering is by datetime then the primary key, so
# items sharing a timestamp cannot shuffle between pages.

p1 <- catalog |> stac_search(collections = "terrain", limit = 3) |> get_request()
p2 <- items_next(p1)

ids1 <- items_reap(p1, field = "id")
ids2 <- items_reap(p2, field = "id")
ids1
ids2
intersect(ids1, ids2)   # expect character(0)

items_matched(p1)       # total, unchanged by paging


# ---- 13. Convert to sf -------------------------------------------------------
# Requires the sf package.

sf_items <- items_as_sf(all_items)
sf_items
plot(sf::st_geometry(sf_items))


# ---- 14. Error responses -----------------------------------------------------
# These should fail cleanly rather than hang or return nonsense.

try(catalog |> collections("no-such-collection") |> get_request())        # 404
try(catalog |> collections("terrain") |> items("no-such-item") |> get_request())  # 404
try(catalog |> stac_search(limit = 0) |> get_request())                   # 400
