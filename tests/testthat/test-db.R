# Database-backed tests. Skipped unless STACSERVER_TEST_PG is set; see
# helper-db.R.

test_that("a collection round-trips through the database", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)

  stac_db_insert_collection(con, test_collection(cid))
  stored <- .db_get_collection(con, cid)

  expect_equal(stored$id, cid)
  expect_equal(stored$description, "Fixture collection")
  # Arrays survive as arrays rather than collapsing to scalars
  expect_type(stored$extent$spatial$bbox, "list")
})

test_that("inserting a collection twice updates rather than duplicating", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)

  stac_db_insert_collection(con, test_collection(cid))
  col <- test_collection(cid)
  col@description <- "Changed"
  stac_db_insert_collection(con, col)

  expect_equal(.db_get_collection(con, cid)$description, "Changed")
  n <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM stac_collections WHERE id = $1",
    params = list(cid)
  )
  expect_equal(as.integer(n$n[[1]]), 1L)
})

test_that("item coordinates are stored at full precision", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))

  lon <- -114.123456789
  lat <- 51.987654321
  stac_db_insert_item(
    con,
    test_item("precise", cid, lon, lat, "2024-06-01T00:00:00Z")
  )

  stored <- .db_get_item(con, cid, "precise")
  # jsonlite's default of 4 decimal places would round these to ~11 m
  expect_equal(stored$geometry$coordinates[[1]], lon)
  expect_equal(stored$geometry$coordinates[[2]], lat)
})

test_that("deleting a collection cascades to its items", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  stac_db_insert_item(con, test_item("a", cid, -114, 51, "2024-06-01T00:00:00Z"))

  stac_db_delete_collection(con, cid)

  expect_null(.db_get_collection(con, cid))
  expect_null(.db_get_item(con, cid, "a"))
})

test_that("deleting an item leaves the rest of the collection alone", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  stac_db_insert_item(con, test_item("a", cid, -114, 51, "2024-06-01T00:00:00Z"))
  stac_db_insert_item(con, test_item("b", cid, -114, 51, "2024-06-02T00:00:00Z"))

  stac_db_delete_item(con, "a", cid)

  expect_null(.db_get_item(con, cid, "a"))
  expect_false(is.null(.db_get_item(con, cid, "b")))
})

test_that("bbox search finds only intersecting items", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  stac_db_insert_item(con, test_item("inside", cid, -114.0, 51.0, "2024-06-01T00:00:00Z"))
  stac_db_insert_item(con, test_item("outside", cid, 10.0, 10.0, "2024-06-01T00:00:00Z"))

  res <- .db_search_items(
    con,
    bbox = c(-114.5, 50.5, -113.5, 51.5),
    collections = cid
  )
  expect_equal(res$matched, 1L)
  expect_equal(res$items[[1]]$id, "inside")
})

test_that("a six-element bbox searches on its horizontal corners", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  stac_db_insert_item(con, test_item("inside", cid, -114.0, 51.0, "2024-06-01T00:00:00Z"))

  res <- .db_search_items(
    con,
    bbox = c(-114.5, 50.5, 0, -113.5, 51.5, 5000),
    collections = cid
  )
  expect_equal(res$matched, 1L)
})

test_that("a bbox crossing the antimeridian matches both sides", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  stac_db_insert_item(con, test_item("east", cid, 179.0, 0, "2024-06-01T00:00:00Z"))
  stac_db_insert_item(con, test_item("west", cid, -179.0, 0, "2024-06-01T00:00:00Z"))
  stac_db_insert_item(con, test_item("far", cid, 0.0, 0, "2024-06-01T00:00:00Z"))

  res <- .db_search_items(con, bbox = c(170, -10, -170, 10), collections = cid)
  expect_equal(res$matched, 2L)
  expect_setequal(
    vapply(res$items, function(i) i$id, character(1)),
    c("east", "west")
  )
})

test_that("datetime search covers the four interval forms", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  stac_db_insert_item(con, test_item("y2023", cid, -114, 51, "2023-06-01T00:00:00Z"))
  stac_db_insert_item(con, test_item("y2024", cid, -114, 51, "2024-06-01T00:00:00Z"))
  stac_db_insert_item(con, test_item("y2025", cid, -114, 51, "2025-06-01T00:00:00Z"))

  search <- function(datetime) {
    dt <- .parse_datetime_param(datetime)
    .db_search_items(
      con,
      dt_start = dt$start,
      dt_end = dt$end,
      single_dt = dt$single_dt,
      collections = cid
    )$matched
  }

  expect_equal(search("2024-06-01T00:00:00Z"), 1L)
  expect_equal(search("2024-01-01T00:00:00Z/2025-01-01T00:00:00Z"), 1L)
  expect_equal(search("2024-01-01T00:00:00Z/.."), 2L)
  expect_equal(search("../2024-01-01T00:00:00Z"), 1L)
})

test_that("paging is stable when every item shares a datetime", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  for (i in 1:10) {
    stac_db_insert_item(
      con,
      test_item(sprintf("item-%02d", i), cid, -114, 51, "2024-06-01T00:00:00Z")
    )
  }

  ids <- function(offset) {
    res <- .db_search_items(con, collections = cid, limit = 5L, offset = offset)
    vapply(res$items, function(i) i$id, character(1))
  }

  page1 <- ids(0L)
  page2 <- ids(5L)

  # Ordering by datetime alone leaves ties free to move between pages, which
  # makes OFFSET paging drop and repeat items
  expect_length(intersect(page1, page2), 0L)
  expect_setequal(c(page1, page2), sprintf("item-%02d", 1:10))
  # And the same page twice gives the same answer
  expect_equal(ids(0L), page1)
})

test_that("matched counts the whole result set, not the page", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  for (i in 1:7) {
    stac_db_insert_item(
      con,
      test_item(sprintf("item-%d", i), cid, -114, 51, "2024-06-01T00:00:00Z")
    )
  }

  res <- .db_search_items(con, collections = cid, limit = 3L)
  expect_equal(res$matched, 7L)
  expect_length(res$items, 3L)

  # An empty page past the end still reports the true total
  past_end <- .db_search_items(con, collections = cid, limit = 3L, offset = 99L)
  expect_equal(past_end$matched, 7L)
  expect_length(past_end$items, 0L)

  # As does a query that matches nothing at all
  none <- .db_search_items(con, collections = "no-such-collection")
  expect_equal(none$matched, 0L)
  expect_length(none$items, 0L)
})

test_that("query filters run against real JSONB properties", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))

  clear <- stacbuildr::stac_item(
    id = "clear",
    geometry = list(type = "Point", coordinates = c(-114, 51)),
    bbox = c(-114, 51, -114, 51),
    datetime = "2024-06-01T00:00:00Z",
    properties = list("eo:cloud_cover" = 2.5, platform = "sentinel-2a")
  )
  clear@collection <- cid
  cloudy <- stacbuildr::stac_item(
    id = "cloudy",
    geometry = list(type = "Point", coordinates = c(-114, 51)),
    bbox = c(-114, 51, -114, 51),
    datetime = "2024-06-02T00:00:00Z",
    properties = list("eo:cloud_cover" = 80, platform = "sentinel-2b")
  )
  cloudy@collection <- cid
  stac_db_insert_item(con, clear)
  stac_db_insert_item(con, cloudy)

  matched <- function(query) {
    .db_search_items(con, collections = cid, query = query)$matched
  }

  expect_equal(matched(list("eo:cloud_cover" = 2.5)), 1L)
  expect_equal(matched(list("eo:cloud_cover" = list(lt = 10))), 1L)
  expect_equal(matched(list("eo:cloud_cover" = list(gte = 2.5))), 2L)
  expect_equal(matched(list("eo:cloud_cover" = list(neq = 80))), 1L)
  expect_equal(matched(list(platform = list(startsWith = "sentinel-2"))), 2L)
  expect_equal(matched(list(platform = list(endsWith = "2b"))), 1L)
  expect_equal(matched(list(platform = list(contains = "-2a"))), 1L)
  expect_equal(matched(list(platform = list("in" = list("sentinel-2a", "x")))), 1L)
  # Filters combine with AND
  expect_equal(
    matched(list("eo:cloud_cover" = list(lt = 10), platform = "sentinel-2a")),
    1L
  )
})

test_that("a numeric comparison tolerates a property of mixed type", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))

  numeric_item <- stacbuildr::stac_item(
    id = "numeric",
    geometry = list(type = "Point", coordinates = c(-114, 51)),
    bbox = c(-114, 51, -114, 51),
    datetime = "2024-06-01T00:00:00Z",
    properties = list(gsd = 10)
  )
  numeric_item@collection <- cid
  string_item <- stacbuildr::stac_item(
    id = "string",
    geometry = list(type = "Point", coordinates = c(-114, 51)),
    bbox = c(-114, 51, -114, 51),
    datetime = "2024-06-02T00:00:00Z",
    properties = list(gsd = "unknown")
  )
  string_item@collection <- cid
  stac_db_insert_item(con, numeric_item)
  stac_db_insert_item(con, string_item)

  # Without the jsonb_typeof guard the ::numeric cast would error on "unknown"
  res <- .db_search_items(con, collections = cid, query = list(gsd = list(lt = 20)))
  expect_equal(res$matched, 1L)
  expect_equal(res$items[[1]]$id, "numeric")
})

test_that("stac_db_setup can be run repeatedly", {
  skip_if_no_pg()
  con <- test_con()
  expect_no_error(stac_db_setup(con))
  expect_no_error(stac_db_setup(con))
})
