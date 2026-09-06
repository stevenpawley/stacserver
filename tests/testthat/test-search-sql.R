# SQL construction for spatial and property filters. These build strings and
# parameter lists only; test-db.R exercises them against a real database.

test_that(".bbox_sql_clause binds one envelope for an ordinary box", {
  cl <- .bbox_sql_clause(c(-114.1, 51, -114, 51.1), 1L)
  expect_equal(cl$params, list(-114.1, 51, -114, 51.1))
  expect_match(cl$sql, "ST_MakeEnvelope\\(\\$1, \\$2, \\$3, \\$4, 4326\\)")
  # One envelope, so one ST_Intersects
  expect_equal(lengths(regmatches(cl$sql, gregexpr("ST_Intersects", cl$sql))), 1L)
})

test_that(".bbox_sql_clause splits a box that crosses the antimeridian", {
  # west (170) is east of east (-170): the box spans the 180th meridian
  cl <- .bbox_sql_clause(c(170, -10, -170, 10), 1L)
  expect_equal(cl$params, list(170, -10, 180, 10, -180, -10, -170, 10))
  expect_equal(lengths(regmatches(cl$sql, gregexpr("ST_Intersects", cl$sql))), 2L)
  expect_match(cl$sql, "OR")
})

test_that(".bbox_sql_clause ignores the elevations of a 3D bbox", {
  flat <- .bbox_sql_clause(c(-1, -2, 3, 4), 1L)
  cube <- .bbox_sql_clause(c(-1, -2, 100, 3, 4, 200), 1L)
  expect_equal(flat, cube)
})

test_that(".bbox_sql_clause numbers placeholders from the offset given", {
  cl <- .bbox_sql_clause(c(-1, -2, 3, 4), 7L)
  expect_match(cl$sql, "\\$7, \\$8, \\$9, \\$10")
})

test_that(".intersects_sql_clause stamps the SRID on the parsed geometry", {
  cl <- .intersects_sql_clause(
    list(type = "Point", coordinates = list(-114, 51)),
    1L
  )
  # Without ST_SetSRID, GeoJSON carrying no CRS member reaches the 4326 column
  # with no SRID of its own, which errors rather than matching nothing
  expect_match(cl$sql, "ST_SetSRID\\(ST_GeomFromGeoJSON\\(\\$1::text\\), 4326\\)")
  expect_equal(cl$params, list('{"type":"Point","coordinates":[-114,51]}'))
})

test_that(".intersects_sql_clause numbers its placeholder from the offset given", {
  cl <- .intersects_sql_clause(
    list(type = "Point", coordinates = list(0, 0)),
    5L
  )
  expect_match(cl$sql, "\\$5::text", fixed = FALSE)
})

test_that(".geojson_text writes both parsed shapes back to the same JSON", {
  # A GET query string goes through .parse_json() and stays nested lists; a
  # POST body is simplified by plumber, so a polygon's ring arrives as an array
  nested <- .parse_json(
    '{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,0]]]}'
  )
  simplified <- jsonlite::fromJSON(
    '{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,0]]]}',
    simplifyVector = TRUE
  )
  expect_equal(.geojson_text(nested), .geojson_text(simplified))
  expect_equal(
    .geojson_text(nested),
    '{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,0]]]}'
  )
})

test_that(".geojson_text keeps full coordinate precision", {
  txt <- .geojson_text(
    list(type = "Point", coordinates = list(-114.123456789, 51.987654321))
  )
  expect_match(txt, "-114.123456789", fixed = TRUE)
  expect_match(txt, "51.987654321", fixed = TRUE)
})

test_that("a bare query value means equality via JSONB containment", {
  cl <- .query_sql_clause(list("eo:cloud_cover" = 4.1), 1L)
  expect_length(cl$sql, 1L)
  expect_match(cl$sql, "content->'properties' @> \\$1::jsonb")
  expect_equal(cl$params, list('{"eo:cloud_cover":4.1}'))
})

test_that("query equality keeps full numeric precision", {
  cl <- .query_sql_clause(list("x" = 1.123456789), 1L)
  expect_equal(cl$params, list('{"x":1.123456789}'))
})

test_that("neq negates the containment test", {
  cl <- .query_sql_clause(list("gsd" = list(neq = 10)), 1L)
  expect_match(cl$sql, "^NOT \\(")
  expect_equal(cl$params, list('{"gsd":10}'))
})

test_that("numeric comparisons guard the cast with a type test", {
  cl <- .query_sql_clause(list("eo:cloud_cover" = list(lt = 10)), 1L)
  # Without the jsonb_typeof guard the cast would fail on an item storing a
  # string in the same property
  expect_match(cl$sql, "jsonb_typeof\\(content->'properties'->\\$1::text\\) = 'number'")
  expect_match(cl$sql, "::numeric < \\$2::numeric")
  expect_equal(cl$params, list("eo:cloud_cover", 10))
})

test_that("string comparisons fall back to text ordering", {
  cl <- .query_sql_clause(list("datetime" = list(gte = "2020-01-01")), 1L)
  expect_match(cl$sql, ">= \\$2::text")
  expect_equal(cl$params, list("datetime", "2020-01-01"))
})

test_that("string operators avoid LIKE so wildcards need no escaping", {
  starts <- .query_sql_clause(list("id" = list(startsWith = "100%_x")), 1L)
  expect_match(starts$sql, "starts_with")
  expect_false(grepl("LIKE", starts$sql))
  expect_equal(starts$params, list("id", "100%_x"))

  ends <- .query_sql_clause(list("id" = list(endsWith = "_v2")), 1L)
  expect_match(ends$sql, "right\\(")
  expect_false(grepl("LIKE", ends$sql))

  has <- .query_sql_clause(list("id" = list(contains = "50%")), 1L)
  expect_match(has$sql, "position\\(")
  expect_false(grepl("LIKE", has$sql))
})

test_that("the in operator expands to one placeholder per value", {
  cl <- .query_sql_clause(list("platform" = list("in" = list("a", "b", "c"))), 1L)
  expect_match(cl$sql, "IN \\(\\$2::text, \\$3::text, \\$4::text\\)")
  expect_equal(cl$params, list("platform", "a", "b", "c"))
})

test_that("an empty in set matches nothing rather than everything", {
  cl <- .query_sql_clause(list("platform" = list("in" = list())), 1L)
  expect_equal(cl$sql, "FALSE")
  expect_equal(cl$params, list())
})

test_that("placeholder numbering stays consistent across several filters", {
  cl <- .query_sql_clause(
    list(
      "sci:doi" = "10.1000/xyz",
      "eo:cloud_cover" = list(lt = 10)
    ),
    3L
  )
  expect_length(cl$sql, 2L)
  expect_length(cl$params, 3L)
  expect_match(cl$sql[1], "\\$3::jsonb")
  # Second filter continues from $4 (key) and $5 (value)
  expect_match(cl$sql[2], "\\$4::text")
  expect_match(cl$sql[2], "\\$5::numeric")
})

test_that("an unknown operator is rejected rather than ignored", {
  expect_error(
    .query_sql_clause(list("x" = list(matches = "y")), 1L),
    "Unsupported query operator"
  )
})
