# Query-parameter parsing. These run without a database.

test_that(".parse_datetime_param handles the four STAC interval forms", {
  expect_equal(
    .parse_datetime_param("2020-01-01T00:00:00Z"),
    list(
      start = "2020-01-01T00:00:00Z",
      end = "2020-01-01T00:00:00Z",
      single_dt = TRUE
    )
  )

  expect_equal(
    .parse_datetime_param("2020-01-01T00:00:00Z/2021-01-01T00:00:00Z"),
    list(
      start = "2020-01-01T00:00:00Z",
      end = "2021-01-01T00:00:00Z",
      single_dt = FALSE
    )
  )

  # Open start
  expect_equal(
    .parse_datetime_param("../2021-01-01T00:00:00Z"),
    list(start = NULL, end = "2021-01-01T00:00:00Z", single_dt = FALSE)
  )

  # Open end
  expect_equal(
    .parse_datetime_param("2020-01-01T00:00:00Z/.."),
    list(start = "2020-01-01T00:00:00Z", end = NULL, single_dt = FALSE)
  )
})

test_that(".parse_datetime_param treats absent and empty input as no filter", {
  none <- list(start = NULL, end = NULL, single_dt = FALSE)
  expect_equal(.parse_datetime_param(NULL), none)
  expect_equal(.parse_datetime_param(""), none)
})

test_that(".parse_bbox_param returns four numbers", {
  expect_equal(.parse_bbox_param("-114.1,51,-114,51.1"), c(-114.1, 51, -114, 51.1))
  expect_null(.parse_bbox_param(NULL))
  expect_null(.parse_bbox_param(""))
})

test_that(".bbox_to_wkt closes the ring back at the first corner", {
  wkt <- .bbox_to_wkt(c(-1, -2, 3, 4))
  expect_match(wkt, "^POLYGON\\(\\(")

  coords <- strsplit(gsub("POLYGON\\(\\(|\\)\\)", "", wkt), ",")[[1]]
  expect_length(coords, 5L)
  expect_equal(trimws(coords[1]), trimws(coords[5]))
})

test_that(".split_param accepts both plumber shapes for a repeated parameter", {
  # Comma-separated GET query string
  expect_equal(.split_param("a,b,c"), c("a", "b", "c"))
  # Repeated keys arrive as a vector
  expect_equal(.split_param(c("a", "b")), c("a", "b"))
  expect_equal(.split_param("a"), "a")
  expect_null(.split_param(NULL))
  expect_null(.split_param(""))
})

test_that(".as_char_vec flattens a parsed JSON array", {
  expect_equal(.as_char_vec(list("a", "b")), c("a", "b"))
  expect_equal(.as_char_vec("a"), "a")
  expect_equal(.as_char_vec(list(1, 2)), c("1", "2"))
  expect_null(.as_char_vec(NULL))
  expect_null(.as_char_vec(list()))
})

test_that(".query_string encodes and drops empty values", {
  expect_equal(.query_string(a = "1", b = "2"), "a=1&b=2")
  expect_equal(.query_string(a = "1", b = NULL), "a=1")
  expect_equal(.query_string(a = "1", b = ""), "a=1")
  expect_equal(.query_string(), "")
  expect_equal(.query_string(bbox = "-1,2,3,4"), "bbox=-1%2C2%2C3%2C4")
})

test_that(".parse_json keeps single-element arrays as lists", {
  parsed <- .parse_json('{"roles": ["data"], "n": 1}')
  expect_type(parsed$roles, "list")
  expect_equal(parsed$roles[[1]], "data")
})

test_that(".datetime_sql_clause binds one parameter for an open-ended range", {
  # Point in time: $p reused three times, but bound once
  pt <- .datetime_sql_clause("2020-01-01", "2020-01-01", TRUE, 1L)
  expect_equal(pt$params, list("2020-01-01"))
  expect_match(pt$sql, "datetime = \\$1::timestamptz")

  # Open start binds dt_end
  open_start <- .datetime_sql_clause(NULL, "2021-01-01", FALSE, 1L)
  expect_equal(open_start$params, list("2021-01-01"))
  expect_match(open_start$sql, "datetime <= \\$1::timestamptz")

  # Open end binds dt_start
  open_end <- .datetime_sql_clause("2020-01-01", NULL, FALSE, 1L)
  expect_equal(open_end$params, list("2020-01-01"))
  expect_match(open_end$sql, "datetime >= \\$1::timestamptz")
})

test_that(".datetime_sql_clause binds two parameters for a closed range", {
  closed <- .datetime_sql_clause("2020-01-01", "2021-01-01", FALSE, 3L)
  expect_equal(closed$params, list("2020-01-01", "2021-01-01"))
  # Numbering continues from the offset it was given
  expect_match(closed$sql, "\\$3::timestamptz")
  expect_match(closed$sql, "\\$4::timestamptz")
})

test_that(".parse_bbox_param accepts the six-element 3D form", {
  expect_equal(
    .parse_bbox_param("-114.1,51,100,-114,51.1,200"),
    c(-114.1, 51, 100, -114, 51.1, 200)
  )
})

test_that(".parse_bbox_param still rejects any other length", {
  expect_error(.parse_bbox_param("1,2,3"), "four numbers")
  expect_error(.parse_bbox_param("1,2,3,4,5"), "four numbers")
  expect_error(.parse_bbox_param("1,2,3,4,5,6,7"), "four numbers")
  expect_error(.parse_bbox_param("1,2,3,north"), "four numbers")
})

test_that("a rejected bbox signals a bad-request condition", {
  # The router turns this class into a 400 rather than a 500
  expect_error(.parse_bbox_param("1,2,3"), class = "stacserver_bad_request")
})

test_that(".bbox_horizontal reads the horizontal corners out of either form", {
  expect_equal(.bbox_horizontal(c(-1, -2, 3, 4)), c(-1, -2, 3, 4))
  # Elevations sit in positions 3 and 6, so east/north move along
  expect_equal(.bbox_horizontal(c(-1, -2, 100, 3, 4, 200)), c(-1, -2, 3, 4))
  expect_error(.bbox_horizontal(c(1, 2, 3, 4, 5)), "four or six")
})

test_that(".bbox_to_wkt uses the horizontal corners of a 3D bbox", {
  flat <- .bbox_to_wkt(c(-1, -2, 3, 4))
  cube <- .bbox_to_wkt(c(-1, -2, 100, 3, 4, 200))
  expect_equal(flat, cube)
})

test_that(".bbox_to_wkt keeps full coordinate precision", {
  wkt <- .bbox_to_wkt(c(-114.123456789, 51.987654321, -114, 52))
  expect_match(wkt, "-114.123456789", fixed = TRUE)
  expect_match(wkt, "51.987654321", fixed = TRUE)
})

test_that(".parse_int_param accepts integers and falls back to the default", {
  expect_equal(.parse_int_param("25", "limit", 10L), 25L)
  expect_equal(.parse_int_param(25, "limit", 10L), 25L)
  expect_equal(.parse_int_param("", "limit", 10L), 10L)
  expect_equal(.parse_int_param(NULL, "limit", 10L), 10L)
})

test_that(".parse_int_param rejects values that would become NA", {
  # as.integer("abc") is NA, and LIMIT NULL in PostgreSQL means "no limit",
  # so a non-integer must never reach the query
  expect_error(.parse_int_param("abc", "limit", 10L), class = "stacserver_bad_request")
  expect_error(.parse_int_param("abc", "limit", 10L), "must be an integer")
  expect_error(.parse_int_param("1.5", "limit", 10L), "must be an integer")
})

test_that(".parse_int_param enforces its bounds", {
  expect_error(
    .parse_int_param("0", "limit", 10L, min = 1L, max = 10000L),
    "at least 1"
  )
  expect_error(
    .parse_int_param("-1", "offset", 0L, min = 0L),
    "at least 0"
  )
  expect_error(
    .parse_int_param("10001", "limit", 10L, min = 1L, max = 10000L),
    "at most 10000"
  )
  expect_equal(.parse_int_param("10000", "limit", 10L, min = 1L, max = 10000L), 10000L)
})

test_that(".parse_bbox_body flattens a JSON array and reports non-numbers", {
  expect_equal(.parse_bbox_body(list(1, 2, 3, 4)), c(1, 2, 3, 4))
  expect_null(.parse_bbox_body(NULL))
  # Strings become NA rather than erroring, so validation is what rejects them
  expect_error(
    .validate_bbox(.parse_bbox_body(list("a", "b", "c", "d"))),
    class = "stacserver_bad_request"
  )
})

test_that(".parse_intersects_param accepts a geometry in either shape", {
  # A GET query string arrives as JSON text
  from_get <- .parse_intersects_param(
    '{"type":"Point","coordinates":[-114,51]}'
  )
  expect_equal(from_get$type, "Point")

  # A POST body arrives already parsed
  from_post <- .parse_intersects_param(
    list(type = "Polygon", coordinates = list(list(list(0, 0))))
  )
  expect_equal(from_post$type, "Polygon")
})

test_that(".parse_intersects_param treats absent and empty input as no filter", {
  expect_null(.parse_intersects_param(NULL))
  expect_null(.parse_intersects_param(""))
})

test_that(".parse_intersects_param accepts every GeoJSON geometry type", {
  for (type in .geojson_geometry_types) {
    member <- if (type == "GeometryCollection") "geometries" else "coordinates"
    geom <- setNames(list(type, list()), c("type", member))
    expect_equal(.parse_intersects_param(geom)$type, type)
  }
})

test_that(".parse_intersects_param rejects a Feature rather than unwrapping it", {
  # Unwrapping would answer a different search than the client asked for
  expect_error(
    .parse_intersects_param(
      list(type = "Feature", geometry = list(type = "Point", coordinates = list(0, 0)))
    ),
    "not a 'Feature'"
  )
  expect_error(
    .parse_intersects_param(list(type = "FeatureCollection", features = list())),
    class = "stacserver_bad_request"
  )
})

test_that(".parse_intersects_param rejects malformed input as a bad request", {
  # Not JSON at all
  expect_error(
    .parse_intersects_param("not json"),
    class = "stacserver_bad_request"
  )
  # JSON, but no type
  expect_error(
    .parse_intersects_param('{"coordinates":[0,0]}'),
    "must be a GeoJSON geometry object"
  )
  # A type this is not
  expect_error(
    .parse_intersects_param('{"type":"Circle","coordinates":[0,0]}'),
    "not a 'Circle'"
  )
  # The right type with nothing in it: PostGIS would reject this, and a 500 is
  # the wrong answer to a malformed request
  expect_error(
    .parse_intersects_param('{"type":"Polygon"}'),
    "must have a 'coordinates' member"
  )
  expect_error(
    .parse_intersects_param('{"type":"GeometryCollection"}'),
    "must have a 'geometries' member"
  )
})

test_that(".check_spatial_filters rejects bbox and intersects together", {
  geom <- list(type = "Point", coordinates = list(0, 0))
  expect_error(
    .check_spatial_filters(c(-1, -1, 1, 1), geom),
    class = "stacserver_bad_request"
  )
  expect_error(
    .check_spatial_filters(c(-1, -1, 1, 1), geom),
    "Only one of 'bbox' and 'intersects'"
  )
  # Either alone, or neither, is fine
  expect_null(.check_spatial_filters(c(-1, -1, 1, 1), NULL))
  expect_null(.check_spatial_filters(NULL, geom))
  expect_null(.check_spatial_filters(NULL, NULL))
})

test_that(".extent_bound formats an RFC 3339 UTC string", {
  t <- as.POSIXct("2024-06-01 18:22:31", tz = "UTC")
  expect_equal(.extent_bound(t, "start"), "2024-06-01T18:22:31Z")
  expect_equal(.extent_bound(t, "end"), "2024-06-01T18:22:31Z")
})

test_that(".extent_bound rounds a sub-second bound outward", {
  # An extent is a bound: rounding the start up or the end down would leave a
  # collection that no longer covers the item the bound came from
  t <- as.POSIXct("2024-06-01 18:22:31.024", tz = "UTC")
  expect_equal(.extent_bound(t, "start"), "2024-06-01T18:22:31Z")
  expect_equal(.extent_bound(t, "end"), "2024-06-01T18:22:32Z")
})

test_that(".extent_bound converts to UTC rather than reporting local time", {
  t <- as.POSIXct("2024-06-01 12:00:00", tz = "America/Edmonton")
  expect_equal(.extent_bound(t, "start"), "2024-06-01T18:00:00Z")
})

test_that(".extent_bound treats a missing bound as open", {
  expect_null(.extent_bound(NA, "start"))
  expect_null(.extent_bound(as.POSIXct(NA), "end"))
  expect_null(.extent_bound(character(0), "start"))
})

test_that("an open temporal bound is written as null, not dropped", {
  # An interval with an open end reaches clients only if the NULL survives
  # into the array; dropping it would leave a one-element interval, which is
  # not a valid STAC extent. Items ingested through stacbuildr always carry a
  # closed bound, so this is only reachable for data written by other means.
  extent <- list(
    spatial = list(bbox = list(as.list(c(-1, -2, 3, 4)))),
    temporal = list(interval = list(list("2024-01-01T00:00:00Z", NULL)))
  )
  expect_match(
    .stac_to_json(extent),
    '"interval":[["2024-01-01T00:00:00Z",null]]',
    fixed = TRUE
  )

  open_start <- list(temporal = list(interval = list(list(NULL, "2024-01-01T00:00:00Z"))))
  expect_match(
    .stac_to_json(open_start),
    '"interval":[[null,"2024-01-01T00:00:00Z"]]',
    fixed = TRUE
  )
})
