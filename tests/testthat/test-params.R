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

test_that(".parse_bbox_param rejects a bbox that is not four numbers", {
  expect_error(.parse_bbox_param("1,2,3"), "four numbers")
  expect_error(.parse_bbox_param("1,2,3,4,5"), "four numbers")
  expect_error(.parse_bbox_param("1,2,3,north"), "four numbers")
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
