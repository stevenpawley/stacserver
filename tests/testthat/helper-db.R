# Helpers for the database-backed tests.
#
# These run only when STACSERVER_TEST_PG names a PostgreSQL/PostGIS database
# the tests may write to, e.g.
#   STACSERVER_TEST_PG=postgresql://postgres:postgres@localhost:5432/stac_test
# Everything they create is namespaced by a random collection id and removed
# afterwards.

skip_if_no_pg <- function() {
  testthat::skip_if_not_installed("RPostgres")
  testthat::skip_if(
    !nzchar(Sys.getenv("STACSERVER_TEST_PG")),
    "STACSERVER_TEST_PG is not set"
  )
}

# Split a connection URI into the arguments RPostgres wants. RPostgres does
# not ask libpq to expand a URI given as `dbname`, so it has to be taken apart
# here; anything that is not a URI is passed through as a database name.
pg_conn_args <- function(x) {
  if (!grepl("^postgres(ql)?://", x)) {
    return(list(dbname = x))
  }

  rest <- sub("^postgres(ql)?://", "", x)
  rest <- sub("\\?.*$", "", rest) # drop any ?sslmode=... options

  user <- NULL
  password <- NULL
  if (grepl("@", rest, fixed = TRUE)) {
    userinfo <- sub("@.*$", "", rest)
    rest <- sub("^[^@]*@", "", rest)
    user <- sub(":.*$", "", userinfo)
    if (grepl(":", userinfo, fixed = TRUE)) {
      password <- sub("^[^:]*:", "", userinfo)
    }
  }

  hostport <- sub("/.*$", "", rest)
  dbname <- if (grepl("/", rest, fixed = TRUE)) sub("^[^/]*/", "", rest) else ""

  host <- sub(":.*$", "", hostport)
  port <- if (grepl(":", hostport, fixed = TRUE)) {
    as.integer(sub("^[^:]*:", "", hostport))
  } else {
    NULL
  }

  args <- list(dbname = dbname)
  if (nzchar(host)) args$host <- host
  if (!is.null(port)) args$port <- port
  if (!is.null(user) && nzchar(user)) args$user <- user
  if (!is.null(password) && nzchar(password)) args$password <- password
  args
}

# Connect, ensure the schema exists, and disconnect when the caller finishes.
test_con <- function(env = parent.frame()) {
  args <- pg_conn_args(Sys.getenv("STACSERVER_TEST_PG"))
  con <- do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), args))
  withr::defer(DBI::dbDisconnect(con), envir = env)
  # The schema is created with IF NOT EXISTS, so every run after the first
  # reports a NOTICE per object; they are not interesting here.
  DBI::dbExecute(con, "SET client_min_messages = warning")
  stac_db_setup(con)
  con
}

# A collection id unique to this test run, dropped when the caller finishes.
test_collection_id <- function(con, env = parent.frame()) {
  id <- paste0(
    "stacserver-test-",
    paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
  )
  withr::defer(stac_db_delete_collection(con, id), envir = env)
  id
}

test_collection <- function(id, bbox = c(-114.2, 50.9, -113.9, 51.2)) {
  stacbuildr::stac_collection(
    id = id,
    description = "Fixture collection",
    license = "CC-BY-4.0",
    extent = stacbuildr::stac_extent(
      spatial_bbox = list(bbox),
      temporal_interval = list(list("2024-01-01T00:00:00Z", NULL))
    )
  )
}

test_item <- function(id, collection_id, lon, lat, datetime, assets = list()) {
  item <- stacbuildr::stac_item(
    id = id,
    geometry = list(type = "Point", coordinates = c(lon, lat)),
    bbox = c(lon, lat, lon, lat),
    datetime = datetime,
    assets = assets
  )
  item@collection <- collection_id
  item
}
