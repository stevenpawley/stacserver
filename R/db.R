#' Create the STAC database schema
#'
#' Idempotently creates the `stac_collections` and `stac_items` tables and all
#' required indexes. Requires the PostGIS extension to be available.
#'
#' `CREATE EXTENSION postgis` needs elevated privileges. On managed PostgreSQL
#' services PostGIS is usually pre-installed and the connecting role is not a
#' superuser, so a failure to create the extension is tolerated as long as
#' PostGIS is actually present; only a genuinely missing PostGIS aborts.
#'
#' @param con A DBI connection, or a `pool::dbPool()` object. A pool is
#'   recommended for long-running servers — see [stac_api_router()].
#' @return `con`, invisibly.
#' @export
stac_db_setup <- function(con) {
  .ensure_postgis(con)

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS stac_collections (
      id             TEXT PRIMARY KEY,
      content        JSONB        NOT NULL,
      spatial_extent GEOMETRY(Geometry, 4326),
      datetime_start TIMESTAMPTZ,
      datetime_end   TIMESTAMPTZ,
      created_at     TIMESTAMPTZ  DEFAULT NOW(),
      updated_at     TIMESTAMPTZ  DEFAULT NOW()
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE INDEX IF NOT EXISTS idx_stac_collections_spatial
      ON stac_collections USING GIST (spatial_extent)
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE INDEX IF NOT EXISTS idx_stac_collections_content
      ON stac_collections USING GIN (content)
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS stac_items (
      id             TEXT         NOT NULL,
      collection_id  TEXT         NOT NULL
        REFERENCES stac_collections(id) ON DELETE CASCADE,
      content        JSONB        NOT NULL,
      geometry       GEOMETRY(Geometry, 4326),
      datetime       TIMESTAMPTZ,
      start_datetime TIMESTAMPTZ,
      end_datetime   TIMESTAMPTZ,
      created_at     TIMESTAMPTZ  DEFAULT NOW(),
      updated_at     TIMESTAMPTZ  DEFAULT NOW(),
      PRIMARY KEY (id, collection_id)
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE INDEX IF NOT EXISTS idx_stac_items_geometry
      ON stac_items USING GIST (geometry)
  "
  )

  # Paging orders by (datetime DESC, collection_id, id); the composite index
  # matches that ordering so a page can be served without a full sort.
  DBI::dbExecute(
    con,
    "
    CREATE INDEX IF NOT EXISTS idx_stac_items_datetime
      ON stac_items (datetime DESC, collection_id, id)
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE INDEX IF NOT EXISTS idx_stac_items_collection
      ON stac_items (collection_id)
  "
  )

  # GIN index over item properties enables filtering on any extension field
  # (e.g. eo:cloud_cover, sci:doi, classification:classes)
  DBI::dbExecute(
    con,
    "
    CREATE INDEX IF NOT EXISTS idx_stac_items_properties
      ON stac_items USING GIN ((content->'properties'))
  "
  )

  invisible(con)
}

# Create the PostGIS extension, tolerating a role that lacks the privilege as
# long as PostGIS is already installed.
.ensure_postgis <- function(con) {
  created <- tryCatch(
    {
      DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis")
      TRUE
    },
    error = function(e) {
      structure(FALSE, message = conditionMessage(e))
    }
  )

  if (isTRUE(created)) {
    return(invisible(TRUE))
  }

  installed <- tryCatch(
    {
      row <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM pg_extension WHERE extname = 'postgis'"
      )
      as.integer(row$n[[1]]) > 0L
    },
    error = function(e) FALSE
  )

  if (!installed) {
    cli::cli_abort(c(
      "PostGIS is not available on this database.",
      x = "{attr(created, 'message')}",
      i = "Install it as a superuser with {.code CREATE EXTENSION postgis}, or
           enable it through your managed database provider."
    ))
  }

  invisible(TRUE)
}

# Serialise a STAC object to JSON for storage.
#
# digits = NA keeps full numeric precision. The jsonlite default of 4 decimal
# places would silently round coordinates to roughly 11 m at the equator.
.stac_to_json <- function(x) {
  as.character(jsonlite::toJSON(
    as.list(x),
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
}

#' Insert or update a STAC Collection in the database
#'
#' @param con A DBI connection.
#' @param collection A [stacbuildr::stac_collection()] object.
#' @return `collection`, invisibly.
#' @export
stac_db_insert_collection <- function(con, collection) {
  if (!inherits(collection, "stac_collection")) {
    cli::cli_abort("'collection' must be a stac_collection object")
  }

  content_json <- .stac_to_json(collection)

  bbox <- collection@extent@spatial@bbox[[1]]
  geom_wkt <- .bbox_to_wkt(bbox)

  interval <- collection@extent@temporal@interval[[1]]
  dt_start <- interval[[1]]
  dt_end <- interval[[2]]

  DBI::dbExecute(
    con,
    "
    INSERT INTO stac_collections
      (id, content, spatial_extent, datetime_start, datetime_end, updated_at)
    VALUES
      ($1, $2::jsonb, ST_GeomFromText($3, 4326), $4::timestamptz, $5::timestamptz, NOW())
    ON CONFLICT (id) DO UPDATE SET
      content        = EXCLUDED.content,
      spatial_extent = EXCLUDED.spatial_extent,
      datetime_start = EXCLUDED.datetime_start,
      datetime_end   = EXCLUDED.datetime_end,
      updated_at     = NOW()
  ",
    params = list(
      collection@id,
      content_json,
      geom_wkt,
      dt_start %||% NA_character_,
      dt_end %||% NA_character_
    )
  )

  invisible(collection)
}

#' Insert or update a STAC Item in the database
#'
#' @param con A DBI connection.
#' @param item A [stacbuildr::stac_item()] object. Must have `item@collection` set.
#' @return `item`, invisibly.
#' @export
stac_db_insert_item <- function(con, item) {
  if (!inherits(item, "stac_item")) {
    cli::cli_abort("'item' must be a stac_item object")
  }
  if (is.null(item@collection) || nchar(item@collection) == 0) {
    cli::cli_abort("item@collection must be set before inserting")
  }

  content_json <- .stac_to_json(item)

  geom_wkt <- if (!is.null(item@geometry)) {
    tryCatch(
      sf::st_as_text(geojsonsf::geojson_sfc(
        jsonlite::toJSON(item@geometry, auto_unbox = TRUE, digits = NA)
      )),
      error = function(e) {
        # An item stored without a geometry can never match a spatial search,
        # so the failure must not pass silently.
        cli::cli_warn(c(
          "Could not convert the geometry of item {.val {item@id}}; it will be
           stored without one and will not match spatial searches.",
          x = conditionMessage(e)
        ))
        NA_character_
      }
    )
  } else {
    NA_character_
  }

  props <- item@properties
  datetime <- props$datetime %||% NA_character_
  start_dt <- props$start_datetime %||% NA_character_
  end_dt <- props$end_datetime %||% NA_character_

  DBI::dbExecute(
    con,
    "
    INSERT INTO stac_items
      (id, collection_id, content, geometry, datetime, start_datetime, end_datetime, updated_at)
    VALUES
      ($1, $2, $3::jsonb,
       CASE WHEN $4::text IS NULL THEN NULL ELSE ST_GeomFromText($4, 4326) END,
       $5::timestamptz, $6::timestamptz, $7::timestamptz, NOW())
    ON CONFLICT (id, collection_id) DO UPDATE SET
      content        = EXCLUDED.content,
      geometry       = EXCLUDED.geometry,
      datetime       = EXCLUDED.datetime,
      start_datetime = EXCLUDED.start_datetime,
      end_datetime   = EXCLUDED.end_datetime,
      updated_at     = NOW()
  ",
    params = list(
      item@id,
      item@collection,
      content_json,
      geom_wkt,
      datetime,
      start_dt,
      end_dt
    )
  )

  invisible(item)
}

#' Delete a STAC Item from the database
#'
#' @param con A DBI connection.
#' @param id Item ID.
#' @param collection_id Collection ID.
#' @return `NULL`, invisibly.
#' @export
stac_db_delete_item <- function(con, id, collection_id) {
  DBI::dbExecute(
    con,
    "DELETE FROM stac_items WHERE id = $1 AND collection_id = $2",
    params = list(id, collection_id)
  )
  invisible(NULL)
}

#' Delete a STAC Collection and all its items from the database
#'
#' Items are removed via the `ON DELETE CASCADE` foreign key.
#'
#' @param con A DBI connection.
#' @param id Collection ID.
#' @return `NULL`, invisibly.
#' @export
stac_db_delete_collection <- function(con, id) {
  DBI::dbExecute(
    con,
    "DELETE FROM stac_collections WHERE id = $1",
    params = list(id)
  )
  invisible(NULL)
}

.db_get_all_collections <- function(con) {
  rows <- DBI::dbGetQuery(
    con,
    "SELECT content::text AS content FROM stac_collections ORDER BY id"
  )
  lapply(rows$content, .parse_json)
}

.db_get_collection <- function(con, collection_id) {
  rows <- DBI::dbGetQuery(
    con,
    "SELECT content::text AS content FROM stac_collections WHERE id = $1",
    params = list(collection_id)
  )
  if (nrow(rows) == 0) {
    return(NULL)
  }
  .parse_json(rows$content[[1]])
}

.db_get_item <- function(con, collection_id, item_id) {
  rows <- DBI::dbGetQuery(
    con,
    "
    SELECT content::text AS content
    FROM stac_items
    WHERE collection_id = $1 AND id = $2
  ",
    params = list(collection_id, item_id)
  )
  if (nrow(rows) == 0) {
    return(NULL)
  }
  .parse_json(rows$content[[1]])
}

# Search items. Returns list(items = <list of parsed JSON>, matched = <integer>).
# bbox:        numeric(4) c(west, south, east, north), numeric(6) with the
#              elevation values in positions 3 and 6, or NULL
# dt_start:    character ISO 8601 or NULL
# dt_end:      character ISO 8601 or NULL
# single_dt:   logical - TRUE when dt_start == dt_end (point-in-time search)
# collections: character vector or NULL
# ids:         character vector or NULL
# query:       named list of property filters in STAC Query extension form,
#              either a bare value for equality or a named list of operators
#              e.g. list("eo:cloud_cover" = list(lt = 10)), or NULL
# limit:       integer
# offset:      integer
.db_search_items <- function(
  con,
  bbox = NULL,
  dt_start = NULL,
  dt_end = NULL,
  single_dt = FALSE,
  collections = NULL,
  ids = NULL,
  query = NULL,
  limit = 10L,
  offset = 0L
) {
  clauses <- character(0)
  params <- list()
  p <- 1L

  if (!is.null(bbox)) {
    bbox_part <- .bbox_sql_clause(bbox, p)
    clauses <- c(clauses, bbox_part$sql)
    params <- c(params, bbox_part$params)
    p <- p + length(bbox_part$params)
  }

  if (!is.null(dt_start) || !is.null(dt_end)) {
    dt_clause <- .datetime_sql_clause(dt_start, dt_end, single_dt, p)
    clauses <- c(clauses, dt_clause$sql)
    params <- c(params, dt_clause$params)
    p <- p + length(dt_clause$params)
  }

  if (!is.null(collections) && length(collections) > 0) {
    placeholders <- paste(
      sprintf("$%d", seq(p, p + length(collections) - 1L)),
      collapse = ", "
    )
    clauses <- c(clauses, sprintf("collection_id IN (%s)", placeholders))
    params <- c(params, as.list(collections))
    p <- p + length(collections)
  }

  if (!is.null(ids) && length(ids) > 0) {
    placeholders <- paste(
      sprintf("$%d", seq(p, p + length(ids) - 1L)),
      collapse = ", "
    )
    clauses <- c(clauses, sprintf("id IN (%s)", placeholders))
    params <- c(params, as.list(ids))
    p <- p + length(ids)
  }

  # Property filters (STAC API Query extension), supporting extension fields
  # such as "eo:cloud_cover"
  if (!is.null(query) && length(query) > 0) {
    query_part <- .query_sql_clause(query, p)
    clauses <- c(clauses, query_part$sql)
    params <- c(params, query_part$params)
    p <- p + length(query_part$params)
  }

  where_sql <- if (length(clauses) > 0) {
    paste("WHERE", paste(clauses, collapse = " AND "))
  } else {
    ""
  }

  # COUNT(*) OVER () carries the total match count on every returned row, so
  # the common case needs one pass over the filtered set rather than two.
  # ORDER BY includes the primary key so that paging is stable: ordering by
  # datetime alone leaves items sharing a timestamp free to move between
  # pages, which makes OFFSET paging skip and repeat them.
  data_sql <- paste(
    "SELECT content::text AS content, COUNT(*) OVER () AS total_matched",
    "FROM stac_items",
    where_sql,
    "ORDER BY datetime DESC NULLS LAST, collection_id, id",
    sprintf("LIMIT $%d OFFSET $%d", p, p + 1L)
  )
  data_params <- c(params, list(as.integer(limit), as.integer(offset)))
  rows <- DBI::dbGetQuery(con, data_sql, params = data_params)

  matched <- if (nrow(rows) > 0) {
    as.integer(rows$total_matched[[1]])
  } else {
    # An empty page (offset past the end, or no matches at all) carries no
    # window-function row to read the count from, so ask for it directly.
    count_sql <- paste("SELECT COUNT(*) AS n FROM stac_items", where_sql)
    count_row <- DBI::dbGetQuery(con, count_sql, params = params)
    as.integer(count_row$n[[1]])
  }

  list(
    items = lapply(rows$content, .parse_json),
    matched = matched
  )
}

# Build the SQL spatial clause and parameters for a bbox.
# Returns list(sql = character(1), params = list).
#
# A bbox whose west edge is greater than its east edge crosses the
# antimeridian and covers two envelopes, one either side of 180 degrees.
.bbox_sql_clause <- function(bbox, p) {
  b <- .bbox_horizontal(bbox)
  west <- b[1]
  south <- b[2]
  east <- b[3]
  north <- b[4]

  if (west > east) {
    sql <- sprintf(
      "(
        ST_Intersects(geometry, ST_MakeEnvelope($%d, $%d, $%d, $%d, 4326))
        OR
        ST_Intersects(geometry, ST_MakeEnvelope($%d, $%d, $%d, $%d, 4326))
      )",
      p,
      p + 1L,
      p + 2L,
      p + 3L,
      p + 4L,
      p + 5L,
      p + 6L,
      p + 7L
    )
    return(list(
      sql = sql,
      params = list(west, south, 180, north, -180, south, east, north)
    ))
  }

  sql <- sprintf(
    "ST_Intersects(geometry, ST_MakeEnvelope($%d, $%d, $%d, $%d, 4326))",
    p,
    p + 1L,
    p + 2L,
    p + 3L
  )
  list(sql = sql, params = list(west, south, east, north))
}

# Build SQL property-filter clauses and parameters from a STAC Query extension
# object. Returns list(sql = character(n), params = list).
.query_sql_clause <- function(query, p) {
  sql <- character(0)
  params <- list()

  for (key in names(query)) {
    spec <- query[[key]]

    # A bare value is shorthand for equality
    if (!is.list(spec) || is.null(names(spec))) {
      spec <- list(eq = spec)
    }

    for (op in names(spec)) {
      part <- .query_op_clause(key, op, spec[[op]], p)
      sql <- c(sql, part$sql)
      params <- c(params, part$params)
      p <- p + length(part$params)
    }
  }

  list(sql = sql, params = params)
}

# Field accessors used by the Query extension operators. `->` keeps the JSON
# value (for containment and type tests), `->>` extracts it as text. The
# property name is bound as a parameter and cast explicitly, because `->` and
# `->>` are overloaded on text and integer and an untyped parameter leaves
# Postgres unable to choose between them.
.query_field <- function(p) {
  list(
    json = sprintf("content->'properties'->$%d::text", p),
    text = sprintf("content->'properties'->>$%d::text", p)
  )
}

.query_op_clause <- function(key, op, value, p) {
  # Equality and inequality go through JSONB containment, which the GIN index
  # on content->'properties' can serve directly.
  if (op %in% c("eq", "neq")) {
    as_json <- as.character(jsonlite::toJSON(
      setNames(list(value), key),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ))
    sql <- sprintf("content->'properties' @> $%d::jsonb", p)
    if (identical(op, "neq")) {
      sql <- sprintf("NOT (%s)", sql)
    }
    return(list(sql = sql, params = list(as_json)))
  }

  fld <- .query_field(p)
  key_param <- list(key)

  if (op %in% c("lt", "lte", "gt", "gte")) {
    sql_op <- c(lt = "<", lte = "<=", gt = ">", gte = ">=")[[op]]

    if (is.numeric(value)) {
      # The type guard keeps the numeric cast away from properties that hold a
      # string in some items and a number in others.
      sql <- sprintf(
        "(jsonb_typeof(%s) = 'number' AND (%s)::numeric %s $%d::numeric)",
        fld$json,
        fld$text,
        sql_op,
        p + 1L
      )
      return(list(sql = sql, params = c(key_param, list(as.numeric(value)))))
    }

    sql <- sprintf("(%s) %s $%d::text", fld$text, sql_op, p + 1L)
    return(list(sql = sql, params = c(key_param, list(as.character(value)))))
  }

  # The string operators use functions rather than LIKE so that a value
  # containing % or _ needs no escaping.
  if (identical(op, "startsWith")) {
    sql <- sprintf("starts_with(%s, $%d::text)", fld$text, p + 1L)
    return(list(sql = sql, params = c(key_param, list(as.character(value)))))
  }

  if (identical(op, "endsWith")) {
    sql <- sprintf(
      "right(%s, length($%d::text)) = $%d::text",
      fld$text,
      p + 1L,
      p + 1L
    )
    return(list(sql = sql, params = c(key_param, list(as.character(value)))))
  }

  if (identical(op, "contains")) {
    sql <- sprintf("position($%d::text in %s) > 0", p + 1L, fld$text)
    return(list(sql = sql, params = c(key_param, list(as.character(value)))))
  }

  if (identical(op, "in")) {
    vals <- as.character(unlist(value))
    if (length(vals) == 0) {
      # An empty set matches nothing
      return(list(sql = "FALSE", params = list()))
    }
    placeholders <- paste(
      sprintf("$%d::text", seq(p + 1L, p + length(vals))),
      collapse = ", "
    )
    sql <- sprintf("(%s) IN (%s)", fld$text, placeholders)
    return(list(sql = sql, params = c(key_param, as.list(vals))))
  }

  cli::cli_abort(c(
    "Unsupported query operator {.val {op}} for property {.val {key}}.",
    i = "Supported operators are {.val eq}, {.val neq}, {.val lt}, {.val lte},
         {.val gt}, {.val gte}, {.val startsWith}, {.val endsWith},
         {.val contains} and {.val in}."
  ))
}

# Build SQL datetime clause and parameters list.
# Returns list(sql = character(1), params = list).
.datetime_sql_clause <- function(dt_start, dt_end, single_dt, p) {
  if (single_dt) {
    # Point-in-time: items whose datetime == $p, or whose range contains $p
    sql <- sprintf(
      "
      (
        (datetime IS NOT NULL AND datetime = $%d::timestamptz)
        OR
        (start_datetime IS NOT NULL
         AND start_datetime <= $%d::timestamptz
         AND (end_datetime IS NULL OR end_datetime >= $%d::timestamptz))
      )
    ",
      p,
      p,
      p
    )
    list(sql = sql, params = list(dt_start))
  } else if (is.null(dt_start)) {
    # Open start - items up to dt_end
    sql <- sprintf(
      "
      (
        (datetime IS NOT NULL AND datetime <= $%d::timestamptz)
        OR
        (start_datetime IS NOT NULL AND start_datetime <= $%d::timestamptz)
      )
    ",
      p,
      p
    )
    list(sql = sql, params = list(dt_end))
  } else if (is.null(dt_end)) {
    # Open end - items from dt_start
    sql <- sprintf(
      "
      (
        (datetime IS NOT NULL AND datetime >= $%d::timestamptz)
        OR
        (end_datetime IS NOT NULL AND end_datetime >= $%d::timestamptz)
        OR
        (start_datetime IS NOT NULL AND end_datetime IS NULL)
      )
    ",
      p,
      p
    )
    list(sql = sql, params = list(dt_start))
  } else {
    # Closed range - overlapping interval check
    sql <- sprintf(
      "
      (
        (datetime IS NOT NULL AND datetime >= $%d::timestamptz
         AND datetime <= $%d::timestamptz)
        OR
        (start_datetime IS NOT NULL AND start_datetime <= $%d::timestamptz
         AND (end_datetime IS NULL OR end_datetime >= $%d::timestamptz))
        OR
        (start_datetime IS NULL AND end_datetime IS NOT NULL
         AND end_datetime >= $%d::timestamptz)
      )
    ",
      p,
      p + 1L,
      p + 1L,
      p,
      p
    )
    list(sql = sql, params = list(dt_start, dt_end))
  }
}

# Parse the STAC datetime query parameter ("2020/.." / "2020/2021" / "2020")
# Returns list(start, end, single_dt)
.parse_datetime_param <- function(datetime) {
  if (is.null(datetime) || !nzchar(datetime)) {
    return(list(start = NULL, end = NULL, single_dt = FALSE))
  }

  parts <- strsplit(datetime, "/", fixed = TRUE)[[1]]

  if (length(parts) == 1L) {
    return(list(start = parts[1], end = parts[1], single_dt = TRUE))
  }

  start <- if (parts[1] == "..") NULL else parts[1]
  end <- if (length(parts) < 2 || parts[2] == "..") NULL else parts[2]
  list(start = start, end = end, single_dt = FALSE)
}

# Parse bbox query string -> numeric(4) or numeric(6), or NULL.
# "west,south,east,north" or, for a 3D bbox,
# "west,south,min_elevation,east,north,max_elevation".
.parse_bbox_param <- function(bbox) {
  if (is.null(bbox) || !nzchar(bbox)) {
    return(NULL)
  }
  vals <- suppressWarnings(as.numeric(strsplit(bbox, ",", fixed = TRUE)[[1]]))
  .validate_bbox(vals)
}

# Shared validation for a bbox arriving as a query string or a JSON array.
# Signals a bad-request condition so the router can answer with a 400.
.validate_bbox <- function(vals) {
  if (!length(vals) %in% c(4L, 6L) || any(is.na(vals))) {
    .abort_bad_request(paste(
      "'bbox' must be four numbers (west,south,east,north)",
      "or six (west,south,min_elevation,east,north,max_elevation)"
    ))
  }
  vals
}

# Reduce a 4- or 6-element bbox to c(west, south, east, north). A 6-element
# bbox carries elevations in positions 3 and 6, so the horizontal values are
# not in the same places as in the 4-element form.
.bbox_horizontal <- function(bbox) {
  if (length(bbox) == 6L) {
    return(c(bbox[1], bbox[2], bbox[4], bbox[5]))
  }
  if (length(bbox) != 4L) {
    cli::cli_abort(
      "A bbox must have four or six elements, not {length(bbox)}."
    )
  }
  bbox[1:4]
}

# Convert bbox vector to WKT polygon string
.bbox_to_wkt <- function(bbox) {
  b <- .bbox_horizontal(bbox)
  sprintf(
    "POLYGON((%.17g %.17g,%.17g %.17g,%.17g %.17g,%.17g %.17g,%.17g %.17g))",
    b[1],
    b[2],
    b[3],
    b[2],
    b[3],
    b[4],
    b[1],
    b[4],
    b[1],
    b[2]
  )
}

# Parse JSONB text to R list, preserving arrays and nulls
.parse_json <- function(json_text) {
  jsonlite::fromJSON(
    json_text,
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}
