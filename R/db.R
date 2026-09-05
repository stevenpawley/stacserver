#' Create the STAC database schema
#'
#' Idempotently creates the `stac_collections` and `stac_items` tables and all
#' required indexes. Requires the PostGIS extension to be available.
#'
#' @param con A DBI connection.
#' @return `con`, invisibly.
#' @export
stac_db_setup <- function(con) {
  DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis")

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

  DBI::dbExecute(
    con,
    "
    CREATE INDEX IF NOT EXISTS idx_stac_items_datetime
      ON stac_items (datetime)
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

  content_json <- jsonlite::toJSON(
    as.list(collection),
    auto_unbox = TRUE,
    null = "null"
  )

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
      as.character(content_json),
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

  content_json <- jsonlite::toJSON(
    as.list(item),
    auto_unbox = TRUE,
    null = "null"
  )

  geom_wkt <- if (!is.null(item@geometry)) {
    tryCatch(
      sf::st_as_text(geojsonsf::geojson_sfc(
        jsonlite::toJSON(item@geometry, auto_unbox = TRUE)
      )),
      error = function(e) NA_character_
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
      as.character(content_json),
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
# bbox:        numeric(4) c(west, south, east, north) or NULL
# dt_start:    character ISO 8601 or NULL
# dt_end:      character ISO 8601 or NULL
# single_dt:   logical — TRUE when dt_start == dt_end (point-in-time search)
# collections: character vector or NULL
# ids:         character vector or NULL
# properties:  named list for simple property equality matching or NULL
#              e.g. list("eo:cloud_cover" = 5) -> content->'properties'->>'eo:cloud_cover' = '5'
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
  properties = NULL,
  limit = 10L,
  offset = 0L
) {
  clauses <- character(0)
  params <- list()
  p <- 1L

  if (!is.null(bbox)) {
    clauses <- c(
      clauses,
      sprintf(
        "ST_Intersects(geometry, ST_MakeEnvelope($%d, $%d, $%d, $%d, 4326))",
        p,
        p + 1L,
        p + 2L,
        p + 3L
      )
    )
    params <- c(params, list(bbox[1], bbox[2], bbox[3], bbox[4]))
    p <- p + 4L
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

  # Simple property equality filter (supports extension fields like "eo:cloud_cover")
  if (!is.null(properties) && length(properties) > 0) {
    for (key in names(properties)) {
      val <- properties[[key]]
      # Use JSONB containment for equality: content->'properties' @> $N::jsonb
      prop_json <- jsonlite::toJSON(
        setNames(list(val), key),
        auto_unbox = TRUE,
        null = "null"
      )
      clauses <- c(clauses, sprintf("content->'properties' @> $%d::jsonb", p))
      params <- c(params, list(as.character(prop_json)))
      p <- p + 1L
    }
  }

  where_sql <- if (length(clauses) > 0) {
    paste("WHERE", paste(clauses, collapse = " AND "))
  } else {
    ""
  }

  count_sql <- paste("SELECT COUNT(*) AS n FROM stac_items", where_sql)
  count_row <- DBI::dbGetQuery(con, count_sql, params = params)
  matched <- as.integer(count_row$n[[1]])

  data_sql <- paste(
    "SELECT content::text AS content FROM stac_items",
    where_sql,
    "ORDER BY datetime DESC NULLS LAST",
    sprintf("LIMIT $%d OFFSET $%d", p, p + 1L)
  )
  data_params <- c(params, list(as.integer(limit), as.integer(offset)))
  rows <- DBI::dbGetQuery(con, data_sql, params = data_params)

  list(
    items = lapply(rows$content, .parse_json),
    matched = matched
  )
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
    # Open start — items up to dt_end
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
    # Open end — items from dt_start
    sql <- sprintf(
      "
      (
        (datetime IS NOT NULL AND datetime >= $%d::timestamptz)
        OR
        (end_datetime IS NOT NULL AND end_datetime >= $%d::timestamptz)
        OR
        (start_datetime IS NOT NULL AND end_datetime IS NULL
         AND start_datetime >= $%d::timestamptz)
      )
    ",
      p,
      p,
      p
    )
    list(sql = sql, params = list(dt_start))
  } else {
    # Closed range — overlapping interval check
    sql <- sprintf(
      "
      (
        (datetime IS NOT NULL AND datetime >= $%d::timestamptz
         AND datetime <= $%d::timestamptz)
        OR
        (start_datetime IS NOT NULL AND start_datetime <= $%d::timestamptz
         AND (end_datetime IS NULL OR end_datetime >= $%d::timestamptz))
      )
    ",
      p,
      p + 1L,
      p + 1L,
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

# Parse bbox query string "west,south,east,north" -> numeric(4) or NULL
.parse_bbox_param <- function(bbox) {
  if (is.null(bbox) || !nzchar(bbox)) {
    return(NULL)
  }
  vals <- suppressWarnings(as.numeric(strsplit(bbox, ",", fixed = TRUE)[[1]]))
  if (length(vals) != 4 || any(is.na(vals))) {
    cli::cli_abort(
      "'bbox' must be a comma-separated list of four numbers: west,south,east,north"
    )
  }
  vals
}

# Convert bbox vector to WKT polygon string
.bbox_to_wkt <- function(bbox) {
  sprintf(
    "POLYGON((%f %f,%f %f,%f %f,%f %f,%f %f))",
    bbox[1],
    bbox[2],
    bbox[3],
    bbox[2],
    bbox[3],
    bbox[4],
    bbox[1],
    bbox[4],
    bbox[1],
    bbox[2]
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
