# HTTP-level tests. These drive the router the way a client does, through
# plumber's routing, filters, body parsing and response serializer; see
# helper-api.R. Skipped unless STACSERVER_TEST_PG is set, like test-db.R.

test_that("the landing page is a catalog declaring its conformance classes", {
  skip_if_no_pg()
  con <- test_con()
  pr <- test_api(con, test_collection_id(con))

  res <- api_get(pr, "/")

  expect_equal(res$status, 200L)
  expect_equal(res$body$type, "Catalog")
  expect_equal(res$body$stac_version, "1.1.0")
  expect_true(length(res$body$conformsTo) > 0L)
  expect_equal(api_link(res, "self")$href, api_base_url)
  expect_equal(api_link(res, "data")$href, paste0(api_base_url, "/collections"))
})

test_that("/conformance answers with the same classes as the landing page", {
  skip_if_no_pg()
  con <- test_con()
  pr <- test_api(con, test_collection_id(con))

  landing <- api_get(pr, "/")
  conformance <- api_get(pr, "/conformance")

  expect_equal(conformance$status, 200L)
  expect_equal(conformance$body$conformsTo, landing$body$conformsTo)
})

test_that("collection responses carry links pointing back at this API", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  listed <- api_get(pr, "/collections")
  expect_equal(listed$status, 200L)
  expect_true(cid %in% vapply(listed$body$collections, function(c) c$id, character(1)))

  one <- api_get(pr, paste0("/collections/", cid))
  expect_equal(one$status, 200L)
  expect_equal(one$body$id, cid)

  self <- Filter(function(l) identical(l$rel, "self"), one$body$links)
  expect_length(self, 1L)
  expect_equal(self[[1]]$href, paste0(api_base_url, "/collections/", cid))
})

test_that("an unknown collection or item is a 404 with an error body", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  missing_collection <- api_get(pr, "/collections/no-such-collection")
  expect_equal(missing_collection$status, 404L)
  expect_equal(missing_collection$body$code, 404L)
  expect_match(missing_collection$body$description, "Collection not found")

  expect_equal(api_get(pr, "/collections/no-such-collection/items")$status, 404L)

  missing_item <- api_get(pr, paste0("/collections/", cid, "/items/no-such-item"))
  expect_equal(missing_item$status, 404L)
  expect_match(missing_item$body$description, "Item not found")
})

test_that("an items response is a FeatureCollection with navigable items", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  res <- api_get(pr, paste0("/collections/", cid, "/items"))

  expect_equal(res$status, 200L)
  expect_equal(res$body$type, "FeatureCollection")
  expect_equal(res$body$numberMatched, 4L)
  expect_equal(res$body$numberReturned, 4L)

  item <- res$body$features[[1]]
  rels <- vapply(item$links, function(l) l$rel, character(1))
  expect_true(all(c("self", "root", "collection", "parent") %in% rels))

  single <- api_get(pr, paste0("/collections/", cid, "/items/t1"))
  expect_equal(single$status, 200L)
  expect_equal(single$body$id, "t1")
  expect_equal(single$body$collection, cid)
})

test_that("GET /search filters on a bbox", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  res <- api_get(pr, "/search", collections = cid, bbox = "0.4,0,0.6,1")

  expect_equal(res$status, 200L)
  expect_setequal(api_ids(res), c("t1", "t2", "t3"))
})

test_that("GET /search filters on an intersects geometry", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  res <- api_get(pr, "/search", collections = cid, intersects = api_triangle_json())

  expect_equal(res$status, 200L)
  expect_equal(res$body$numberMatched, 3L)
  # corner lies inside the triangle's bounding box but outside the triangle,
  # which is the whole reason intersects exists alongside bbox
  expect_setequal(api_ids(res), c("t1", "t2", "t3"))

  box <- api_get(pr, "/search", collections = cid, bbox = "0,0,1,1")
  expect_true("corner" %in% api_ids(box))
})

test_that("POST /search filters on an intersects geometry", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  res <- api_post(pr, "/search", list(
    collections = list(cid),
    intersects = api_triangle()
  ))

  expect_equal(res$status, 200L)
  expect_setequal(api_ids(res), c("t1", "t2", "t3"))
})

test_that("a search rejects a request it cannot answer with a 400", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  bad_limit <- api_get(pr, "/search", limit = "0")
  expect_equal(bad_limit$status, 400L)
  expect_equal(bad_limit$body$code, 400L)

  # A non-integer limit reaching PostgreSQL as LIMIT NULL would return
  # everything rather than one page
  expect_equal(api_get(pr, "/search", limit = "abc")$status, 400L)
  expect_equal(api_get(pr, "/search", bbox = "1,2,3")$status, 400L)

  both <- api_get(pr, "/search", bbox = "0,0,1,1", intersects = api_triangle_json())
  expect_equal(both$status, 400L)
  expect_match(both$body$description, "Only one of 'bbox' and 'intersects'")

  malformed <- api_get(pr, "/search", intersects = "not json")
  expect_equal(malformed$status, 400L)
  expect_match(malformed$body$description, "GeoJSON geometry object")

  feature <- api_post(pr, "/search", list(
    intersects = list(type = "Feature", geometry = api_triangle())
  ))
  expect_equal(feature$status, 400L)
  expect_match(feature$body$description, "not a 'Feature'")
})

test_that("following the next link of a GET search keeps the filters", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  page1 <- api_get(pr, "/search", collections = cid, intersects = api_triangle_json(), limit = 2)
  expect_equal(page1$body$numberMatched, 3L)

  page2 <- api_follow(pr, api_link(page1, "next"))

  expect_equal(page2$status, 200L)
  expect_equal(page2$body$numberMatched, 3L)
  expect_length(intersect(api_ids(page1), api_ids(page2)), 0L)
  expect_false("corner" %in% c(api_ids(page1), api_ids(page2)))
})

test_that("a POST search pages with POST links carrying the whole body", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  page1 <- api_post(pr, "/search", list(
    collections = list(cid),
    bbox = list(0, 0, 1, 1),
    datetime = "2024-06-01T00:00:00Z/..",
    limit = 2
  ))

  nxt <- api_link(page1, "next")
  expect_equal(nxt$method, "POST")
  # The filters live in the body, so a link carrying only a query string would
  # page through the whole catalog instead
  expect_equal(nxt$href, paste0(api_base_url, "/search"))
  expect_equal(nxt$body$collections, list(cid))
  expect_equal(nxt$body$bbox, list(0, 0, 1, 1))
  expect_equal(nxt$body$datetime, "2024-06-01T00:00:00Z/..")
  expect_equal(nxt$body$offset, 2L)
  expect_false(nxt$merge)
})

test_that("following a POST next link returns the next page of the same search", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  page1 <- api_post(pr, "/search", list(
    collections = list(cid),
    intersects = api_triangle(),
    limit = 2
  ))
  expect_equal(page1$body$numberMatched, 3L)

  page2 <- api_follow(pr, api_link(page1, "next"))

  expect_equal(page2$status, 200L)
  # Still the filtered set: 3 matched, and corner never appears
  expect_equal(page2$body$numberMatched, 3L)
  expect_length(intersect(api_ids(page1), api_ids(page2)), 0L)
  expect_setequal(c(api_ids(page1), api_ids(page2)), c("t1", "t2", "t3"))
  expect_null(api_link(page2, "next"))

  back <- api_follow(pr, api_link(page2, "prev"))
  expect_equal(api_ids(back), api_ids(page1))
})

test_that("a single collection id survives a POST paging round trip", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  page1 <- api_post(pr, "/search", list(collections = list(cid), limit = 2))
  nxt <- api_link(page1, "next")

  # The serializer unboxes length-one vectors, so an id written back as a bare
  # string rather than an array would be an invalid body on the next request
  expect_type(nxt$body$collections, "list")
  expect_equal(nxt$body$collections, list(cid))

  page2 <- api_follow(pr, nxt)
  expect_equal(page2$body$numberMatched, 4L)
})

test_that("responses keep full coordinate precision over the wire", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  stac_db_insert_collection(con, test_collection(cid))
  stac_db_insert_item(
    con,
    test_item("precise", cid, -114.123456789, 51.987654321, "2024-06-01T00:00:00Z")
  )
  pr <- stac_api_router(con, base_url = api_base_url)

  res <- api_get(pr, paste0("/collections/", cid, "/items/precise"))

  # jsonlite's default of 4 decimal places would round these to about 11 m
  expect_match(res$text, "-114.123456789", fixed = TRUE)
  expect_match(res$text, "51.987654321", fixed = TRUE)
})

test_that("a one-element array is serialized as an array, not a string", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  res <- api_get(pr, paste0("/collections/", cid, "/items/t1"))

  expect_match(res$text, '"roles":["data"]', fixed = TRUE)
  expect_type(res$body$assets$dem$roles, "list")
})

test_that("asset hrefs are signed when the router is given a signer", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid, sign_fn = function(href) paste0(href, "?sig=test"))

  item <- api_get(pr, paste0("/collections/", cid, "/items/t1"))
  expect_equal(item$body$assets$dem$href, "https://store.example.com/dem.tif?sig=test")

  # Signing applies to search results too, not only to a single item
  searched <- api_get(pr, "/search", collections = cid, ids = "t1")
  expect_match(searched$body$features[[1]]$assets$dem$href, "?sig=test", fixed = TRUE)
})

test_that("a failing signer warns and leaves the href alone rather than 500ing", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid, sign_fn = function(href) stop("no credentials"))

  expect_warning(
    item <- api_get(pr, paste0("/collections/", cid, "/items/t1")),
    "Asset signing failed"
  )
  expect_equal(item$status, 200L)
  expect_equal(item$body$assets$dem$href, "https://store.example.com/dem.tif")
})

test_that("every response carries the CORS headers and OPTIONS is answered", {
  skip_if_no_pg()
  con <- test_con()
  cid <- test_collection_id(con)
  pr <- test_api(con, cid)

  res <- api_get(pr, "/collections")
  expect_equal(res$headers[["Access-Control-Allow-Origin"]], "*")

  preflight <- api_call(pr, api_request("OPTIONS", "/search"))
  expect_equal(preflight$status, 200L)
  expect_match(preflight$headers[["Access-Control-Allow-Methods"]], "POST")
})
