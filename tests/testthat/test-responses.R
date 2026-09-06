# Link, pagination and response-body construction. These run without a database.

test_that(".link includes optional fields only when supplied", {
  expect_equal(
    .link("self", "https://example.com"),
    list(rel = "self", href = "https://example.com")
  )
  expect_equal(
    .link("search", "https://example.com/search", "application/geo+json", "POST"),
    list(
      rel = "search",
      href = "https://example.com/search",
      type = "application/geo+json",
      method = "POST"
    )
  )
})

test_that(".merge_links keeps stored links and skips duplicate rel/href pairs", {
  existing <- list(.link("license", "https://example.com/licence.txt"))
  merged <- .merge_links(
    existing,
    list(
      .link("license", "https://example.com/licence.txt"), # duplicate
      .link("self", "https://example.com/item")
    )
  )
  expect_length(merged, 2L)
  expect_equal(merged[[1]]$rel, "license")
  expect_equal(merged[[2]]$rel, "self")
})

test_that(".merge_links distinguishes links sharing a rel but not an href", {
  merged <- .merge_links(
    list(.link("child", "https://example.com/a")),
    list(.link("child", "https://example.com/b"))
  )
  expect_length(merged, 2L)
})

test_that(".merge_links accepts NULL for an object with no stored links", {
  merged <- .merge_links(NULL, list(.link("self", "https://example.com")))
  expect_length(merged, 1L)
})

test_that(".feature_collection reports matched and returned counts", {
  fc <- .feature_collection(list(list(id = "a")), matched = 10L, returned = 1L)
  expect_equal(fc$type, "FeatureCollection")
  expect_equal(fc$numberMatched, 10L)
  expect_equal(fc$numberReturned, 1L)
  # The deprecated Context extension is not declared, so it is not emitted
  expect_null(fc$context)
})

rels <- function(links) vapply(links, function(l) l$rel, character(1))

test_that(".pagination_links omits prev on the first page", {
  links <- .pagination_links("https://example.com/search", 0L, 10L, 100L)
  expect_setequal(rels(links), c("self", "next"))
})

test_that(".pagination_links omits next on the last page", {
  links <- .pagination_links("https://example.com/search", 90L, 10L, 100L)
  expect_setequal(rels(links), c("self", "prev"))
})

test_that(".pagination_links emits both on a middle page", {
  links <- .pagination_links("https://example.com/search", 10L, 10L, 100L)
  expect_setequal(rels(links), c("self", "next", "prev"))

  nxt <- Filter(function(l) l$rel == "next", links)[[1]]
  prv <- Filter(function(l) l$rel == "prev", links)[[1]]
  expect_match(nxt$href, "offset=20")
  expect_match(prv$href, "offset=0")
})

test_that(".pagination_links omits both when one page holds every match", {
  links <- .pagination_links("https://example.com/search", 0L, 10L, 5L)
  expect_equal(rels(links), "self")
})

test_that(".pagination_links never emits a negative offset", {
  links <- .pagination_links("https://example.com/search", 5L, 10L, 100L)
  prv <- Filter(function(l) l$rel == "prev", links)[[1]]
  expect_match(prv$href, "offset=0")
})

test_that(".pagination_links appends filters with & once a query is present", {
  links <- .pagination_links(
    "https://example.com/search",
    0L,
    10L,
    100L,
    extra_query = "bbox=1%2C2%2C3%2C4"
  )
  self <- links[[1]]
  expect_match(self$href, "\\?limit=10&offset=0&bbox=1%2C2%2C3%2C4")
})

test_that(".inject_item_links adds the four navigation links", {
  item <- .inject_item_links(
    list(id = "dem-001", collection = "terrain"),
    "https://example.com"
  )
  expect_setequal(rels(item$links), c("self", "root", "collection", "parent"))

  self <- Filter(function(l) l$rel == "self", item$links)[[1]]
  expect_equal(
    self$href,
    "https://example.com/collections/terrain/items/dem-001"
  )
})

test_that(".inject_item_links preserves links already stored on the item", {
  item <- .inject_item_links(
    list(
      id = "dem-001",
      collection = "terrain",
      links = list(.link("license", "https://example.com/licence.txt"))
    ),
    "https://example.com"
  )
  expect_length(item$links, 5L)
  expect_equal(item$links[[1]]$rel, "license")
})

test_that(".inject_item_links is idempotent", {
  once <- .inject_item_links(
    list(id = "dem-001", collection = "terrain"),
    "https://example.com"
  )
  twice <- .inject_item_links(once, "https://example.com")
  expect_equal(once$links, twice$links)
})

test_that(".error_body carries the code and description", {
  body <- .error_body(404L, "Collection not found")
  expect_equal(body$code, 404L)
  expect_equal(body$description, "Collection not found")
})

test_that(".not_found sets a 404 status on the response", {
  # plumber's response object has reference semantics; an environment stands
  # in for it so the mutation is visible to the caller.
  res <- new.env(parent = emptyenv())
  res$status <- 200L
  body <- .not_found(res, "Item not found")
  expect_equal(res$status, 404L)
  expect_equal(body$code, 404L)
  expect_equal(body$description, "Item not found")
})

test_that(".stac_conformance_uris declares core, features and item-search", {
  uris <- unlist(.stac_conformance_uris())
  expect_true(any(grepl("/core$", uris)))
  expect_true(any(grepl("item-search", uris)))
  expect_true(any(grepl("ogcapi-features", uris)))
})

test_that(".pagination_links leaves no dangling separator without filters", {
  links <- .pagination_links("https://example.com/search", 10L, 10L, 100L)
  for (lnk in links) {
    expect_false(grepl("[?&]$", lnk$href))
    # Exactly one "?" introduces the query string
    expect_equal(lengths(regmatches(lnk$href, gregexpr("?", lnk$href, fixed = TRUE))), 1L)
  }
  expect_equal(links[[1]]$href, "https://example.com/search?limit=10&offset=10")
})

test_that(".merge_links replaces the rels named in override", {
  existing <- list(
    .link("self", "https://static.example.com/item.json"),
    .link("license", "https://example.com/licence.txt")
  )
  merged <- .merge_links(
    existing,
    new_links = list(.link("self", "https://api.example.com/item")),
    override = "self"
  )
  # The stored self link is gone, the unrelated one is kept
  expect_setequal(rels(merged), c("license", "self"))
  self <- Filter(function(l) l$rel == "self", merged)
  expect_length(self, 1L)
  expect_equal(self[[1]]$href, "https://api.example.com/item")
})

test_that(".inject_item_links replaces a stored static self link", {
  item <- .inject_item_links(
    list(
      id = "dem-001",
      collection = "terrain",
      links = list(
        .link("self", "https://static.example.com/terrain/dem-001.json"),
        .link("license", "https://example.com/licence.txt")
      )
    ),
    "https://api.example.com"
  )
  self <- Filter(function(l) l$rel == "self", item$links)
  expect_length(self, 1L)
  expect_equal(
    self[[1]]$href,
    "https://api.example.com/collections/terrain/items/dem-001"
  )
  # Links the API does not manage survive
  expect_true("license" %in% rels(item$links))
})

test_that("conformance declares only implemented classes", {
  uris <- unlist(.stac_conformance_uris())
  # The fields extension is not implemented, so it must not be advertised
  expect_false(any(grepl("#fields", uris, fixed = TRUE)))
  expect_true(any(grepl("#query", uris, fixed = TRUE)))
  expect_true(any(grepl("/collections$", uris)))
  # Versions match the stac_version stamped on served objects
  expect_false(any(grepl("api.stacspec.org/v1.0.0", uris, fixed = TRUE)))
  expect_true(all(grepl("v1.1.0", grep("api.stacspec.org", uris, value = TRUE), fixed = TRUE)))
})

test_that(".with_bad_request converts the condition into a 400 body", {
  res <- new.env(parent = emptyenv())
  res$status <- 200L
  body <- .with_bad_request(res, .abort_bad_request("'limit' must be an integer"))
  expect_equal(res$status, 400L)
  expect_equal(body$code, 400L)
  expect_equal(body$description, "'limit' must be an integer")
})

test_that(".with_bad_request passes other values and errors straight through", {
  res <- new.env(parent = emptyenv())
  expect_equal(.with_bad_request(res, "ok"), "ok")
  expect_error(.with_bad_request(res, stop("boom")), "boom")
})

test_that(".landing_links carries every rel the conformance classes require", {
  links <- .landing_links("https://example.com")
  expect_true(all(
    c("self", "root", "conformance", "data", "service-desc", "search") %in%
      rels(links)
  ))

  # oas30 is declared, so the OpenAPI description has to be discoverable
  desc <- Filter(function(l) l$rel == "service-desc", links)[[1]]
  expect_equal(desc$href, "https://example.com/openapi.json")

  # Item Search requires one search link per supported method
  search <- Filter(function(l) l$rel == "search", links)
  expect_setequal(vapply(search, function(l) l$method, character(1)), c("GET", "POST"))
})

test_that("the response serializer keeps full coordinate precision", {
  # jsonlite's default of 4 decimal places would round this to -114.1235,
  # about 11 m at the equator, on the way out of the API. The serializer's
  # closure holds the configured encoder, which avoids having to stand up a
  # plumber response object just to encode a value.
  encode <- environment(.stac_serializer())$serialize_fn
  expect_type(encode, "closure")

  body <- encode(list(
    id = "dem-001",
    geometry = list(
      type = "Point",
      coordinates = c(-114.123456789, 51.987654321)
    )
  ))

  expect_match(body, "-114.123456789", fixed = TRUE)
  expect_match(body, "51.987654321", fixed = TRUE)
  expect_equal(
    jsonlite::fromJSON(body)$geometry$coordinates,
    c(-114.123456789, 51.987654321)
  )
})

test_that("the response serializer unboxes scalars and writes nulls", {
  encode <- environment(.stac_serializer())$serialize_fn
  # encode() returns a classed json string, so compare the text itself
  expect_equal(as.character(encode(list(id = "a"))), '{"id":"a"}')
  # Arrays read back out of JSONB stay lists, so they stay arrays
  expect_equal(
    as.character(encode(list(roles = list("data")))),
    '{"roles":["data"]}'
  )
})

test_that(".pagination_links pages a POST search with POST links", {
  body <- list(collections = list("terrain"), bbox = list(-1, -2, 3, 4))
  links <- .pagination_links(
    "https://example.com/search",
    offset = 10L,
    limit = 10L,
    matched = 100L,
    body = body
  )
  expect_setequal(rels(links), c("self", "next", "prev"))

  for (lnk in links) {
    expect_equal(lnk$method, "POST")
    # The href carries no query string: the parameters travel in the body
    expect_equal(lnk$href, "https://example.com/search")
    # merge = FALSE says the body is complete, not a fragment to merge in
    expect_false(lnk$merge)
    expect_equal(lnk$body$collections, list("terrain"))
    expect_equal(lnk$body$bbox, list(-1, -2, 3, 4))
    expect_equal(lnk$body$limit, 10L)
  }

  nxt <- Filter(function(l) l$rel == "next", links)[[1]]
  prv <- Filter(function(l) l$rel == "prev", links)[[1]]
  expect_equal(nxt$body$offset, 20L)
  expect_equal(prv$body$offset, 0L)
})

test_that("POST paging carries the filters rather than dropping them", {
  # A GET-shaped next link would page through the whole catalog, because the
  # filters of a POST search live in the body and not in the URL
  body <- .search_body(
    bbox = c(-114.5, 50.5, -113.5, 51.5),
    datetime = "2024-06-01T00:00:00Z/..",
    collections = "terrain",
    query = list("eo:cloud_cover" = list(lt = 10))
  )
  links <- .pagination_links(
    "https://example.com/search",
    offset = 0L,
    limit = 10L,
    matched = 100L,
    body = body
  )
  nxt <- Filter(function(l) l$rel == "next", links)[[1]]

  expect_equal(nxt$body$datetime, "2024-06-01T00:00:00Z/..")
  expect_equal(nxt$body$query, list("eo:cloud_cover" = list(lt = 10)))
  expect_equal(nxt$body$collections, list("terrain"))
  expect_equal(nxt$body$offset, 10L)
})

test_that("a POST search with no filters still pages by POST", {
  links <- .pagination_links(
    "https://example.com/search",
    offset = 0L,
    limit = 10L,
    matched = 100L,
    body = .search_body()
  )
  nxt <- Filter(function(l) l$rel == "next", links)[[1]]
  expect_equal(nxt$method, "POST")
  expect_equal(nxt$body, list(limit = 10L, offset = 10L))
})

test_that(".pagination_links still builds GET links when given no body", {
  links <- .pagination_links("https://example.com/search", 0L, 10L, 100L)
  for (lnk in links) {
    expect_null(lnk$method)
    expect_null(lnk$body)
  }
})

test_that(".search_body drops absent filters and keeps arrays as arrays", {
  expect_equal(.search_body(), list())

  # The serializer unboxes length-one vectors, so a single id has to be a list
  # or it would be written back as a string and rejected on the next request
  body <- .search_body(collections = "terrain", ids = "dem-001")
  expect_equal(body$collections, list("terrain"))
  expect_equal(body$ids, list("dem-001"))
  expect_equal(
    as.character(environment(.stac_serializer())$serialize_fn(body)),
    '{"collections":["terrain"],"ids":["dem-001"]}'
  )
})

test_that(".search_body passes a geometry through unchanged", {
  geom <- list(type = "Point", coordinates = list(-114, 51))
  expect_equal(.search_body(intersects = geom)$intersects, geom)
})

test_that(".link carries body and merge only when supplied", {
  plain <- .link("next", "https://example.com/search")
  expect_null(plain$body)
  expect_null(plain$merge)

  posted <- .link(
    "next",
    "https://example.com/search",
    "application/geo+json",
    method = "POST",
    body = list(limit = 10L),
    merge = FALSE
  )
  expect_equal(posted$body, list(limit = 10L))
  expect_false(posted$merge)
})
