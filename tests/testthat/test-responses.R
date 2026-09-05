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
  # The Context extension repeats the same counts
  expect_equal(fc$context, list(returned = 1L, matched = 10L))
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
