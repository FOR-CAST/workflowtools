## extract_metadata() generic + default + character (URL) -------------------------------------

testthat::test_that("extract_metadata() errors on unknown classes", {
  testthat::expect_snapshot(extract_metadata(123L), error = TRUE)
})

testthat::test_that("extract_metadata.character() constructs a record without network calls", {
  rec <- extract_metadata("https://example.com/data/foo.csv", head_lookup = FALSE)
  testthat::expect_equal(rec$source$type, "http_download")
  testthat::expect_equal(rec$source$url, "https://example.com/data/foo.csv")
  testthat::expect_equal(rec$name, "foo.csv")
  testthat::expect_equal(rec$id, "foo")
  testthat::expect_equal(rec$local_path, "data/foo.csv")
})

testthat::test_that("extract_metadata.character() honours explicit id/name/local_path", {
  rec <- extract_metadata(
    "https://example.com/data/foo.csv",
    id = "my-foo",
    name = "Foo Dataset",
    local_path = "data/processed/foo.csv",
    head_lookup = FALSE
  )
  testthat::expect_equal(rec$id, "my-foo")
  testthat::expect_equal(rec$name, "Foo Dataset")
  testthat::expect_equal(rec$local_path, "data/processed/foo.csv")
})

testthat::test_that("extract_metadata.character() id slugifies URL segments", {
  rec <- extract_metadata(
    "https://example.com/Path/With%20Spaces+stuff.geojson?ts=2026",
    head_lookup = FALSE
  )
  ## "With%20Spaces+stuff" -> "with-20spaces-stuff"
  testthat::expect_match(rec$id, "^[a-z0-9-]+$")
})

## extract_metadata.bcdc_record() ----------------------------------------------------------------

make_fake_bcdc_record <- function(
  title = "BC Test Dataset",
  bcdc_id = "12345678-aaaa-bbbb-cccc-1234567890ab",
  publisher = list(name = "Forest Analysis and Inventory Branch"),
  license = list(title = "Open Government Licence - British Columbia", id = "OGL-BC"),
  last_modified = as.POSIXct("2024-12-01", tz = "UTC"),
  record_url = "https://catalogue.data.gov.bc.ca/dataset/12345678-aaaa-bbbb-cccc-1234567890ab",
  resources = list(list(
    url = "https://catalogue.data.gov.bc.ca/dataset/12345678/resource/abc.gpkg"
  ))
) {
  structure(
    list(
      id = bcdc_id,
      title = title,
      publisher = publisher,
      license = license,
      last_modified = last_modified,
      record_url = record_url,
      resources = resources
    ),
    class = c("bcdc_record", "list")
  )
}

testthat::test_that("extract_metadata.bcdc_record() maps title, publisher, license, last_modified", {
  rec <- extract_metadata(make_fake_bcdc_record())
  testthat::expect_equal(rec$name, "BC Test Dataset")
  testthat::expect_equal(rec$source$type, "bcdata")
  testthat::expect_equal(rec$source$query_args$record_id, "12345678-aaaa-bbbb-cccc-1234567890ab")
  testthat::expect_equal(rec$license, "Open Government Licence - British Columbia")
  testthat::expect_equal(rec$version_or_vintage, "2024-12-01")
})

testthat::test_that("extract_metadata.bcdc_record() generates a citation with bibtex_key, entry, prose", {
  rec <- extract_metadata(make_fake_bcdc_record())
  testthat::expect_true(!is.null(rec$citation$bibtex_key))
  testthat::expect_match(rec$citation$bibtex_key, ":2024$")
  testthat::expect_match(rec$citation$bibtex_entry, "@misc\\{")
  testthat::expect_match(rec$citation$bibtex_entry, "title = \\{\\{BC Test Dataset\\}\\}")
  testthat::expect_match(
    rec$citation$formatted_text,
    "Forest Analysis and Inventory Branch \\(2024\\)\\. BC Test Dataset\\."
  )
  testthat::expect_false(rec$citation$external)
})

testthat::test_that("extract_metadata.bcdc_record() uses bcdc id as the default record id", {
  rec <- extract_metadata(make_fake_bcdc_record())
  testthat::expect_equal(rec$id, "12345678-aaaa-bbbb-cccc-1234567890ab")
})

testthat::test_that("extract_metadata.bcdc_record() preserves bcdc_record_id in extra", {
  rec <- extract_metadata(make_fake_bcdc_record())
  testthat::expect_equal(rec$extra$bcdc_record_id, "12345678-aaaa-bbbb-cccc-1234567890ab")
})

testthat::test_that("extract_metadata.bcdc_record() degrades when publisher/license are missing", {
  rec <- extract_metadata(make_fake_bcdc_record(publisher = NULL, license = NULL))
  testthat::expect_false("license" %in% names(rec))
  testthat::expect_match(rec$citation$bibtex_key, "^BCDC:")
})

## metadata_bcdata() caching layer ---------------------------------------------------------------

testthat::test_that("metadata_bcdata() caches the extracted record and hits the cache on rerun", {
  testthat::skip_if_not_installed("withr")
  cache_dir <- withr::local_tempdir()
  withr::local_options(workflowtools.metadata_cache.dir = cache_dir)

  fake <- make_fake_bcdc_record(title = "Cached Test", bcdc_id = "cache-test")

  ## Replace bcdata::bcdc_get_record() with a counting mock.
  call_count <- 0L
  testthat::local_mocked_bindings(
    bcdc_get_record = function(record_id) {
      call_count <<- call_count + 1L
      fake
    },
    .package = "bcdata"
  )

  rec1 <- metadata_bcdata("cache-test")
  rec2 <- metadata_bcdata("cache-test")
  testthat::expect_equal(call_count, 1L)
  testthat::expect_equal(rec1$id, rec2$id)
  testthat::expect_equal(rec1$name, "Cached Test")
})

testthat::test_that("metadata_bcdata(refresh = TRUE) bypasses the cache", {
  testthat::skip_if_not_installed("withr")
  cache_dir <- withr::local_tempdir()
  withr::local_options(workflowtools.metadata_cache.dir = cache_dir)

  fake <- make_fake_bcdc_record(title = "Refresh Test", bcdc_id = "refresh-test")
  call_count <- 0L
  testthat::local_mocked_bindings(
    bcdc_get_record = function(record_id) {
      call_count <<- call_count + 1L
      fake
    },
    .package = "bcdata"
  )

  metadata_bcdata("refresh-test")
  metadata_bcdata("refresh-test", refresh = TRUE)
  testthat::expect_equal(call_count, 2L)
})

testthat::test_that("metadata_bcdata(ttl_days = 0) treats every cached entry as stale", {
  testthat::skip_if_not_installed("withr")
  cache_dir <- withr::local_tempdir()
  withr::local_options(workflowtools.metadata_cache.dir = cache_dir)

  fake <- make_fake_bcdc_record(title = "TTL Test", bcdc_id = "ttl-test")
  call_count <- 0L
  testthat::local_mocked_bindings(
    bcdc_get_record = function(record_id) {
      call_count <<- call_count + 1L
      fake
    },
    .package = "bcdata"
  )

  metadata_bcdata("ttl-test")
  metadata_bcdata("ttl-test", ttl_days = 0L)
  testthat::expect_equal(call_count, 2L)
})

testthat::test_that("metadata_bcdata() errors when bcdata is not installed", {
  ## Force the requireNamespace check to fail by stubbing requireNamespace.
  testthat::skip_if_not_installed("withr")
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  testthat::expect_snapshot(metadata_bcdata("anything"), error = TRUE)
})
