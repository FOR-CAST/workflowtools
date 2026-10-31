## Helpers ----------------------------------------------------------------------------------------

make_record <- function(id = "test-ds", ...) {
  defaults <- list(
    id = id,
    name = "Test Dataset",
    source = list(type = "http_download", url = "https://example.com/data.csv"),
    local_path = sprintf("data/%s.csv", id),
    retrieved_at = "2026-06-09T20:00:00Z"
  )
  args <- modifyList(defaults, list(...))
  do.call(input_manifest_record, args)
}

## Constructor + schema --------------------------------------------------------------------------

testthat::test_that("input_manifest_record() returns a record with required fields", {
  rec <- make_record()
  testthat::expect_equal(rec$id, "test-ds")
  testthat::expect_equal(rec$retrieved_at, "2026-06-09T20:00:00Z")
  testthat::expect_equal(rec$source$type, "http_download")
})

testthat::test_that("input_manifest_record() converts POSIXt retrieved_at to ISO-8601 UTC", {
  rec <- make_record(retrieved_at = as.POSIXct("2026-06-09 12:34:56", tz = "UTC"))
  testthat::expect_equal(rec$retrieved_at, "2026-06-09T12:34:56Z")
})

testthat::test_that("input_manifest_record() strips NULL optional fields", {
  rec <- make_record()
  testthat::expect_false("description" %in% names(rec))
  testthat::expect_false("sha256" %in% names(rec))
})

testthat::test_that("input_manifest_record() retains supplied optional fields", {
  rec <- make_record(
    description = "An exemplary dataset",
    license = "CC0-1.0",
    citation = list(doi = "10.0/0", bibtex_key = "Example:2026")
  )
  testthat::expect_equal(rec$description, "An exemplary dataset")
  testthat::expect_equal(rec$license, "CC0-1.0")
  testthat::expect_equal(rec$citation$bibtex_key, "Example:2026")
})

testthat::test_that("input_manifest_record() rejects missing required fields", {
  testthat::expect_snapshot(
    input_manifest_record(id = "x", name = "X", source = list(type = "t"), local_path = "p"),
    error = TRUE
  )
})

testthat::test_that("input_manifest_record() rejects source without type or url", {
  testthat::expect_snapshot(
    input_manifest_record(id = "x", name = "X", local_path = "p", source = list(type = "t")),
    error = TRUE
  )
})

testthat::test_that("input_manifest_schema() returns the documented field set", {
  schema <- input_manifest_schema()
  required <- vapply(schema, function(s) identical(s$level, "required"), logical(1L))
  testthat::expect_setequal(
    names(schema)[required],
    c("id", "name", "source", "retrieved_at", "local_path")
  )
})

## Round-trip read/write ------------------------------------------------------------------------

testthat::test_that("write_input_manifest() + read_input_manifest() round-trips", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  records <- list(make_record(id = "alpha"), make_record(id = "beta", description = "two"))
  write_input_manifest(records, path)
  out <- read_input_manifest(path)

  testthat::expect_length(out, 2L)
  testthat::expect_equal(vapply(out, `[[`, character(1L), "id"), c("alpha", "beta"))
  testthat::expect_equal(out[[2]]$description, "two")
})

testthat::test_that("write_input_manifest() sorts records by id for stable diffs", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  records <- list(make_record(id = "gamma"), make_record(id = "alpha"), make_record(id = "beta"))
  write_input_manifest(records, path)
  out <- read_input_manifest(path)
  testthat::expect_equal(vapply(out, `[[`, character(1L), "id"), c("alpha", "beta", "gamma"))
})

testthat::test_that("write_input_manifest(keep_retrieved_at = TRUE) keeps an unchanged record's date", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  ## optional fields of several types, so the comparison must survive the JSON round trip
  rec <- function(retrieved_at, url = "https://example.com/data.csv") {
    make_record(
      id = "alpha",
      source = list(type = "http_download", url = url, shared_by = c("A", "B")),
      retrieved_at = retrieved_at,
      description = "a dataset",
      citation = list(bibtex_key = "Example:2026", external = FALSE),
      extra = list(licence = "UNSTATED", n = 3L, share = 0.123456)
    )
  }
  write_input_manifest(list(rec("2026-06-09T20:00:00Z")), path)
  write_input_manifest(
    list(
      rec("2026-10-30T00:00:00Z"),
      make_record(id = "beta", retrieved_at = "2026-10-30T00:00:00Z")
    ),
    path,
    keep_retrieved_at = TRUE
  )
  out <- read_input_manifest(path)
  testthat::expect_identical(
    vapply(out, `[[`, character(1L), "retrieved_at"),
    c("2026-06-09T20:00:00Z", "2026-10-30T00:00:00Z")
  )
})

testthat::test_that("write_input_manifest(keep_retrieved_at = TRUE) dates a changed record anew", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  write_input_manifest(list(make_record(id = "alpha")), path)
  changed <- make_record(
    id = "alpha",
    source = list(type = "http_download", url = "https://example.com/v2.csv"),
    retrieved_at = "2026-10-30T00:00:00Z"
  )
  write_input_manifest(list(changed), path, keep_retrieved_at = TRUE)
  testthat::expect_identical(read_input_manifest(path)[[1L]]$retrieved_at, "2026-10-30T00:00:00Z")
})

testthat::test_that("write_input_manifest() writes every record's date as given by default", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  write_input_manifest(list(make_record(id = "alpha")), path)
  write_input_manifest(list(make_record(id = "alpha", retrieved_at = "2026-10-30T00:00:00Z")), path)
  testthat::expect_identical(read_input_manifest(path)[[1L]]$retrieved_at, "2026-10-30T00:00:00Z")
})

testthat::test_that("read_input_manifest() returns empty list when file is absent", {
  out <- read_input_manifest(tempfile(fileext = ".json"))
  testthat::expect_equal(out, list())
})

testthat::test_that("read_input_manifest() ignores extra top-level fields", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  records <- list(make_record(id = "alpha"))
  write_input_manifest(records, path)

  raw <- jsonlite::read_json(path)
  testthat::expect_equal(raw$`@context`, "https://schema.org")
  testthat::expect_equal(raw$`@type`, "DataCatalog")
  testthat::expect_equal(raw$version, "1")
})

## register_input() upsert ---------------------------------------------------------------------

testthat::test_that("register_input() creates the manifest when absent", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  register_input(make_record(id = "alpha"), path)
  out <- read_input_manifest(path)
  testthat::expect_equal(vapply(out, `[[`, character(1L), "id"), "alpha")
})

testthat::test_that("register_input() upserts existing records by id (no duplicates)", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  register_input(make_record(id = "alpha", description = "v1"), path)
  register_input(make_record(id = "beta"), path)
  register_input(make_record(id = "alpha", description = "v2"), path)

  out <- read_input_manifest(path)
  testthat::expect_length(out, 2L)
  alpha <- out[[which(vapply(out, `[[`, character(1L), "id") == "alpha")]]
  testthat::expect_equal(alpha$description, "v2")
})

testthat::test_that("register_input() rejects invalid records", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")

  path <- withr::local_tempfile(fileext = ".json")
  bad <- list(id = "x", name = "X") ## missing source, local_path, retrieved_at
  testthat::expect_snapshot(register_input(bad, path), error = TRUE)
})
