test_that("archive_extract_once extracts, then skips re-extraction", {
  a <- system.file("extdata", "data.zip", package = "archive")
  d <- withr::local_tempdir()

  f1 <- archive_extract_once(a, d, "iris.csv")
  expect_contains(list.files(d), "iris.csv")

  ## already present -> returns the same path without re-extracting
  f2 <- archive_extract_once(a, d, "iris.csv")
  expect_identical(f1, f2)
})

test_that("unzip fallback extracts a zip", {
  skip_if(!nzchar(Sys.which("unzip")))
  a <- system.file("extdata", "data.zip", package = "archive")
  d <- withr::local_tempdir()

  .extract_unzip_fallback(a, d, "airquality.csv", simpleError("forced"))
  expect_contains(list.files(d), "airquality.csv")
})

test_that("fallback re-throws when it cannot handle the archive", {
  expect_snapshot(
    error = TRUE,
    .extract_unzip_fallback("foo.tar.gz", tempdir(), NULL, simpleError("boom"))
  )
})

test_that("archive_extract_once re-extracts a TRUNCATED file rather than trusting existence", {
  ## The failure this guards against: an interrupted extraction leaves a short file, and an
  ## existence-only check then treats it as done forever. LandWeb lost ~70% of a 1.88 GB fire
  ## perimeter shapefile this way, silently.
  a <- system.file("extdata", "data.zip", package = "archive")
  d <- withr::local_tempdir()

  archive_extract_once(a, d, "iris.csv")
  full <- file.size(file.path(d, "iris.csv"))

  ## simulate the interrupted extraction
  con <- file(file.path(d, "iris.csv"), "r+b")
  truncate(con, 10)
  close(con)
  expect_lt(file.size(file.path(d, "iris.csv")), full)

  archive_extract_once(a, d, "iris.csv")
  expect_identical(file.size(file.path(d, "iris.csv")), full)
})

test_that("archive_extract_once skips when files are complete, including files = NULL", {
  a <- system.file("extdata", "data.zip", package = "archive")
  d <- withr::local_tempdir()

  archive_extract_once(a, d)
  before <- file.mtime(list.files(d, full.names = TRUE))

  ## everything is present at full size -> nothing is re-extracted
  archive_extract_once(a, d)
  expect_identical(file.mtime(list.files(d, full.names = TRUE)), before)
})

test_that("archive_extract_once errors when extraction leaves a file short", {
  a <- system.file("extdata", "data.zip", package = "archive")
  d <- withr::local_tempdir()
  manifest <- archive::archive(a)

  ## a manifest claiming a bigger file than the archive holds: extraction can never satisfy it
  manifest$size[manifest$path == "iris.csv"] <- 1e9
  local_mocked_bindings(archive = function(...) manifest, .package = "archive")

  expect_error(archive_extract_once(a, d, "iris.csv"), "extraction incomplete")
})
