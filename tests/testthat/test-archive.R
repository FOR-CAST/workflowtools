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
