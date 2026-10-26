## The mocks stand in for the transfer. Each writes into the path it is given; a failing one writes
## part of the file first, which is what an interrupted transfer leaves behind.

## Everything in `dir`, dotfiles included, so a leftover `.partial-*` file shows up.
dir_contents <- function(dir) list.files(dir, all.files = TRUE, no.. = TRUE)

test_that("download_once() leaves nothing behind when the download fails", {
  ## Downloading straight into `destfile` left the partial file in place, and every later call
  ## then skipped the download because the file existed.
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("partial", destfile)
      stop("transfer interrupted")
    },
    .package = "utils"
  )

  expect_error(download_once("https://example.org/landbase.zip", dest), "transfer interrupted")
  expect_false(file.exists(dest))
  expect_length(dir_contents(d), 0L)
})

test_that("download_once() does not install a download that returned a non-zero status", {
  ## the "wget" and "curl" methods report failure as a status and a warning, not an error
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("partial", destfile)
      warning("download had nonzero exit status")
      invisible(8L)
    },
    .package = "utils"
  )

  expect_warning(status <- download_once("https://example.org/landbase.zip", dest), "nonzero")
  expect_identical(status, 8L)
  expect_false(file.exists(dest))
  expect_length(dir_contents(d), 0L)
})

test_that("download_once() downloads again after a failed attempt", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("partial", destfile)
      stop("transfer interrupted")
    },
    .package = "utils"
  )
  expect_error(download_once("https://example.org/landbase.zip", dest))

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("complete", destfile)
      invisible(0L)
    },
    .package = "utils"
  )
  download_once("https://example.org/landbase.zip", dest)
  expect_identical(readLines(dest), "complete")
})

test_that("download_once() moves a complete download into place", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  seen <- NULL
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      seen <<- list(url = url, dots = list(...))
      writeLines("complete", destfile)
      invisible(0L)
    },
    .package = "utils"
  )

  status <- download_once("https://example.org/landbase.zip", dest, quiet = TRUE, mode = "wb")
  expect_identical(status, 0L)
  expect_identical(readLines(dest), "complete")
  expect_identical(dir_contents(d), "landbase.zip")
  expect_identical(
    seen,
    list(url = "https://example.org/landbase.zip", dots = list(quiet = TRUE, mode = "wb"))
  )
})

test_that("download_once() never exposes a half-written destination", {
  ## A second caller that found the destination mid-download skipped its own download and
  ## extracted the half-written zip (`unzip` status 9, no central directory).
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  visible_mid_download <- NA
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("partial", destfile)
      visible_mid_download <<- file.exists(dest)
      writeLines("complete", destfile)
      invisible(0L)
    },
    .package = "utils"
  )

  download_once("https://example.org/landbase.zip", dest)
  expect_false(visible_mid_download)
  expect_identical(readLines(dest), "complete")
})

test_that("download_once() downloads to a unique temporary file beside the destination", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  used <- character(0)
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      used <<- c(used, destfile)
      stop("transfer interrupted")
    },
    .package = "utils"
  )

  expect_error(download_once("https://example.org/landbase.zip", dest))
  expect_error(download_once("https://example.org/landbase.zip", dest))

  expect_length(used, 2L)
  expect_identical(dirname(used), rep(d, 2L))
  expect_true(all(startsWith(basename(used), ".partial-")))
  expect_true(all(endsWith(basename(used), "-landbase.zip")))
  expect_identical(tools::file_ext(used), rep("zip", 2L))
  expect_false(used[1] == used[2])
})

test_that("download_once() skips a destination that already exists", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  writeLines("KEEP", dest)
  local_mocked_bindings(
    download.file = function(url, destfile, ...) stop("should not download"),
    .package = "utils"
  )

  expect_no_error(download_once("https://example.org/landbase.zip", dest))
  expect_identical(readLines(dest), "KEEP")
})

test_that("drive_download_once() leaves nothing behind when the download fails", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  local_mocked_bindings(
    drive_download = function(file, path, ...) {
      writeLines("partial", path)
      stop("Download failed.")
    },
    .package = "googledrive"
  )

  expect_error(drive_download_once("drive-file-id", dest), "Download failed")
  expect_false(file.exists(dest))
  expect_length(dir_contents(d), 0L)
})

test_that("drive_download_once() moves a complete download into place", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  seen <- NULL
  local_mocked_bindings(
    drive_download = function(file, path, ...) {
      seen <<- list(file = file, dots = list(...))
      writeLines("complete", path)
      invisible(data.frame(name = "landbase.zip", local_path = path))
    },
    .package = "googledrive"
  )

  res <- drive_download_once("drive-file-id", dest, type = "zip")
  expect_identical(readLines(dest), "complete")
  expect_identical(dir_contents(d), "landbase.zip")
  expect_identical(res$local_path, dest)
  expect_identical(seen, list(file = "drive-file-id", dots = list(type = "zip")))
})

test_that("drive_download_once() never exposes a half-written destination", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  visible_mid_download <- NA
  local_mocked_bindings(
    drive_download = function(file, path, ...) {
      writeLines("partial", path)
      visible_mid_download <<- file.exists(dest)
      writeLines("complete", path)
      invisible(data.frame(name = "landbase.zip", local_path = path))
    },
    .package = "googledrive"
  )

  drive_download_once("drive-file-id", dest)
  expect_false(visible_mid_download)
  expect_identical(readLines(dest), "complete")
})

test_that("drive_download_once() downloads to a temporary file beside the destination", {
  ## googledrive reads the export type from the extension of `path`, so the temporary name must
  ## end with the destination's name
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  used <- NULL
  local_mocked_bindings(
    drive_download = function(file, path, ...) {
      used <<- path
      stop("Download failed.")
    },
    .package = "googledrive"
  )

  expect_error(drive_download_once("drive-file-id", dest))
  expect_identical(dirname(used), d)
  expect_true(startsWith(basename(used), ".partial-"))
  expect_true(endsWith(basename(used), "-landbase.zip"))
  expect_identical(tools::file_ext(used), "zip")
})

test_that("drive_download_once() keeps an extension googledrive appends", {
  ## exporting a native Google file to a path without an extension, googledrive appends one
  d <- withr::local_tempdir()
  dest <- file.path(d, "species")
  local_mocked_bindings(
    drive_download = function(file, path, ...) {
      path <- paste0(path, ".csv")
      writeLines("complete", path)
      invisible(data.frame(name = "species", local_path = path))
    },
    .package = "googledrive"
  )

  res <- drive_download_once("drive-file-id", dest)
  expect_identical(res$local_path, paste0(dest, ".csv"))
  expect_identical(dir_contents(d), "species.csv")
})

test_that("drive_download_once() skips a destination that already exists", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "landbase.zip")
  writeLines("KEEP", dest)
  local_mocked_bindings(
    drive_download = function(file, path, ...) stop("should not download"),
    .package = "googledrive"
  )

  expect_no_error(drive_download_once("drive-file-id", dest))
  expect_identical(readLines(dest), "KEEP")
})
