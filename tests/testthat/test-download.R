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
  ## against dirname(dest), not `d`: on Windows dirname() rewrites the separators
  expect_identical(dirname(used), rep(dirname(dest), 2L))
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
  expect_identical(dirname(used), dirname(dest))
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

## A fake Drive for drive_download_folder(): `tree` maps a folder id to its entries, a data frame of
## id, name, and md5 (NA for a folder or a Google-native file); `folder` marks the subfolders.
## drive_ls() answers from it and drive_download() writes the file id into the file, recording each
## id it is asked for in `env$downloaded`.
local_fake_drive <- function(tree, env, frame = parent.frame()) {
  env$downloaded <- character()
  local_mocked_bindings(
    drive_ls = function(path = NULL, pattern = NULL, recursive = FALSE, ..., type = NULL) {
      x <- tree[[path]]
      if (identical(type, "folder")) {
        x <- x[x$folder, , drop = FALSE]
      }
      res <- data.frame(id = x$id, name = x$name)
      ## Drive gives an md5Checksum for stored files only, not for folders or Google-native files
      res$drive_resource <- Map(
        function(m, f) {
          if (f) {
            list(mimeType = "application/vnd.google-apps.folder")
          } else if (is.na(m)) {
            list(mimeType = "application/vnd.google-apps.document")
          } else {
            list(md5Checksum = m)
          }
        },
        x$md5,
        x$folder
      )
      res
    },
    drive_download = function(file, path = NULL, ..., overwrite = FALSE) {
      env$downloaded <- c(env$downloaded, file)
      writeLines(file, path)
      invisible(data.frame(id = file, name = basename(path), local_path = path))
    },
    .package = "googledrive",
    .env = frame
  )
}

entries <- function(id, name, md5 = NA_character_, folder = FALSE) {
  data.frame(id = id, name = name, md5 = md5, folder = folder)
}

## The md5 of each string as drive_download() above writes it (one line).
md5_of <- function(text) {
  vapply(
    text,
    function(x) {
      f <- withr::local_tempfile()
      writeLines(x, f)
      unname(tools::md5sum(f))
    },
    character(1),
    USE.NAMES = FALSE
  )
}

test_that("drive_download_folder(skip_unchanged = TRUE) skips a file whose md5 matches", {
  d <- withr::local_tempdir()
  writeLines("fa", file.path(d, "a.txt"))
  env <- new.env()
  local_fake_drive(list(root = entries("fa", "a.txt", md5_of("fa"))), env)

  drive_download_folder("root", d, skip_unchanged = TRUE)
  expect_identical(env$downloaded, character())
  expect_identical(readLines(file.path(d, "a.txt")), "fa")
})

test_that("drive_download_folder(skip_unchanged = TRUE) downloads a changed file and a missing one", {
  d <- withr::local_tempdir()
  writeLines("old", file.path(d, "b.txt"))
  env <- new.env()
  local_fake_drive(
    list(root = entries(c("fb", "fc"), c("b.txt", "c.txt"), md5_of(c("fb", "fc")))),
    env
  )

  drive_download_folder("root", d, skip_unchanged = TRUE)
  expect_identical(env$downloaded, c("fb", "fc"))
  expect_identical(readLines(file.path(d, "b.txt")), "fb")
})

test_that("drive_download_folder(skip_unchanged = TRUE) always downloads a file without an md5", {
  d <- withr::local_tempdir()
  writeLines("fdoc", file.path(d, "notes"))
  env <- new.env()
  local_fake_drive(list(root = entries("fdoc", "notes")), env)

  drive_download_folder("root", d, skip_unchanged = TRUE)
  expect_identical(env$downloaded, "fdoc")
})

test_that("drive_download_folder(skip_unchanged = TRUE) skips unchanged files in subfolders too", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "sub"))
  writeLines("fd", file.path(d, "sub", "d.txt"))
  env <- new.env()
  tree <- list(
    root = entries("dsub", "sub", folder = TRUE),
    dsub = entries("fd", "d.txt", md5_of("fd"))
  )
  local_fake_drive(tree, env)

  drive_download_folder("root", d, skip_unchanged = TRUE)
  expect_identical(env$downloaded, character())
})

test_that("drive_download_folder() downloads every file by default", {
  d <- withr::local_tempdir()
  writeLines("fa", file.path(d, "a.txt"))
  env <- new.env()
  local_fake_drive(list(root = entries("fa", "a.txt", md5_of("fa"))), env)

  drive_download_folder("root", d)
  expect_identical(env$downloaded, "fa")
})

test_that("drive_download_folder(files =) downloads only the named files", {
  d <- withr::local_tempdir()
  env <- new.env()
  local_fake_drive(
    list(
      root = entries(
        c("fa", "fb", "fz"),
        c("a.txt", "b.txt", "big.zip"),
        md5_of(c("fa", "fb", "fz"))
      )
    ),
    env
  )

  drive_download_folder("root", d, files = c("a.txt", "b.txt"))
  expect_identical(env$downloaded, c("fa", "fb"))
  expect_false(file.exists(file.path(d, "big.zip")))
})

test_that("drive_download_folder(files =) names the files the folder lacks, and downloads nothing", {
  ## a caller must not go on to read an old local copy of a file removed from Drive
  d <- withr::local_tempdir()
  writeLines("old", file.path(d, "gone.rds"))
  env <- new.env()
  local_fake_drive(list(root = entries("fa", "a.txt", md5_of("fa"))), env)

  err <- expect_error(drive_download_folder("root", d, files = c("a.txt", "gone.rds")))
  expect_match(conditionMessage(err), "gone.rds", fixed = TRUE)
  expect_match(conditionMessage(err), "root", fixed = TRUE)
  expect_identical(env$downloaded, character())
})

test_that("drive_download_folder(files =) does not search subfolders", {
  d <- withr::local_tempdir()
  env <- new.env()
  tree <- list(
    root = rbind(entries("fa", "a.txt", md5_of("fa")), entries("dsub", "sub", folder = TRUE)),
    dsub = entries("fd", "d.txt", md5_of("fd"))
  )
  local_fake_drive(tree, env)

  drive_download_folder("root", d, files = "a.txt")
  expect_identical(env$downloaded, "fa")
  expect_false(dir.exists(file.path(d, "sub")))
})

test_that("drive_download_folder(files =) refuses a name the folder holds twice", {
  d <- withr::local_tempdir()
  env <- new.env()
  local_fake_drive(
    list(root = entries(c("f1", "f2"), c("a.txt", "a.txt"), md5_of(c("f1", "f2")))),
    env
  )

  expect_error(drive_download_folder("root", d, files = "a.txt"), "more than one", fixed = TRUE)
  expect_identical(env$downloaded, character())
})

test_that("drive_download_folder(files =, skip_unchanged = TRUE) skips a named file that has not changed", {
  d <- withr::local_tempdir()
  writeLines("fa", file.path(d, "a.txt"))
  env <- new.env()
  local_fake_drive(
    list(root = entries(c("fa", "fb"), c("a.txt", "b.txt"), md5_of(c("fa", "fb")))),
    env
  )

  drive_download_folder("root", d, files = c("a.txt", "b.txt"), skip_unchanged = TRUE)
  expect_identical(env$downloaded, "fb")
})
