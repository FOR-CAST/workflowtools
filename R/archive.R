#' Extract an archive, skipping if previously extracted
#'
#' @inheritParams archive::archive_extract
#'
#' @param ... additional parameters passed to [archive::archive_extract]
#'
#' @param force logical, whether to extract and overwrite existing files
#'
#' @details
#' Extraction is skipped only when every wanted file is already on disk **at the size
#' the archive says it should be**. Checking existence alone is not enough: an
#' interrupted extraction leaves a short file behind, which then looks "already
#' extracted" forever.
#'
#' Extraction goes into a staging directory (a child of `dir`, removed on exit) and is
#' verified against the archive manifest there; only then are the members moved into
#' `dir`. An error is raised if anything is still missing or short, so a partial
#' extraction fails loudly instead of being silently reused -- and, because nothing is
#' moved until the whole set verifies, a failure part-way through can no longer leave a
#' truncated file in `dir`. That matters because a single short member makes the next
#' call re-extract the *whole* archive, so extracting in place turns one interrupted run
#' into a permanent loop. Note this needs room for a second copy of the extracted
#' members while the staging directory exists.
#'
#' `libarchive` (via [archive::archive_extract]) can fail on very large `ZIP64`
#' archives (e.g. with a "Truncated input file" error). When extraction fails and
#' the archive is a `.zip`, `archive_extract_once()` falls back to the system
#' `unzip` (Info-ZIP), which handles `ZIP64` and selective extraction.
#'
#' @returns character, the filenames extracted (invisibly)
#'
#' @examples
#' a <- system.file(package = "archive", "extdata", "data.zip")
#' d <- tempfile()
#' f <- c("iris.csv", "airquality.csv")
#' i <- c(1L, 3L)
#'
#' archive_extract_once(a, d)
#' list.files(d, full.names = TRUE)
#' archive_extract_once(a, d) ## does not re-extract files
#' unlink(d)
#'
#' d <- tempfile()
#' archive_extract_once(a, d, f)
#' list.files(d, full.names = TRUE)
#' archive_extract_once(a, d, f) ## does not re-extract files
#' unlink(d)
#'
#' d <- tempfile()
#' archive_extract_once(a, d, i)
#' list.files(d, full.names = TRUE)
#' archive_extract_once(a, d, i) ## does not re-extract files
#' unlink(d)
#'
#' @export
archive_extract_once <- function(archive, dir = ".", files = NULL, ..., force = FALSE) {
  ## `archive::archive()` and `archive::archive_extract()` open the archive with
  ## `file(path, "rb")`, which takes its `encoding` from `getOption("encoding")`. Under
  ## `options(encoding = "UTF-8")` a re-encoding layer is applied to a BINARY stream,
  ## libarchive misparses the ZIP, and every member's recorded size comes back as 0.
  ## Sizes are exactly what this function compares against, so under that option EVERY file
  ## looks short: the archive is re-extracted on every single call, and the verification
  ## afterwards can never pass no matter what landed on disk. `options(encoding = "UTF-8")`
  ## is in force inside a SpaDES run, which is why this only ever bit in the pipeline and
  ## never in a standalone test. Force a binary-safe encoding for the duration.
  old_enc <- options(encoding = "native.enc")
  on.exit(options(old_enc), add = TRUE)

  manifest <- archive::archive(archive)

  if (is.numeric(files)) {
    ## get filenames from position indices
    files <- manifest |> dplyr::slice(files) |> dplyr::pull(path)
  }
  wanted <- if (is.null(files)) manifest[["path"]] else files

  incomplete <- .archive_incomplete(manifest, dir, wanted)
  extract <- isTRUE(force) || length(incomplete) > 0L

  if (isTRUE(extract)) {
    fs::dir_create(dir)

    ## Extract into a STAGING directory and only move the result into `dir` once it verifies.
    ## `archive_extract()` opens each member `O_WRONLY|O_TRUNC`, so extracting straight into
    ## `dir` destroys the existing copy the instant it starts -- and any failure part-way (an
    ## error, an interrupt, a killed worker) leaves a truncated file behind. Because ONE short
    ## member makes `.archive_incomplete()` re-extract the WHOLE archive, that is self-sustaining:
    ## every later call restarts the extraction and dies at the same point. Staging makes the
    ## operation all-or-nothing from `dir`'s point of view: a good file is only ever replaced by
    ## a verified one. The staging dir is a child of `dir` so the moves stay on one filesystem
    ## (`file.rename()` is then atomic per file), and it is removed on exit either way.
    staging <- file.path(dir, sprintf(".archive_extract_once-%s", basename(tempfile(""))))
    fs::dir_create(staging)
    on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

    f <- tryCatch(
      archive::archive_extract(archive = archive, dir = staging, files = files, ...),
      error = function(e) .extract_unzip_fallback(archive, staging, files, e)
    )

    ## Verify what actually landed, IN STAGING, before anything is moved into place. An
    ## interrupted extraction leaves a SHORT file behind, and because the original skip-test was
    ## `file.exists()` only, every later call saw the stub and skipped -- so the truncation became
    ## permanent and silent. This bit LandWeb: a 1.88 GB NBAC fire-perimeter .shp was left at
    ## 567 MB, GDAL logged 74,178 read errors, `sf::st_read()` still returned the full feature
    ## count (the .shx index was intact) and the pipeline completed with historic fire summaries
    ## built from ~30% of the record.
    still <- .archive_incomplete(manifest, staging, wanted)
    if (length(still) > 0L) {
      stop(
        "archive_extract_once(): extraction incomplete for ",
        length(still),
        " file(s) from ",
        archive,
        ":\n  ",
        paste(utils::head(still, 5L), collapse = "\n  "),
        call. = FALSE
      )
    }

    .archive_move_into_place(staging, dir, wanted)
  } else {
    f <- wanted
  }

  return(fs::path(dir, f))
}

## Move verified members from the staging directory into their final home, replacing whatever is
## there. `file.rename()` is atomic within a filesystem, and staging is a child of `to_dir`, so
## each destination goes from old-and-good to new-and-good with no truncated window. The whole set
## is not atomic -- a crash mid-move can leave some members new and some old -- but every
## individual file is always a COMPLETE file, which is the property that matters here: the failure
## mode being fixed is a half-written 1.88 GB shapefile, not a mixed-vintage set. `file.rename()`
## is documented to fail across filesystems, so fall back to copy-then-remove if that ever happens
## (it should not, given where staging lives).
.archive_move_into_place <- function(from_dir, to_dir, wanted) {
  is_dir <- grepl("/$", wanted)
  dirs <- wanted[is_dir]
  if (length(dirs)) {
    fs::dir_create(file.path(to_dir, dirs))
  }

  rel <- wanted[!is_dir]
  if (!length(rel)) {
    return(invisible(character(0)))
  }
  src <- file.path(from_dir, rel)
  dst <- file.path(to_dir, rel)
  fs::dir_create(unique(dirname(dst))) ## members may sit in subdirectories of the archive

  ok <- suppressWarnings(file.rename(src, dst))
  if (!all(ok)) {
    copied <- file.copy(src[!ok], dst[!ok], overwrite = TRUE)
    if (!all(copied)) {
      stop(
        "archive_extract_once(): could not move ",
        sum(!copied),
        " extracted file(s) into ",
        to_dir,
        ":\n  ",
        paste(utils::head(rel[!ok][!copied], 5L), collapse = "\n  "),
        call. = FALSE
      )
    }
    unlink(src[!ok])
  }

  invisible(dst)
}

## Which of `wanted` are missing on disk, or present but not the size the archive says they should
## be? Directory entries carry no content, so they are existence-checked only. Returns a character
## vector of offending paths (empty when everything is complete).
.archive_incomplete <- function(manifest, dir, wanted) {
  size <- stats::setNames(manifest[["size"]], manifest[["path"]])
  on_disk <- file.path(dir, wanted)
  expected <- unname(size[wanted])
  actual <- file.size(on_disk)

  is_dir <- grepl("/$", wanted)
  bad <- ifelse(
    is_dir,
    !dir.exists(on_disk),
    is.na(actual) | (!is.na(expected) & actual != expected)
  )

  wanted[bad]
}

## libarchive can choke on very large ZIP64 archives; fall back to the system
## `unzip` (Info-ZIP), which handles ZIP64 and selective extraction. Re-throws
## the original error if the archive is not a .zip or `unzip` is unavailable.
.extract_unzip_fallback <- function(archive, dir, files, err) {
  if (!grepl("\\.zip$", archive, ignore.case = TRUE) || !nzchar(Sys.which("unzip"))) {
    stop(err)
  }
  fs::dir_create(dir)
  args <- c("-o", shQuote(archive))
  if (!is.null(files)) {
    args <- c(args, shQuote(files))
  }
  args <- c(args, "-d", shQuote(dir))
  status <- system2("unzip", args, stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(status), 0L)) {
    stop("`unzip` fallback failed (status ", status, ") for archive: ", archive)
  }
  if (is.null(files)) {
    ## list the archive members (Info-ZIP)
    system2("unzip", c("-Z1", shQuote(archive)), stdout = TRUE, stderr = FALSE)
  } else {
    files
  }
}
