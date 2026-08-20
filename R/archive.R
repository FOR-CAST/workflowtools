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
#' extracted" forever. After extracting, the result is verified against the archive
#' manifest and an error is raised if anything is still missing or short, so a partial
#' extraction fails loudly instead of being silently reused.
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
  manifest <- archive::archive(archive)

  if (is.numeric(files)) {
    ## get filenames from position indices
    files <- manifest |> dplyr::slice(files) |> dplyr::pull(path)
  }
  wanted <- if (is.null(files)) manifest[["path"]] else files

  incomplete <- .archive_incomplete(manifest, dir, wanted)
  extract <- isTRUE(force) || length(incomplete) > 0L

  if (isTRUE(extract)) {
    f <- tryCatch(
      archive::archive_extract(archive = archive, dir = dir, files = files, ...),
      error = function(e) .extract_unzip_fallback(archive, dir, files, e)
    )

    ## Verify what actually landed. An interrupted extraction leaves a SHORT file behind, and
    ## because the previous skip-test was `file.exists()` only, every later call saw the stub and
    ## skipped -- so the truncation became permanent and silent. This bit LandWeb: a 1.88 GB NBAC
    ## fire-perimeter .shp was left at 567 MB, GDAL logged 74,178 read errors, `sf::st_read()` still
    ## returned the full feature count (the .shx index was intact) and the pipeline completed with
    ## historic fire summaries built from ~30% of the record.
    still <- .archive_incomplete(manifest, dir, wanted)
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
  } else {
    f <- wanted
  }

  return(fs::path(dir, f))
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
