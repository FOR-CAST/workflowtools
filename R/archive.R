#' Extract an archive, skipping if previously extracted
#'
#' @inheritParams archive::archive_extract
#'
#' @param ... additional parameters passed to [archive::archive_extract]
#'
#' @param force logical, whether to extract and overwrite existing files
#'
#' @details
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
  extract <- if (isTRUE(force)) {
    TRUE
  } else {
    if (is.null(files)) {
      TRUE
    } else {
      if (is.numeric(files)) {
        ## get filenames from position indices
        files <- archive::archive(archive) |> dplyr::slice(files) |> dplyr::pull(path)
      }
      !all(file.exists(file.path(dir, files)))
    }
  }

  if (isTRUE(extract)) {
    f <- tryCatch(
      archive::archive_extract(archive = archive, dir = dir, files = files, ...),
      error = function(e) .extract_unzip_fallback(archive, dir, files, e)
    )
  } else {
    if (is.null(files)) {
      f <- archive::archive(archive) |> dplyr::pull(path)
    } else if (is.character(files)) {
      f <- archive::archive(archive) |> dplyr::filter(path %in% files) |> dplyr::pull(path)
    }
  }

  return(fs::path(dir, f))
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
