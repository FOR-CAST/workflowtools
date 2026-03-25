#' Extract an archive, skipping if previously extracted
#'
#' @inheritParams archive::archive_extract
#'
#' @param ... additional parameters passed to [archive::archive_extract]
#'
#' @param force logical, whether to extract and overwrite existing files
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
#' @importFrom archive archive_extract
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
    f <- archive::archive_extract(archive = archive, dir = dir, files = files, ...)
  } else {
    if (is.null(files)) {
      f <- archive::archive(archive) |> dplyr::pull(path)
    } else if (is.character(files)) {
      f <- archive::archive(archive) |> dplyr::filter(path %in% files) |> dplyr::pull(path)
    }
  }

  return(fs::path(dir, f))
}
