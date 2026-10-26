#' Download a file, skipping if file already exists locally
#'
#' @param url character, specifying a URL to a file
#'
#' @param destfile character, specifying a local destination file path
#'
#' @param ... additional parameters passed to `download.file`
#'
#' @details
#' The file is downloaded to a temporary file beside `destfile` (`.partial-<random>-<name>`),
#' which is renamed to `destfile` only once the download has finished. Downloading straight into
#' `destfile` let a concurrent caller find a half-written file, skip its own download and use it,
#' and a failed download left a partial file that every later call then skipped. Both paths are in
#' one directory, so the rename is atomic. The temporary file is removed if the download fails; one
#' left by a killed process is never mistaken for the download, and can be deleted.
#'
#' @export
#' @seealso [drive_download_once()]
download_once <- function(url, destfile, ...) {
  if (!file.exists(destfile)) {
    partial <- .partial_path(destfile)
    on.exit(unlink(partial, force = TRUE), add = TRUE)
    status <- utils::download.file(url = url, destfile = partial, ...)
    ## the "wget" and "curl" methods report a failure as a non-zero status and a warning
    if (identical(as.integer(status), 0L)) {
      .move_into_place(partial, destfile)
    }
    invisible(status)
  }
}

#' Download a file from Google Drive, skipping if file already exists locally
#'
#' @inheritParams googledrive::drive_download
#'
#' @param ... additional parameters passed to [utils::download.file]
#'
#' @details
#' As in [download_once()], the file is downloaded to a temporary file beside `path`, which is
#' renamed into place only once the download has finished. The temporary name ends with the name
#' of `path`, so googledrive still reads the export type from its extension.
#'
#' @export
#' @seealso [download_once()]
drive_download_once <- function(file, path, ...) {
  if (!file.exists(path)) {
    partial <- .partial_path(path)
    on.exit(unlink(partial, force = TRUE), add = TRUE)
    res <- googledrive::drive_download(file = file, path = partial, ...)
    ## googledrive appends an extension when it exports a native Google file to a path without one
    landed <- res[["local_path"]]
    dest <- paste0(path, substring(landed, nchar(partial) + 1L))
    .move_into_place(landed, dest)
    res[["local_path"]] <- dest
    invisible(res)
  }
}

#' Download multiple files/folders from Google Drive
#'
#' Based on <https://github.com/tidyverse/googledrive/issues/123#issuecomment-563484927>.
#'
#' Recursively download the contents of a folder from Google Drive.
#'
#' @param drive_folder The folder on Google Drive, given as a URL, file id, or dribble.
#'
#' @param path The local destination folder.
#'
#' @param batch_size the maximum number of files per upload batch.
#'   Downloads are done in batches to mitigate curl handle errors.
#'
#' @param overwrite logical indicating whether to overwrite local files
#'
#' @return Invisibly, a dribble of the downloaded files (not directories).
#'
#' @export
drive_download_folder <- function(drive_folder, path, batch_size = 10, overwrite = TRUE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  ## avoid curl HTTP2 framing layer error (see .use_http11):
  ## per https://github.com/tidyverse/googlesheets4/issues/233#issuecomment-889376499
  .use_http11()

  drive_dirs <- googledrive::drive_ls(drive_folder, recursive = FALSE, type = "folder")
  drive_files <- googledrive::drive_ls(drive_folder, recursive = FALSE) |>
    dplyr::filter(!id %in% drive_dirs[["id"]])

  ## download the files in the current directory
  downloaded_files <- NULL
  if (nrow(drive_files) > 0) {
    g <- seq(nrow(drive_files)) %/% batch_size
    downloaded_files <- drive_files |>
      split(g) |>
      lapply(function(x) {
        purrr::map2_dfr(
          .x = x$id,
          .y = x$name,
          .f = ~ googledrive::drive_download(.x, file.path(path, .y), overwrite = overwrite)
        )
      }) |>
      dplyr::bind_rows()
  }

  ## recursively download directories
  if (nrow(drive_dirs) > 0) {
    purrr::map2_dfr(
      .x = drive_dirs$id,
      .y = drive_dirs$name,
      .f = ~ drive_download_folder(
        .x,
        file.path(path, .y),
        batch_size = batch_size,
        overwrite = overwrite
      )
    ) |>
      dplyr::bind_rows(downloaded_files) |>
      invisible() ## return a dribble of what's been uploaded
  } else {
    downloaded_files
  }
}

## A unique temporary path for a download in progress. It is in the same directory as `path`, so
## `file.rename()` into place stays on one filesystem and is atomic (NFS included), and it ends with
## `basename(path)`, keeping the extension that googledrive reads the export type from.
.partial_path <- function(path) {
  file.path(dirname(path), paste0(".partial-", basename(tempfile("")), "-", basename(path)))
}

## Move a finished download into place. `file.rename()` replaces a `to` that appeared meanwhile,
## which is fine: if another caller finished the same download first, both copies are complete.
.move_into_place <- function(from, to) {
  if (!file.rename(from, to)) {
    stop("could not move the download ", from, " into place at ", to, call. = FALSE)
  }
  invisible(to)
}
