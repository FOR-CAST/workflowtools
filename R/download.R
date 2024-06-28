utils::globalVariables(c(
  "id"
))

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

  ## avoid curl HTTP2 framing layer error:
  ## per https://github.com/tidyverse/googlesheets4/issues/233#issuecomment-889376499
  old <- httr::set_config(httr::config(http_version = 2)) ## corresponds to CURL_HTTP_VERSION_1_1
  on.exit(httr::set_config(old), add = TRUE)

  drive_dirs <- drive_ls(drive_folder, recursive = FALSE, type = "folder")
  drive_files <- drive_ls(drive_folder, recursive = FALSE) |>
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
          .f = ~ drive_download(.x, file.path(path, .y), overwrite = overwrite)
        )
      }) |>
      dplyr::bind_rows()
  }

  ## recursively download directories
  if (nrow(drive_dirs) > 0) {
    purrr::map2_dfr(
      .x = drive_dirs$id,
      .y = drive_dirs$name,
      .f = ~ drive_download_folder(.x, file.path(path, .y), batch_size = batch_size, overwrite = overwrite)
    ) |>
      dplyr::bind_rows(downloaded_files) |>
      invisible() ## return a dribble of what's been uploaded
  } else {
    downloaded_files
  }
}
