## Minimal fakes for the Microsoft Graph `ms_drive_item` interface exercised by
## the teams_* helpers. They record what was asked of them so tests can assert
## behaviour without a live connection.

## Fake channel-root folder for teams_download_folder(). `tree` maps a remote
## directory path to a data.frame(name, isdir, size) listing its immediate
## children; the root listing uses the key "<root>". A directory with no entry
## in `tree` lists as empty.
fake_download_folder <- function(tree) {
  empty <- data.frame(
    name = character(0),
    isdir = logical(0),
    size = numeric(0),
    stringsAsFactors = FALSE
  )
  folder <- new.env(parent = emptyenv())
  folder$list_files <- function(path = "", full_names = FALSE) {
    key <- if (nzchar(path)) path else "<root>"
    df <- tree[[key]]
    if (is.null(df)) empty else df
  }
  folder$get_item <- function(path) {
    item <- new.env(parent = emptyenv())
    item$download <- function(dest, overwrite = TRUE) {
      writeLines(paste("content of", path), dest)
      invisible(TRUE)
    }
    item
  }
  folder
}

## Fake destination folder for teams_upload_files(); records the dest names it
## was asked to upload.
fake_upload_target <- function() {
  target <- new.env(parent = emptyenv())
  target$uploaded <- character(0)
  target$upload <- function(src, dest = basename(src), ...) {
    target$uploaded <- c(target$uploaded, dest)
    invisible(TRUE)
  }
  target
}

## Fake drive for teams_ensure_folder(): get_item() errors on a missing path
## (as Microsoft Graph does), create_folder() records and adds the path.
fake_drive <- function(existing = character(0)) {
  drive <- new.env(parent = emptyenv())
  drive$tree <- existing
  drive$created <- character(0)

  make_item <- function(path) {
    e <- new.env(parent = emptyenv())
    e$path <- path
    e$create_folder <- function(name) {
      full <- if (nzchar(path)) paste(path, name, sep = "/") else name
      drive$tree <- c(drive$tree, full)
      drive$created <- c(drive$created, full)
      make_item(full)
    }
    e
  }

  drive$get_item <- function(path) {
    if (path %in% drive$tree) make_item(path) else stop("item not found: ", path)
  }
  drive$create_folder <- make_item("")$create_folder
  drive
}
