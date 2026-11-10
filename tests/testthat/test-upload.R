## The caller is shared the destination folder of a shared drive without being a member of the drive,
## as a project service account is. Drive then refuses a search scoped to the shared drive, which is
## what drive_ls() runs there ("Shared drive not found"), and answers one over all drives with
## `children`, the destination's contents. drive_put() returns the folder id and file name it was
## given, so the result shows where each file went.
local_fake_destination <- function(children, frame = parent.frame()) {
  local_mocked_bindings(
    drive_ls = function(...) stop("Client error: (404) Not Found. Shared drive not found"),
    drive_find = function(..., shared_drive = NULL, corpus = NULL) {
      if (!is.null(shared_drive) || !identical(corpus, "allDrives")) {
        stop("Client error: (404) Not Found. Shared drive not found")
      }
      children
    },
    drive_mkdir = function(name, path = NULL, ...) dplyr::tibble(id = "new-folder", name = name),
    drive_put = function(media, path = NULL, ...) dplyr::tibble(id = path, name = basename(media)),
    .package = "googledrive",
    .env = frame
  )
}

## a local folder `_handoff` holding one file
local_handoff <- function(frame = parent.frame()) {
  src <- withr::local_tempdir(.local_envir = frame)
  dir.create(file.path(src, "_handoff"))
  writeLines("x", file.path(src, "_handoff", "a.txt"))
  file.path(src, "_handoff")
}

test_that("drive_upload_folder() uploads into a folder of a shared drive the caller is not a member of", {
  local_fake_destination(dplyr::tibble(id = "existing", name = "_handoff"))

  up <- drive_upload_folder(local_handoff(), "dest")
  expect_identical(as.character(up$id), "existing")
  expect_identical(up$name, "a.txt")
})

test_that("drive_upload_folder() creates the folder when the destination lacks it", {
  local_fake_destination(dplyr::tibble(id = character(), name = character()))

  up <- drive_upload_folder(local_handoff(), "dest")
  expect_identical(as.character(up$id), "new-folder")
  expect_identical(up$name, "a.txt")
})
