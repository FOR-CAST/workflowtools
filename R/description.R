utils::globalVariables(c(
  "RemoteUsername"
))

#' Create or modify a project's \file{DESCRIPTION} file
#'
#' A simple wrapper around `usethis::use_description()` to write
#' a \file{DESCRIPTION} file in the root project directory.
#' This file can be used to record project metadata, including package version
#' information from the current project library.
#'
#' @note fields Imports and Remotes will be automatically populated based on `snapshot`
#' (or, if `snapshot = NULL`, the currently installed packages).
#'
#' @param fields named list of \file{DESCRIPTION} fields and their values.
#'
#' @param library paths to package libraries. if `NULL`, `.libPaths()` will be used.
#'
#' @param snapshot character. path to a \pkg{renv} lockfile (`.lock`) or \pkg{Require} snapshot
#'   (`.csv`). If `NULL`, a temporary file is used; only allowed with `take_snapshot = TRUE`.
#'
#' @param take_snapshot logical. If `TRUE` (the default), first snapshot the packages installed in
#'   `library` with \pkg{renv} (or with \pkg{Require} when `options(workflowtools.useRequire = TRUE)`)
#'   and write it to `snapshot`, replacing that file: with `snapshot = "renv.lock"`, the project's
#'   lockfile is overwritten with the installed packages.
#'   If `FALSE`, `snapshot` must name an existing renv lockfile or Require snapshot, which is read
#'   as it is and not written to; `library` is then not used.
#'
#' @return `NULL` invisibly.
#' Invoked for side effect of writing a \file{DESCRIPTION} file to the project directory,
#' and printing the resulting \file{DESCRIPTION} file to screen.
#'
#' @export
#'
#' @examples
#' tmpdir <- file.path(tempdir(), "example_project") |>
#'   Require::checkPath(create = TRUE)
#' cwd <- setwd(tmpdir)
#' opts <- options(workflowtools.useRequire = TRUE)
#'
#' description(fields = list(
#'   Title = "My Project: It Does Cool Stuff",
#'   Description = paste(
#'     "My project does so many cool things.",
#'     "It's so useful for all the things."
#'   ),
#'   `Authors@R` = "c(
#'     person('First', 'Last', , 'email@email.com', role = 'aut')
#'   )",
#'   Version = "0.0.1",
#'   Language = "en-CA",
#'   License = "MIT",
#'   Depends = paste0("R (== 4.3)", collapse = ",\n    ")
#' ))
#'
#' setwd(cwd)
#' unlink(tmpdir, recursive = TRUE)
#' options(opts)
description <- function(fields = list(), library = NULL, snapshot = NULL, take_snapshot = TRUE) {
  if (!isTRUE(take_snapshot) && !isFALSE(take_snapshot)) {
    stop("`take_snapshot` must be TRUE or FALSE.")
  }

  if (is.null(library)) {
    library <- .libPaths()
  }

  useRequire <- isTRUE(getOption("workflowtools.useRequire"))

  if (is.null(snapshot) && take_snapshot) {
    if (useRequire) {
      snapshot <- tempfile("pkgsnapshot_Require_", fileext = ".csv")
    } else {
      snapshot <- tempfile("pkgsnapshot_renv_", fileext = ".lock")
    }

    on.exit(unlink(snapshot), add = TRUE)
  }

  if (!take_snapshot) {
    ## read `snapshot` as it is; nothing below writes to it
    if (is.null(snapshot) || !file.exists(snapshot)) {
      stop(
        "With `take_snapshot = FALSE`, `snapshot` must name an existing renv lockfile or ",
        "Require snapshot",
        if (is.null(snapshot)) ", not NULL." else paste0("; '", snapshot, "' does not exist.")
      )
    }
  } else if (useRequire) {
    if (requireNamespace("Require", quietly = TRUE)) {
      Require::pkgSnapshot(packageVersionFile = snapshot, libPaths = library)
    } else {
      stop("Suggested package 'Require' is not installed.")
    }
  } else {
    if (requireNamespace("renv", quietly = TRUE)) {
      renv::snapshot(library = library, lockfile = snapshot, type = "all")
    } else {
      stop("Suggested package 'renv' is not installed.")
    }
  }

  pkgs <- packages_from_snapshot(snapshot)
  cranPkgs <- pkgs[is.na(RemoteUsername), ]
  ghPkgs <- pkgs[!is.na(RemoteUsername), ]

  ## override user-specified field values
  fields <- mod_list(
    fields,
    list(
      Title = tools::toTitleCase(fields$Title),
      Type = "project",
      Package = NULL,
      Imports = paste0(pkgs$Package, " (== ", pkgs$Version, ")", collapse = ",\n    "),
      Remotes = paste0(
        ghPkgs$RemoteUsername,
        "/",
        ghPkgs$RemoteRepo,
        "@",
        ghPkgs$RemoteSha,
        collapse = ",\n    "
      )
    )
  )

  usethis::proj_set(project_path(), force = TRUE)
  usethis::use_description(fields = fields, check_name = FALSE, roxygen = FALSE)

  return(invisible(NULL))
}
