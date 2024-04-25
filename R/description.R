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
#' @param snapshot character. path to a \pkg{renv} or \pkg{Require} snapshot file.
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
description <- function(fields = list(), library = NULL, snapshot = NULL) {
  if (is.null(library)) {
    library <- .libPaths()
  }

  useRequire <- isTRUE(getOption("workflowtools.useRequire"))

  if (is.null(snapshot)) {
    if (useRequire) {
      snapshot <- tempfile("pkgsnapshot_Require_", fileext = ".csv")
    } else {
      snapshot <- tempfile("pkgsnapshot_renv_", fileext = ".lock")
    }

    on.exit(unlink(snapshot), add = TRUE)
  }

  if (useRequire) {
    if (requireNamespace("Require", quietly = TRUE)) {
      Require::pkgSnapshot(packageVersionFile = snapshot, libPaths = library)

      pkgs <- read.csv(snapshot)
      colnames(pkgs) <- colnames(pkgs) |>
        gsub("Github", "Remote", x = _) |>
        gsub("SHA1", "Sha", x = _)
      pkgs <- as.data.table(pkgs)
    } else {
      stop("Suggested package 'Require' is not installed.")
    }
  } else {
    if (requireNamespace("renv", quietly = TRUE) &&
        requireNamespace("jsonlite", quietly = TRUE)) {
      renv::snapshot(library = library, lockfile = snapshot, type = "all")

      pkgs <- jsonlite::fromJSON(txt = "renv.lock")[["Packages"]] |>
        lapply(as.data.frame) |>
        rbindlist(fill = TRUE)
      set(pkgs, NULL, "Requirements", NULL)
    } else {
      stop("Suggested packages 'renv' or 'jsonlite' are not installed.")
    }
  }

  pkgs <- pkgs[!duplicated(pkgs)]

  cranPkgs <- pkgs[is.na(RemoteUsername), ]
  ghPkgs <- pkgs[!is.na(RemoteUsername), ]

  ## override user-specified field values
  fields <- modList(fields, list(
    Title = tools::toTitleCase(fields$Title),
    Type = "project",
    Package = NULL,
    Imports = paste0(pkgs$Package, " (== ", pkgs$Version, ")", collapse = ",\n    "),
    Remotes = paste0(ghPkgs$GithubUsername, "/", ghPkgs$GithubRepo, "@",
                     ghPkgs$GithubSHA1, collapse = ",\n    ")
  ))

  usethis::proj_set(findProjectPath(), force = TRUE)
  usethis::use_description(fields = fields, check_name = FALSE, roxygen = FALSE)

  return(invisible(NULL))
}
