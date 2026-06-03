.spatialPkgs <- c("lwgeom", "raster", "rgdal", "rgeos", "s2", "sf", "sp", "terra", "units")
.spatialPkgsRegex <- paste(.spatialPkgs, collapse = "|")

#' Reproducibility receipt for Rmarkdown documents
#'
#' Insert git repository and R session info into Rmarkdown documents.
#' Based on suggestions in a Twitter thread by Miles McBain
#' (<https://twitter.com/MilesMcBain/status/1263272935197782016?s=20>).
#'
#' Add the following to your Rmd files (without the backslashes):
#'
#' \verb{
#' -----
#'
#' \```{r details, echo=FALSE}
#' workflowtools::reproducibility_receipt()
#' \```
#' }
#'
#' @param prjDir path to project directory
#'
#' @param title Header title for the inserted details section.
#'
#' @param writeTo If provided, an markdown filename to write to (e.g., `outputs/<runName>/INFO.md`).
#'                File path is assumed to be relative to `prjDir`.
#'
#' @export
#' @seealso [info_project()]
#' @rdname reproducibility_receipt
reproducibility_receipt <- function(
  prjDir = NULL,
  title = "Reproducibility receipt",
  writeTo = NULL
) {
  if (is.null(prjDir)) {
    prjDir <- project_path()
  }

  rr <- if (requireNamespace("details", quietly = TRUE)) {
    details::details(
      {
        info_project(prjDir)
      },
      summary = title
    )
  } else {
    stop("Suggested package 'details' is required.")
  }

  if (!is.null(writeTo)) {
    if (!identical(tools::file_ext(writeTo), "md")) {
      writeTo <- paste0(tools::file_path_sans_ext(writeTo), ".md")
    }

    writeTo <- norm_path(file.path(prjDir, writeTo))

    if (file.exists(writeTo)) {
      stop("File ", writeTo, " exists and will not be overwritten.") ## TODO: allow append
    } else {
      cat(paste0("# ", title, "\n"), rr, file = writeTo, sep = "\n")
    }
    return(invisible(rr))
  } else {
    return(rr)
  }
}

#' Project, session, and environment introspection
#'
#' The `info_*()` family returns information about the current project, its
#' git state, the R session, and externally-installed spatial libraries.
#' `info_project()` is the aggregator: it bundles git, session, spatial-library,
#' and timestamp information into a single list. Can be displayed inside an
#' Rmarkdown document via [reproducibility_receipt()], saved alongside project
#' outputs, or attached to a SpaDES `simList` to improve reproducibility.
#'
#' @param prjDir Path to project directory. Defaults to [project_path()].
#'
#' @return
#' - `info_project()`: named list of all the below.
#' - `info_git()`: list with `Local`, `Remote`, `Head`, and `Submodules`.
#' - `info_submodules()`: `data.table` with `commit` and `directory` columns.
#' - `info_session()`: a `sessioninfo::session_info()` object.
#' - `info_spatial_libs()`: named character vector from `sf::sf_extSoftVersion()`.
#' - `timestamp()`: `Sys.time()`.
#'
#' @export
#' @rdname info_project
#'
#' @examples
#' \dontrun{
#' ## get project session info for current project
#' info_project()
#'
#' ## replace default session info in a simList
#' mySimOut <- SpaDES.core::simInitAndSpades()
#' mySimOut@.xData[["._sessionInfo"]] <- info_project()
#' }
info_project <- function(prjDir = NULL) {
  if (is.null(prjDir)) {
    prjDir <- project_path()
  }

  list(
    `Git repository` = info_git(prjDir),
    `External spatial libraries` = info_spatial_libs(),
    `R session info` = info_session(),
    `Timestamp` = timestamp()
  )
}

#' @export
#' @rdname info_project
info_git <- function(prjDir = NULL) {
  if (is.null(prjDir)) {
    prjDir <- project_path()
  }

  cwd <- setwd(prjDir)
  on.exit(setwd(cwd), add = TRUE)
  local <- gsub(
    "[*] ",
    "",
    grep("[*]", system(paste(Sys.which("git"), "branch"), intern = TRUE), value = TRUE)
  )

  remote <- system(paste(Sys.which("git"), "remote -v"), intern = TRUE)
  remote <- strsplit(unique(gsub(" (.*)$", "", remote)), "\t")[[1]]
  remote <- paste0(local, " @ ", remote[1], " (", remote[2], ")")

  head <- system2(Sys.which("git"), "log -1 --format=\"[%h] %as: %s\"", wait = TRUE)

  list(Local = local, Remote = remote, Head = head, Submodules = info_submodules(prjDir))
}

#' @export
#' @rdname info_project
info_submodules <- function(prjDir = NULL) {
  if (is.null(prjDir)) {
    prjDir <- project_path()
  }

  cwd <- setwd(prjDir)
  on.exit(setwd(cwd), add = TRUE)
  submodules <- system(paste(Sys.which("git"), "submodule status"), intern = TRUE)
  submodules <- data.table::rbindlist(lapply(
    strsplit(gsub("^(-| )", "", submodules), " "),
    function(m) {
      d <- data.table::as.data.table(t(m))[, 1:2] ## keep only commit and directory columns
      colnames(d) <- c("commit", "directory")
      d
    }
  ))

  submodules
}

#' @export
#' @rdname info_project
info_session <- function() {
  sessioninfo::session_info()
}

#' @export
#' @rdname info_project
info_spatial_libs <- function() {
  sf::sf_extSoftVersion()
}

#' @export
#' @rdname info_project
timestamp <- function() {
  Sys.time()
}
