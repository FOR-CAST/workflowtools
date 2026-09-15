#' Project root directory
#'
#' Searches upward from the current working directory for an RStudio project file
#' or a git repository, falling back on the working directory when neither is found.
#'
#' The fallback is deliberately *not* part of the `rprojroot` criterion.
#' [rprojroot::from_wd] matches every directory it is given, so combining it with
#' `|` makes the whole criterion match at the first level tested -- the working
#' directory -- and the search never walks up at all. That is what this used to do,
#' which meant `project_path()` returned `getwd()` verbatim and the RStudio and git
#' criteria were dead code. Called from anywhere but the root of a project -- a
#' `tests/testthat` directory, a module subdirectory -- it silently gave the wrong
#' answer, and every path composed from it landed in the wrong place.
#'
#' @return `project_path()` returns an absolute path;
#'         `project_name()` returns the basename of the path.
#'
#' @export
#' @rdname project_path
project_path <- function() {
  tryCatch(
    rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::is_git_root, path = getwd()),
    error = function(e) norm_path(getwd())
  )
}

#' @export
#' @rdname project_path
project_name <- function() {
  basename(project_path())
}

norm_path <- function(path) {
  unlist(path) |>
    fs::path_norm() |>
    fs::path_expand() |>
    normalizePath(winslash = "/", mustWork = FALSE)
}

null_to_na <- function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
}
#' Identify user or machine
#'
#' @param name Optional character string giving user or machine name to match.
#'
#' @return if `name` is non-`NULL`, returns a logical indicating whether
#' the current user/machine matches `name`.
#' Otherwise returns a character string with the value of the current user/machine.
#'
#' @export
#' @rdname whoami
user <- function(name = NULL) {
  if (is.null(name)) {
    Sys.info()[["user"]]
  } else {
    identical(name, Sys.info()[["user"]])
  }
}

#' @export
#' @rdname whoami
machine <- function(name = NULL) {
  if (is.null(name)) {
    Sys.info()[["nodename"]]
  } else {
    grepl(name, Sys.info()[["nodename"]])
  }
}

## copied from Require::modifyList3
mod_list <- function(..., keep.null = TRUE) {
  dots <- list(...)
  dots <- dots[!unlist(lapply(dots, is.null))]
  do.call(Reduce, alist(utils::modifyList, dots))
}

#' Extract components of a GitHub repository string
#'
#' @param x
#'
#' @return character string corresponding to the extracted component.
#'
#' @keywords internal
#' @rdname github_repo
.github_repo <- function(x) {
  ifelse(grepl("(/|@)", x[1]), remotes::parse_github_repo_spec(x[1])[["repo"]], x[[1]][1]) |>
    trimws()
}

#' @rdname github_repo
.github_user <- function(x) {
  ifelse(
    grepl("(/|@)", x[1]),
    remotes::parse_github_repo_spec(x[1])[["username"]],
    NA_character_
  ) |>
    trimws()
}

#' @rdname github_repo
.github_ref <- function(x) {
  ifelse(grepl("(/|@)", x[1]), remotes::parse_github_repo_spec(x[1])[["ref"]], NA_character_)
}
