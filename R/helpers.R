#' Project root directory
#'
#' Searches from current working directory for an RStudio project file
#' or git repository, falling back on using the current working directory.
#'
#' @return `project_path()` returns an absolute path;
#'         `project_name()` returns the basename of the path.
#'
#' @export
#' @rdname project_path
project_path <- function() {
  rprojroot::find_root(
    rprojroot::is_rstudio_project | rprojroot::is_git_root | rprojroot::from_wd,
    path = getwd()
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
