#' Find the project root directory
#'
#' Searches from current working directory for and Rstudio project file
#' or git repository, falling back on using the current working directory.
#'
#' @return `findProjectPath` returns an absolute path;
#'         `findProjectName` returns the basename of the path.
#'
#' @export
#' @importFrom rprojroot find_root from_wd is_git_root is_rstudio_project
#' @rdname findProject
findProjectPath <- function() {
  find_root(is_rstudio_project | is_git_root | from_wd, path = getwd())
}

#' @export
#' @rdname findProject
findProjectName <- function() {
  basename(findProjectPath())
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
