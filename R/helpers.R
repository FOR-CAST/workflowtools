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

#' Path of a file relative to the project root
#'
#' Turns file paths back into paths relative to the project root, so they resolve from any checkout of
#' the project. A file written through a symlink in the project (`outputs` pointing to shared storage,
#' say) is usually reported by its resolved path, outside the project, where a plain relative path
#' would have to climb out with `../`.
#'
#' Each path is resolved, then mapped back by the first rule that applies:
#' 1. inside the project root: the path relative to it;
#' 2. inside the target of one of the project's top-level symlinks: that symlink's name followed by
#'    the rest of the path (the longest matching target wins);
#' 3. through a directory named like the project, as when shared storage is named after the project
#'    (`/mnt/projects/<group>/<project>/outputs/...`): the rest of the path after the deepest such
#'    directory;
#' 4. otherwise, the plain relative path from the project root.
#'
#' A path that does not exist is normalized but not resolved.
#'
#' @param path Character vector of file paths.
#' @param project Project root directory.
#'
#' @return A character vector, one path per element of `path`.
#'
#' @export
project_relative_path <- function(path, project = project_path()) {
  proj <- fs::path_abs(project)
  proj_real <- fs::path_real(proj)
  links <- as.character(fs::dir_ls(proj, type = "symlink", all = TRUE))
  targets <- vapply(
    links,
    function(l) tryCatch(as.character(fs::path_real(l)), error = function(e) NA_character_),
    character(1),
    USE.NAMES = FALSE
  )
  ## a dangling or looping symlink cannot hold a saved file
  links <- links[!is.na(targets)]
  targets <- targets[!is.na(targets)]
  proj_name <- fs::path_file(proj_real)

  vapply(
    as.character(path),
    function(p) {
      pr <- if (file.exists(p)) fs::path_real(p) else fs::path_norm(fs::path_abs(p))
      if (fs::path_has_parent(pr, proj_real)) {
        return(as.character(fs::path_rel(pr, proj_real)))
      }
      under <- vapply(
        targets,
        function(tg) fs::path_has_parent(pr, tg),
        logical(1),
        USE.NAMES = FALSE
      )
      if (any(under)) {
        k <- which(under)[which.max(nchar(targets[under]))]
        return(as.character(fs::path(fs::path_file(links[k]), fs::path_rel(pr, targets[k]))))
      }
      parts <- fs::path_split(pr)[[1L]]
      named <- which(parts == proj_name)
      if (length(named) && max(named) < length(parts)) {
        return(as.character(fs::path_join(parts[(max(named) + 1L):length(parts)])))
      }
      as.character(fs::path_rel(pr, proj_real))
    },
    character(1),
    USE.NAMES = FALSE
  )
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
#' @param x character. A GitHub repository specification in the form
#'   [remotes::parse_github_repo_spec()] accepts -- `"user/repo"`, optionally with a
#'   reference (`"user/repo@branch"`) or a subdirectory. A string containing neither
#'   `/` nor `@` is taken to be a bare package name: `.github_repo()` returns it
#'   unchanged, and `.github_user()` and `.github_ref()` return `NA_character_`.
#'   Only the first element is used.
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
