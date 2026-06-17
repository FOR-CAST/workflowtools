#' Build stamp for report title blocks
#'
#' Compact git/date build-stamp helpers for embedding a traceable identifier
#' in a rendered document's title block (e.g. a Quarto `date:` field). Any two
#' PDFs carrying the same stamp can be traced back to the exact code that
#' produced them.
#'
#' Kept dependency-free (base R plus `git` on `PATH` only) so they can be
#' sourced from a Quarto `date:` inline expression without pulling in heavier
#' packages.
#'
#' - `build_stamp_git()` returns raw git facts (short SHA, branch, dirty flag).
#'   Each shell-out is wrapped so a missing `git` (or a non-repo checkout)
#'   degrades to `"unknown"`/clean rather than erroring.
#' - `build_stamp_date()` formats a one-line stamp for a `date:` field.
#'   ASCII-only and free of em-dashes so it renders cleanly under both
#'   pdflatex and xelatex. Examples:
#'   clean tree: `"2026-06-03 (commit a1b2c3d)"`;
#'   dirty tree: `"2026-06-03 (commit a1b2c3d, uncommitted changes)"`.
#'
#' @param repo Path to the git repository. Defaults to the current working
#'   directory (`"."`), since the Quarto render subprocess runs with the
#'   project root as its working directory.
#'
#' @param date A `Date` (or date-like value accepted by `format()`) to display.
#'   Defaults to `Sys.Date()`.
#'
#' @return
#' - `build_stamp_git()`: a list with `sha` (character), `branch` (character),
#'   and `dirty` (logical).
#' - `build_stamp_date()`: a length-1 character string.
#'
#' @export
#' @family provenance
#' @rdname build_stamp
build_stamp_git <- function(repo = ".") {
  sha <- .git_safe(c("rev-parse", "--short", "HEAD"), repo = repo)
  sha <- if (length(sha)) sha[1L] else "unknown"

  branch <- .git_safe(c("rev-parse", "--abbrev-ref", "HEAD"), repo = repo)
  branch <- if (length(branch)) branch[1L] else "unknown"

  porcelain <- .git_safe(c("status", "--porcelain"), repo = repo)
  dirty <- length(porcelain) > 0L

  list(sha = sha, branch = branch, dirty = dirty)
}

#' @export
#' @family provenance
#' @rdname build_stamp
build_stamp_date <- function(date = Sys.Date(), repo = ".") {
  g <- build_stamp_git(repo = repo)
  if (isTRUE(g$dirty)) {
    sprintf("%s (commit %s, uncommitted changes)", format(date), g$sha)
  } else {
    sprintf("%s (commit %s)", format(date), g$sha)
  }
}
