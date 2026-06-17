## Generic provenance collectors.
##
## Project-agnostic helpers for assembling a build-provenance appendix:
## git repository state, build identity, software toolchain, and the
## R-package version table. Project-specific collectors (data sovereignty,
## input-data manifest, runtime-engine details, citation wording, and the
## appendix orchestrator) stay in the consuming project.

## ---- internal collectors -----------------------------------------------------

#' Run a git command, tolerating failure
#'
#' Wrap a git shell call; return `character(0)` on any failure so callers can
#' fall back to a default without aborting. `repo` is passed via `git -C` so an
#' arbitrary repository path can be targeted (default: current directory).
#'
#' @param args Character vector of git arguments.
#' @param repo Path to the git repository. Defaults to `"."`.
#'
#' @return A character vector of output lines, or `character(0)` on failure.
#'
#' @keywords internal
#' @rdname dot-git_safe
.git_safe <- function(args, repo = ".") {
  full_args <- if (is.null(repo) || identical(repo, ".")) {
    args
  } else {
    c("-C", repo, args)
  }
  tryCatch(
    suppressWarnings(system2("git", full_args, stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
}

#' Git repository state
#'
#' Collect the git state used by the build-identity and repository-state
#' provenance sections: short and full HEAD SHA, branch, the HEAD log line,
#' a human-readable clean/dirty summary, and submodule status.
#'
#' @param repo Path to the git repository. Defaults to `"."`.
#'
#' @return A named list with `sha_short`, `sha_full`, `branch`, `head_line`,
#'   `clean` (logical), `clean_str`, and `submodules`.
#'
#' @keywords internal
#' @rdname dot-prov_git_state
.prov_git_state <- function(repo = ".") {
  sha_full <- .git_safe(c("rev-parse", "HEAD"), repo = repo)
  porcelain <- .git_safe(c("status", "--porcelain"), repo = repo)
  branch <- .git_safe(c("symbolic-ref", "--short", "HEAD"), repo = repo)
  ## Use %n (newline) separators so R's system2 doesn't tokenise the
  ## arg on the embedded spaces in "%h %ad %s" -- that would make git
  ## see %ad and %s as separate revision args and abort.
  head_parts <- .git_safe(
    c("log", "-1", "--format=%h%n%ad%n%s", "--date=format-local:%Y-%m-%dT%H:%M"),
    repo = repo
  )
  head_line <- if (length(head_parts) >= 3L) {
    paste(head_parts[1L], head_parts[2L], head_parts[3L])
  } else {
    character(0)
  }
  sm_lines <- .git_safe(c("submodule", "status"), repo = repo)

  is_clean <- length(porcelain) == 0L
  clean_str <- if (is_clean) {
    "clean"
  } else {
    modified <- sum(grepl("^[ MARCD]M", porcelain))
    untracked <- sum(grepl("^\\?\\?", porcelain))
    sprintf("dirty (%d modified, %d untracked)", modified, untracked)
  }
  submodules_str <- if (length(sm_lines) == 0L) {
    "none"
  } else {
    paste(sub("^[ +U-]", "", trimws(sm_lines)), collapse = "; ")
  }

  list(
    sha_short = if (length(sha_full)) substr(sha_full[1L], 1L, 7L) else "n/a",
    sha_full = if (length(sha_full)) sha_full[1L] else "n/a",
    branch = if (length(branch)) branch[1L] else "n/a",
    head_line = if (length(head_line)) head_line[1L] else "n/a",
    clean = is_clean,
    clean_str = clean_str,
    submodules = submodules_str
  )
}

#' Truncate a string with an ellipsis
#'
#' Truncate a string to `max` characters, appending an ellipsis when cut. Used
#' to keep long unbreakable tokens (sha256 digests, commit subjects) from
#' overflowing markdown table cells in PDF output.
#'
#' @param s A character string (or `NULL`/`NA`).
#' @param max Maximum number of characters. Defaults to `24L`.
#'
#' @return The (possibly truncated) string.
#'
#' @keywords internal
#' @rdname dot-prov_truncate
.prov_truncate <- function(s, max = 24L) {
  if (is.null(s) || is.na(s)) {
    return(s)
  }
  if (nchar(s) <= max) {
    return(s)
  }
  paste0(substr(s, 1L, max - 3L), "...")
}

#' Human-readable operating-system string
#'
#' Build a descriptive OS string. On Linux, prepend the distribution's
#' `PRETTY_NAME` from `/etc/os-release` so the receipt reports e.g.
#' `"Ubuntu 24.04.4 LTS (Linux 6.8.0-124-generic, x86_64)"` instead of just the
#' kernel name.
#'
#' @return A length-1 character string.
#'
#' @keywords internal
#' @rdname dot-prov_os_string
.prov_os_string <- function() {
  si <- Sys.info()
  kernel <- sprintf("%s %s (%s)", si[["sysname"]], si[["release"]], si[["machine"]])
  if (identical(si[["sysname"]], "Linux") && file.exists("/etc/os-release")) {
    pretty <- tryCatch(
      {
        lines <- readLines("/etc/os-release", warn = FALSE)
        m <- regmatches(lines, regexec('^PRETTY_NAME="?([^"]+)"?$', lines))
        hit <- Filter(function(x) length(x) >= 2L, m)
        if (length(hit)) hit[[1L]][2L] else NULL
      },
      error = function(e) NULL
    )
    if (!is.null(pretty) && nzchar(pretty)) {
      return(sprintf("%s (%s %s, %s)", pretty, si[["sysname"]], si[["release"]], si[["machine"]]))
    }
  }
  kernel
}

#' Pandoc version string
#'
#' @return A length-1 character string, or `"n/a"` when unavailable.
#'
#' @keywords internal
#' @rdname dot-prov_pandoc
.prov_pandoc <- function() {
  tryCatch(as.character(rmarkdown::pandoc_version()), error = function(e) "n/a")
}

#' Quarto version string
#'
#' @return A length-1 character string, or `"n/a"` when unavailable.
#'
#' @keywords internal
#' @rdname dot-prov_quarto
.prov_quarto <- function() {
  tryCatch(
    trimws(system2("quarto", "--version", stdout = TRUE, stderr = FALSE))[1L],
    error = function(e) "n/a"
  )
}

## ---- exported collectors -----------------------------------------------------

#' Build-provenance collectors
#'
#' Project-agnostic collectors for assembling a build-provenance appendix in a
#' rendered report. Each returns a `data.frame` suitable for `knitr::kable()`.
#'
#' - `prov_build_identity()`: build timestamp, optional scenario label, and the
#'   git commit/branch with a clean/dirty annotation.
#' - `prov_repository_state()`: HEAD log line, branch, and submodule status,
#'   with long tokens shortened so they fit PDF table cells.
#' - `prov_toolchain()`: R, GEOS/GDAL/PROJ (via [sf::sf_extSoftVersion()]), and
#'   Pandoc/Quarto versions.
#' - `prov_r_packages()`: the installed version of each named package
#'   (`"n/a"` when not installed).
#'
#' @param scenario Optional scenario label to include in the build identity.
#' @param packages Character vector of package names to report.
#' @param repo Path to the git repository. Defaults to `"."`.
#'
#' @return A `data.frame`.
#'
#' @export
#' @family provenance
#' @rdname prov_collectors
prov_build_identity <- function(scenario = NULL, repo = ".") {
  git <- .prov_git_state(repo = repo)
  rows <- list(list(Field = "Built", Value = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
  if (!is.null(scenario)) {
    rows[[length(rows) + 1L]] <- list(Field = "Scenario", Value = sprintf("`%s`", scenario))
  }
  rows <- c(
    rows,
    list(
      list(Field = "Git commit", Value = sprintf("`%s` (%s)", git$sha_short, git$clean_str)),
      list(Field = "Git branch", Value = sprintf("`%s`", git$branch))
    )
  )
  do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE))
}

#' @export
#' @family provenance
#' @rdname prov_collectors
prov_repository_state <- function(repo = ".") {
  git <- .prov_git_state(repo = repo)
  ## HEAD line is "<sha> <date> <subject>"; subjects can be long. Keep the
  ## sha + date in full and truncate the subject to fit.
  head_short <- if (identical(git$head_line, "n/a")) {
    "n/a"
  } else {
    parts <- strsplit(git$head_line, " ", fixed = TRUE)[[1L]]
    if (length(parts) >= 3L) {
      sha_date <- paste(parts[1L], parts[2L])
      subject <- paste(parts[-(1:2)], collapse = " ")
      sprintf("%s %s", sha_date, .prov_truncate(subject, max = 50L))
    } else {
      .prov_truncate(git$head_line, max = 70L)
    }
  }
  ## Submodules joined with "; " is one unbreakable string. Replace the
  ## separator with ", " so LaTeX can wrap, AND shorten the 40-char
  ## submodule SHAs to 12 chars so the SHAs themselves don't overflow.
  sub_break <- gsub(
    "([0-9a-f]{12})[0-9a-f]{28}",
    "\\1",
    gsub("; ", ", ", git$submodules, fixed = TRUE),
    perl = TRUE
  )

  data.frame(
    Field = c("HEAD", "Branch", "Submodules"),
    Value = c(
      sprintf("`%s`", head_short),
      sprintf("`%s` (%s)", git$branch, git$clean_str),
      sub_break
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @export
#' @family provenance
#' @rdname prov_collectors
prov_toolchain <- function() {
  sv <- sf::sf_extSoftVersion()
  data.frame(
    Component = c("R", "GEOS / GDAL / PROJ", "Pandoc / Quarto"),
    Version = c(
      paste(R.version$major, R.version$minor, sep = "."),
      sprintf("%s / %s / %s", sv[["GEOS"]], sv[["GDAL"]], sv[["PROJ"]]),
      sprintf("%s / %s", .prov_pandoc(), .prov_quarto())
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @export
#' @family provenance
#' @rdname prov_collectors
prov_r_packages <- function(packages = c("workflowtools", "targets", "sf", "terra")) {
  pkg_ver <- function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "n/a")
  }
  data.frame(
    Package = paste0("`", packages, "`"),
    Version = vapply(packages, pkg_ver, character(1L)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
