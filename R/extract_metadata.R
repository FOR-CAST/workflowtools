## extract_metadata() S3 generic and base methods.
##
## Generic for turning a source-specific object (e.g. a `bcdc_record` from the
## `bcdata` package, or a raw URL) into a manifest record satisfying
## [input_manifest_schema()]. Source-specific methods live in their own files
## (`extract_bcdata.R` for `bcdc_record`); this file holds the generic, the
## default error method, and the URL fallback.

## ---- generic + default ------------------------------------------------------------------------

#' Extract a manifest record from a source-specific object
#'
#' S3 generic. Methods dispatched on the class of `x` return a manifest
#' record (a list satisfying [input_manifest_schema()]). The returned record
#' can be passed directly to [register_input()].
#'
#' Methods shipped with this package:
#'
#' - `extract_metadata.character()` -- a URL fallback. Attempts an HTTP HEAD
#'   to populate the version/vintage; degrades gracefully on failure.
#' - `extract_metadata.bcdc_record()` -- consumes the output of
#'   `bcdata::bcdc_get_record()` and pulls title, publisher, license,
#'   last-modified, and a primary resource URL.
#'
#' Source-specific methods can be added by other packages or project R/ code
#' using the usual S3 mechanism.
#'
#' @param x A source-specific object.
#' @param ... Method-specific arguments (commonly `id`, `name`, `local_path`).
#'
#' @returns A manifest record (named list) ready for [register_input()].
#' @export
extract_metadata <- function(x, ...) {
  UseMethod("extract_metadata")
}

#' @export
extract_metadata.default <- function(x, ...) {
  stop(
    sprintf(
      "No `extract_metadata()` method for class `%s`.\n  Supported classes: bcdc_record, character (URL).",
      class(x)[1L]
    ),
    call. = FALSE
  )
}

## ---- URL fallback -----------------------------------------------------------------------------

#' @export
extract_metadata.character <- function(
  x,
  id = NULL,
  name = NULL,
  local_path = NULL,
  head_lookup = TRUE,
  ...
) {
  url <- x
  if (is.null(id)) {
    id <- .id_from_url(url)
  }
  if (is.null(name)) {
    name <- basename(sub("\\?.*$", "", url))
  }
  if (is.null(local_path)) {
    local_path <- file.path("data", basename(sub("\\?.*$", "", url)))
  }

  retrieved_at <- .iso8601_now()
  version <- NULL

  if (isTRUE(head_lookup)) {
    resp <- tryCatch(httr::HEAD(url, httr::timeout(10)), error = function(e) NULL)
    if (!is.null(resp) && httr::status_code(resp) == 200L) {
      hdrs <- httr::headers(resp)
      lm <- hdrs[["last-modified"]] %||% hdrs[["Last-Modified"]]
      if (!is.null(lm) && nzchar(lm)) {
        version <- lm
      }
    } else {
      warning(
        sprintf("HEAD %s did not return a 200; version/vintage left empty.", url),
        call. = FALSE
      )
    }
  }

  input_manifest_record(
    id = id,
    name = name,
    source = list(type = "http_download", url = url),
    local_path = local_path,
    retrieved_at = retrieved_at,
    version_or_vintage = version
  )
}

## ---- shared helpers ---------------------------------------------------------------------------

.iso8601_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

## Derive a slug ID from the last segment of a URL (querystring stripped).
.id_from_url <- function(url) {
  base <- basename(sub("\\?.*$", "", url))
  base <- tools::file_path_sans_ext(base)
  base <- gsub("[^A-Za-z0-9-]+", "-", base)
  base <- gsub("-+", "-", base)
  base <- gsub("^-|-$", "", base)
  if (!nzchar(base)) {
    base <- "url-resource"
  }
  tolower(base)
}

## Generate a slugified token from an arbitrary string (e.g. a publisher name).
.slugify <- function(s) {
  s <- gsub("[^A-Za-z0-9]+", "-", s)
  s <- gsub("-+", "-", s)
  s <- gsub("^-|-$", "", s)
  tolower(s)
}

## ---- metadata cache ---------------------------------------------------------------------------

## Default cache location. Override with options(workflowtools.metadata_cache.dir = "...").
.cache_dir <- function() {
  d <- getOption(
    "workflowtools.metadata_cache.dir",
    default = file.path(tools::R_user_dir("workflowtools", which = "cache"), "metadata")
  )
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  d
}

.cache_path <- function(key) {
  file.path(.cache_dir(), paste0(key, ".rds"))
}

## Read a cached metadata record. Returns NULL if absent or stale.
.cache_read <- function(key, ttl_days) {
  path <- .cache_path(key)
  if (!file.exists(path)) {
    return(NULL)
  }
  age_days <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "days"))
  if (age_days > ttl_days) {
    return(NULL)
  }
  readRDS(path)
}

.cache_write <- function(key, value) {
  saveRDS(value, .cache_path(key))
  invisible(value)
}
