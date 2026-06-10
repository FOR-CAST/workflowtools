## bcdata adapter for extract_metadata().
##
## Consumes a bcdc_record (the output of bcdata::bcdc_get_record()) and emits a
## manifest record. Also provides `metadata_bcdata()`, a user-facing wrapper
## that fetches + caches + extracts in one call.
##
## bcdata is a soft dependency (Suggests); errors gracefully when not installed.

## ---- bcdc_record method -----------------------------------------------------------------------

#' @export
extract_metadata.bcdc_record <- function(x, id = NULL, local_path = NULL, ...) {
  ## Defensive accessors -- bcdc_record fields evolve across bcdata versions.
  title <- x$title %||% "Untitled BC Data Catalogue Record"
  bcdc_id <- x$id %||% NA_character_
  record_url <- x$record_url %||% x$url %||% NULL

  publisher <- .extract_publisher(x)
  license <- .extract_license(x)
  last_modified <- .extract_last_modified(x)
  resource_url <- .extract_first_resource_url(x)

  year <- if (!is.null(last_modified) && !is.na(last_modified)) {
    format(as.Date(last_modified), "%Y")
  } else {
    format(Sys.Date(), "%Y")
  }

  if (is.null(id)) {
    id <- if (!is.na(bcdc_id) && nzchar(bcdc_id)) {
      .slugify(bcdc_id)
    } else {
      .slugify(title)
    }
  }
  if (is.null(local_path)) {
    local_path <- file.path("data", paste0(id, ".dat"))
  }

  pub_short <- .acronym(publisher %||% "BCDC")
  bibtex_key <- sprintf("%s:%s", pub_short, year)

  citation <- list(
    bibtex_key = bibtex_key,
    bibtex_entry = .bcdc_bibtex_entry(
      key = bibtex_key,
      title = title,
      publisher = publisher %||% "British Columbia Data Catalogue",
      year = year,
      url = record_url %||% resource_url %||% ""
    ),
    formatted_text = sprintf(
      "%s (%s). %s.",
      publisher %||% "British Columbia Data Catalogue",
      year,
      title
    ),
    external = FALSE
  )

  input_manifest_record(
    id = id,
    name = title,
    source = list(
      type = "bcdata",
      url = record_url %||% resource_url %||% "",
      query_args = list(record_id = bcdc_id)
    ),
    local_path = local_path,
    retrieved_at = .iso8601_now(),
    version_or_vintage = if (!is.null(last_modified)) {
      as.character(last_modified)
    } else {
      NULL
    },
    license = license,
    citation = citation,
    extra = list(bcdc_record_id = bcdc_id)
  )
}

## ---- user-facing wrapper ----------------------------------------------------------------------

#' Get manifest metadata for a BC Data Catalogue record
#'
#' Convenience wrapper that calls `bcdata::bcdc_get_record(record_id)`, passes
#' the result through [extract_metadata()], and caches the resulting record on
#' disk. Subsequent calls with the same `record_id` return the cached record
#' until `ttl_days` elapse or `refresh = TRUE`.
#'
#' Cache location defaults to
#' `tools::R_user_dir("workflowtools", which = "cache")/metadata/`; override
#' with `options(workflowtools.metadata_cache.dir = "...")`.
#'
#' @param record_id Character. The BC Data Catalogue record identifier
#'   (a UUID or slug, e.g. `"2ebb35d8-c82f-4a17-9c96-612ac3532d55"`).
#' @param refresh Logical. When `TRUE`, ignore any cached value and re-fetch
#'   from the BCDC API.
#' @param ttl_days Integer. Cache expiry in days. Default 30; override per
#'   call or globally via
#'   `options(workflowtools.metadata_cache.ttl_days = ...)`.
#' @param ... Additional arguments forwarded to the `bcdc_record` method of
#'   [extract_metadata()].
#'
#' @returns A manifest record.
#' @export
metadata_bcdata <- function(
  record_id,
  refresh = FALSE,
  ttl_days = getOption("workflowtools.metadata_cache.ttl_days", 30L),
  ...
) {
  if (!requireNamespace("bcdata", quietly = TRUE)) {
    stop(
      "Package `bcdata` is required by `metadata_bcdata()`. ",
      "Install it with `install.packages('bcdata')`.",
      call. = FALSE
    )
  }

  cache_key <- sprintf("bcdata-%s", .slugify(record_id))
  if (!isTRUE(refresh)) {
    cached <- .cache_read(cache_key, ttl_days)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  bcdc_rec <- bcdata::bcdc_get_record(record_id)
  rec <- extract_metadata(bcdc_rec, ...)
  .cache_write(cache_key, rec)
  rec
}

## ---- internal helpers -------------------------------------------------------------------------

## bcdc_record field accessors -- defensive, since bcdata field structure
## has shifted across versions.
.extract_publisher <- function(x) {
  if (!is.null(x$publisher)) {
    if (is.list(x$publisher)) {
      return(x$publisher$name %||% x$publisher[[1L]])
    }
    return(as.character(x$publisher))
  }
  if (!is.null(x$organization)) {
    return(as.character(x$organization))
  }
  NULL
}

.extract_license <- function(x) {
  lic <- x$license
  if (is.null(lic)) {
    return(NULL)
  }
  if (is.list(lic)) {
    return(lic$title %||% lic$id %||% lic[[1L]])
  }
  as.character(lic)
}

.extract_last_modified <- function(x) {
  lm <- x$last_modified %||% x$record_last_modified
  if (is.null(lm)) {
    return(NULL)
  }
  lm
}

.extract_first_resource_url <- function(x) {
  if (is.null(x$resources) || length(x$resources) == 0L) {
    return(NULL)
  }
  res <- x$resources[[1L]]
  if (is.list(res)) {
    return(res$url %||% NULL)
  }
  if (is.data.frame(res) && "url" %in% colnames(res)) {
    return(as.character(res$url[1L]))
  }
  NULL
}

## Crude acronym builder: take the uppercase letters of an organisation name,
## or fall back to the first 4 alphanumerics.
.acronym <- function(s) {
  if (is.null(s) || !nzchar(s)) {
    return("BCDC")
  }
  upper <- gsub("[^A-Z]", "", s)
  if (nchar(upper) >= 2L) {
    return(substr(upper, 1L, 6L))
  }
  alpha <- gsub("[^A-Za-z0-9]", "", s)
  toupper(substr(alpha, 1L, 4L))
}

.bcdc_bibtex_entry <- function(key, title, publisher, year, url) {
  paste(
    sprintf("@misc{%s,", key),
    sprintf("  title = {{%s}},", title),
    sprintf("  author = {{%s}},", publisher),
    sprintf("  year = {%s},", year),
    sprintf("  publisher = {{%s}},", publisher),
    sprintf("  url = {%s},", url),
    "}",
    sep = "\n"
  )
}
