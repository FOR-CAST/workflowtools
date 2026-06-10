## Input-data manifest API.
##
## A JSON manifest of input datasets used by a project, recording where each
## dataset came from, when it was retrieved, its version/vintage, content hash,
## license, and citation. The manifest is the substrate that powers downstream
## consumers such as `sync_manifest_to_bibtex()` (one-way export to a sidecar
## .bib) and per-report provenance appendices (a table of inputs with
## pointers).
##
## File format: JSON with a top-level `records` array. Records are sorted by
## `id` on write so diffs are stable. A `@context` field is included for
## forward-compatible JSON-LD interpretation; the parser ignores it.

## ---- schema spec --------------------------------------------------------------------------------

#' Schema for input-manifest records
#'
#' Returns the schema used by [register_input()] and [validate_manifest_record()].
#' Each list entry names a field and gives its requirement level
#' (`"required"`, `"recommended"`, or `"optional"`), an expected R class, and
#' a one-line description.
#'
#' The schema is intentionally small: it captures the fields needed to
#' generate a per-PDF provenance table and a BibTeX sidecar. Source-specific
#' extras (e.g. a BC Data Catalogue record ID) live under a free-form
#' `extra` map.
#'
#' @returns A named list of field specifications.
#' @export
input_manifest_schema <- function() {
  list(
    id = list(
      level = "required",
      class = "character",
      desc = "Stable slug; the join key. Once published, do not rename."
    ),
    name = list(level = "required", class = "character", desc = "Human-readable dataset title."),
    source = list(
      level = "required",
      class = "list",
      desc = "Object: type (e.g. 'bcdata', 'http_download'), url, optional query_args."
    ),
    retrieved_at = list(
      level = "required",
      class = "character",
      desc = "ISO-8601 UTC timestamp of when the local copy was fetched."
    ),
    local_path = list(
      level = "required",
      class = "character",
      desc = "Path to the local copy, relative to the project root."
    ),
    description = list(level = "recommended", class = "character", desc = "One-line abstract."),
    version_or_vintage = list(
      level = "recommended",
      class = "character",
      desc = "Publisher's version/release/as-of date."
    ),
    sha256 = list(
      level = "recommended",
      class = "character",
      desc = "Hex digest of the local file's contents."
    ),
    license = list(
      level = "recommended",
      class = "character",
      desc = "SPDX identifier where possible."
    ),
    citation = list(
      level = "recommended",
      class = "list",
      desc = "Object: doi, bibtex_key, bibtex_entry, formatted_text, external (logical)."
    ),
    extra = list(level = "optional", class = "list", desc = "Free-form source-specific fields.")
  )
}

## ---- record constructor -------------------------------------------------------------------------

#' Construct an input-manifest record
#'
#' Validates a record's required fields, coerces the retrieval timestamp to
#' ISO-8601 UTC if needed, and returns a list ready for [register_input()].
#'
#' @param id Stable slug; the join key. Once published, do not rename.
#' @param name Human-readable dataset title.
#' @param source A list with at minimum `type` and `url`.
#' @param local_path Path to the local copy, relative to the project root.
#' @param retrieved_at ISO-8601 UTC timestamp. Defaults to `Sys.time()`.
#' @param description,version_or_vintage,sha256,license Optional metadata.
#' @param citation Optional list with `doi`, `bibtex_key`, `bibtex_entry`,
#'   `formatted_text`, and `external` (default `FALSE`).
#' @param extra Optional list of source-specific extras.
#'
#' @returns A named list satisfying [input_manifest_schema()].
#' @export
input_manifest_record <- function(
  id,
  name,
  source,
  local_path,
  retrieved_at = Sys.time(),
  description = NULL,
  version_or_vintage = NULL,
  sha256 = NULL,
  license = NULL,
  citation = NULL,
  extra = NULL
) {
  retrieved_at_str <- if (inherits(retrieved_at, "POSIXt")) {
    format(retrieved_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else {
    as.character(retrieved_at)
  }

  record <- list(
    id = id,
    name = name,
    source = source,
    retrieved_at = retrieved_at_str,
    local_path = local_path,
    description = description,
    version_or_vintage = version_or_vintage,
    sha256 = sha256,
    license = license,
    citation = citation,
    extra = extra
  )
  record <- Filter(Negate(is.null), record)

  errs <- validate_manifest_record(record)
  if (length(errs)) {
    stop(
      "input_manifest_record() validation failed:\n  ",
      paste(errs, collapse = "\n  "),
      call. = FALSE
    )
  }
  record
}

## ---- internal validator -------------------------------------------------------------------------

#' Validate an input-manifest record against the schema
#'
#' @param record A list.
#' @returns Character vector of error messages (empty if valid).
#' @keywords internal
#' @export
validate_manifest_record <- function(record) {
  schema <- input_manifest_schema()
  errs <- character(0)

  ## required fields present + non-NA
  for (field in names(schema)) {
    if (!identical(schema[[field]]$level, "required")) {
      next
    }
    if (
      !field %in% names(record) ||
        is.null(record[[field]]) ||
        (is.atomic(record[[field]]) && all(is.na(record[[field]])))
    ) {
      errs <- c(errs, sprintf("required field `%s` is missing or NA", field))
    }
  }

  ## class checks for present fields
  for (field in names(record)) {
    if (!field %in% names(schema)) {
      next
    }
    expected <- schema[[field]]$class
    if (!inherits(record[[field]], expected)) {
      errs <- c(
        errs,
        sprintf(
          "field `%s`: expected class `%s`, got `%s`",
          field,
          expected,
          class(record[[field]])[1L]
        )
      )
    }
  }

  ## source must have type + url
  if ("source" %in% names(record) && is.list(record$source)) {
    for (subfield in c("type", "url")) {
      if (!subfield %in% names(record$source) || is.null(record$source[[subfield]])) {
        errs <- c(errs, sprintf("field `source` is missing required subfield `%s`", subfield))
      }
    }
  }

  errs
}

## ---- read / write -------------------------------------------------------------------------------

#' Read an input-data manifest from JSON
#'
#' @param path Path to the manifest JSON file. Defaults to
#'   `"data/input_manifest.json"` relative to the project root.
#'
#' @returns A list of records (each a named list per [input_manifest_schema()]).
#'   Returns an empty list if `path` does not exist.
#' @export
read_input_manifest <- function(path = "data/input_manifest.json") {
  if (!file.exists(path)) {
    return(list())
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to read manifest files.", call. = FALSE)
  }
  raw <- jsonlite::read_json(path, simplifyVector = FALSE)
  records <- raw$records %||% list()
  ## strip JSON nulls so optional fields are absent rather than NULL
  lapply(records, function(rec) Filter(Negate(is.null), rec))
}

#' Write an input-data manifest to JSON
#'
#' Records are sorted by `id` for stable diffs. A `@context` field is
#' included for forward-compatible JSON-LD interpretation; current
#' readers ignore it.
#'
#' @param records A list of records.
#' @param path Path to the manifest JSON file.
#'
#' @returns `path`, invisibly.
#' @export
write_input_manifest <- function(records, path = "data/input_manifest.json") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to write manifest files.", call. = FALSE)
  }
  for (rec in records) {
    errs <- validate_manifest_record(rec)
    if (length(errs)) {
      stop(
        sprintf("record `%s` failed validation:\n  ", rec$id %||% "<no id>"),
        paste(errs, collapse = "\n  "),
        call. = FALSE
      )
    }
  }
  ids <- vapply(records, `[[`, character(1L), "id")
  records <- records[order(ids)]

  doc <- list(
    `@context` = "https://schema.org",
    `@type` = "DataCatalog",
    version = "1",
    records = records
  )

  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE)
  }

  jsonlite::write_json(doc, path, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  invisible(path)
}

## ---- register / upsert --------------------------------------------------------------------------

#' Register an input dataset in the manifest
#'
#' Idempotent upsert keyed by record `id`. If the manifest does not yet exist,
#' it is created. If `record$id` is already in the manifest, the existing entry
#' is replaced.
#'
#' @param record A manifest record, typically produced by [input_manifest_record()]
#'   or by an `extract_metadata()` method.
#' @param manifest Path to the manifest JSON file.
#'
#' @returns `manifest`, invisibly.
#' @export
register_input <- function(record, manifest = "data/input_manifest.json") {
  errs <- validate_manifest_record(record)
  if (length(errs)) {
    stop(
      sprintf("record `%s` failed validation:\n  ", record$id %||% "<no id>"),
      paste(errs, collapse = "\n  "),
      call. = FALSE
    )
  }
  existing <- read_input_manifest(manifest)
  ids <- vapply(existing, `[[`, character(1L), "id")
  i <- match(record$id, ids)
  if (is.na(i)) {
    existing[[length(existing) + 1L]] <- record
  } else {
    existing[[i]] <- record
  }
  write_input_manifest(existing, manifest)
}
