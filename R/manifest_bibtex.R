## BibTeX sync for the input-data manifest.
##
## One-way export of citations from the JSON manifest to a sidecar .bib file
## (typically `citations/data-sources.bib`), deduplicated against the project's
## hand-curated bibliography (typically `citations/references.bib`). The
## sidecar is auto-generated; the curated bib is read-only as far as this
## module is concerned.

## ---- bibentry / text formatters ---------------------------------------------------------------

#' Convert a manifest record's citation to a `bibentry`
#'
#' If `record$citation$bibtex_entry` is set, that raw BibTeX string is
#' returned as a character vector (the caller can paste it into a .bib).
#' Otherwise a minimal `@misc{...}` entry is composed from the record's
#' identifying fields. Use [as_bibentry()] to obtain a structured
#' [utils::bibentry()] object instead.
#'
#' @param record A manifest record (named list).
#' @returns A character vector of BibTeX text, terminating with `}`.
#' @export
format_bibtex_entry <- function(record) {
  cit <- record$citation %||% list()
  key <- cit$bibtex_key %||% record$id

  ## If the record carries a verbatim BibTeX entry, prefer it. Replace the
  ## key if `bibtex_key` was provided separately and differs.
  if (!is.null(cit$bibtex_entry) && nzchar(cit$bibtex_entry)) {
    entry <- trimws(cit$bibtex_entry)
    if (!is.null(cit$bibtex_key)) {
      entry <- sub("^(@[A-Za-z]+\\{)[^,]+", paste0("\\1", cit$bibtex_key), entry)
    }
    return(strsplit(entry, "\n", fixed = TRUE)[[1L]])
  }

  ## Otherwise compose a minimal @misc entry.
  year <- .bibtex_year(record)
  lines <- c(sprintf("@misc{%s,", key), sprintf("  title = {%s},", record$name))
  if (!is.null(record$source$url)) {
    lines <- c(lines, sprintf("  url = {%s},", record$source$url))
  }
  if (!is.null(year)) {
    lines <- c(lines, sprintf("  year = {%s},", year))
  }
  if (!is.null(cit$doi)) {
    lines <- c(lines, sprintf("  doi = {%s},", cit$doi))
  }
  if (!is.null(record$version_or_vintage)) {
    lines <- c(lines, sprintf("  version = {%s},", record$version_or_vintage))
  }
  if (!is.null(record$retrieved_at)) {
    lines <- c(lines, sprintf("  note = {Retrieved %s},", record$retrieved_at))
  }
  c(lines, "}")
}

#' Convert a manifest record to a `utils::bibentry`
#'
#' Returns a structured [utils::bibentry()] object that can be formatted
#' for prose ([citation_text()]) or rendered as BibTeX by `print()`.
#'
#' @param record A manifest record.
#' @returns A `utils::bibentry`.
#' @export
as_bibentry <- function(record) {
  cit <- record$citation %||% list()
  year <- .bibtex_year(record) %||% format(Sys.Date(), "%Y")

  utils::bibentry(
    bibtype = "Misc",
    key = cit$bibtex_key %||% record$id,
    title = record$name,
    year = year,
    url = record$source$url %||% "",
    note = sprintf("Retrieved %s", record$retrieved_at %||% "")
  )
}

#' Format a manifest record's citation as prose
#'
#' Convenience wrapper over [as_bibentry()] + `format(..., style = "text")`.
#'
#' @param record A manifest record.
#' @returns A character vector of formatted citation text.
#' @export
citation_text <- function(record) {
  format(as_bibentry(record), style = "text")
}

## Extract a usable 4-digit year from version_or_vintage or retrieved_at.
.bibtex_year <- function(record) {
  for (field in c("version_or_vintage", "retrieved_at")) {
    val <- record[[field]]
    if (!is.null(val)) {
      m <- regmatches(val, regexpr("[0-9]{4}", val))
      if (length(m)) {
        return(m[1L])
      }
    }
  }
  NULL
}

## ---- key extraction ---------------------------------------------------------------------------

#' Extract BibTeX keys from a .bib file
#'
#' A small regex-based extractor that returns the keys of every top-level
#' `@<bibtype>{<key>,` entry in `path`. Comments and string definitions are
#' ignored. Used by [sync_manifest_to_bibtex()] to detect collisions with a
#' hand-curated bibliography.
#'
#' @param path Path to a .bib file. If the file does not exist, returns
#'   `character(0)`.
#' @returns A character vector of BibTeX keys.
#' @export
bibtex_keys_in_file <- function(path) {
  if (!file.exists(path)) {
    return(character(0))
  }
  lines <- readLines(path, warn = FALSE)
  m <- regmatches(lines, regexec("^@[A-Za-z]+\\{([^,\\s]+)", lines, perl = TRUE))
  keys <- vapply(m, function(x) if (length(x) >= 2L) x[2L] else NA_character_, character(1L))
  keys[!is.na(keys)]
}

## ---- sync orchestrator ------------------------------------------------------------------------

#' Sync manifest citations to a sidecar BibTeX file
#'
#' One-way, conservative writer. Reads the manifest and the existing
#' (curated) references bib; for every manifest record whose citation is
#' eligible, writes one BibTeX entry to `out`. Eligible means:
#'
#' - The record has a `citation` field with a `bibtex_key`.
#' - `citation$external` is not `TRUE` (records flagged `external = TRUE`
#'   live in the curated bib and are deliberately not re-exported).
#' - The `bibtex_key` is not already present in `references_bib` (warns and
#'   skips on collision so the curated bib stays the source of truth for
#'   keys it owns).
#'
#' Records are sorted by `bibtex_key` for stable diffs. The output file
#' starts with a banner identifying it as auto-generated; do not edit it
#' by hand.
#'
#' @param manifest Path to the manifest JSON file.
#' @param out Path to the sidecar .bib file to write. Created if missing.
#' @param references_bib Path to the curated bibliography. Read for key
#'   deduplication only; never modified.
#' @param prune Logical. When `TRUE`, removes any entries already in `out`
#'   whose keys are no longer in the manifest. When `FALSE` (default), the
#'   function fully regenerates `out` from the manifest each call, which
#'   has the same effect; the argument is reserved for a future
#'   incremental-merge mode.
#'
#' @returns A list summarising the write: `written` (integer count),
#'   `skipped_external` (integer count), `skipped_collision` (character
#'   vector of keys that collided with `references_bib`), invisibly.
#' @export
sync_manifest_to_bibtex <- function(
  manifest = "data/input_manifest.json",
  out = "citations/data-sources.bib",
  references_bib = "citations/references.bib",
  prune = FALSE
) {
  records <- read_input_manifest(manifest)
  curated_keys <- bibtex_keys_in_file(references_bib)

  eligible <- list()
  skipped_external <- 0L
  skipped_collision <- character(0)

  for (rec in records) {
    cit <- rec$citation
    if (is.null(cit) || is.null(cit$bibtex_key)) {
      next
    }
    if (isTRUE(cit$external)) {
      skipped_external <- skipped_external + 1L
      next
    }
    if (cit$bibtex_key %in% curated_keys) {
      skipped_collision <- c(skipped_collision, cit$bibtex_key)
      next
    }
    eligible[[length(eligible) + 1L]] <- rec
  }

  if (length(skipped_collision)) {
    warning(
      sprintf(
        "%d manifest record(s) skipped due to key collision with %s: %s.\n  Mark these records with citation.external = TRUE to suppress this warning.",
        length(skipped_collision),
        references_bib,
        paste(skipped_collision, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  keys <- vapply(eligible, function(r) r$citation$bibtex_key, character(1L))
  eligible <- eligible[order(keys)]

  banner <- c(
    paste("% AUTO-GENERATED by workflowtools::sync_manifest_to_bibtex() from", manifest),
    "% Do not edit by hand: manual edits will be overwritten on the next",
    "% pipeline run. Add new entries to the manifest and rerun the sync.",
    ""
  )

  body <- character(0)
  for (rec in eligible) {
    body <- c(body, format_bibtex_entry(rec), "")
  }

  parent <- dirname(out)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE)
  }
  writeLines(c(banner, body), out)

  invisible(list(
    written = length(eligible),
    skipped_external = skipped_external,
    skipped_collision = skipped_collision
  ))
}
