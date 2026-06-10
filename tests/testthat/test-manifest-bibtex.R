## Shared helpers --------------------------------------------------------------------------------

make_record <- function(id = "test-ds", ...) {
  defaults <- list(
    id = id,
    name = "Test Dataset",
    source = list(type = "http_download", url = "https://example.com/data.csv"),
    local_path = sprintf("data/%s.csv", id),
    retrieved_at = "2026-06-09T20:00:00Z"
  )
  args <- modifyList(defaults, list(...))
  do.call(input_manifest_record, args)
}

## format_bibtex_entry ---------------------------------------------------------------------------

testthat::test_that("format_bibtex_entry() composes a minimal @misc entry when none is supplied", {
  rec <- make_record(
    id = "nfdb",
    name = "Canadian National Fire Database",
    source = list(type = "http_download", url = "https://cwfis.cfs.nrcan.gc.ca/en_CA/nfdb"),
    version_or_vintage = "2024-12-01",
    citation = list(bibtex_key = "CFS:NFDB")
  )
  lines <- format_bibtex_entry(rec)
  testthat::expect_match(lines[1L], "^@misc\\{CFS:NFDB,")
  testthat::expect_true(any(grepl("title = \\{Canadian National Fire Database\\}", lines)))
  testthat::expect_true(any(grepl("url = \\{https://cwfis", lines)))
  testthat::expect_true(any(grepl("year = \\{2024\\}", lines)))
  testthat::expect_equal(lines[length(lines)], "}")
})

testthat::test_that("format_bibtex_entry() falls back to record id when bibtex_key is absent", {
  rec <- make_record(id = "nfdb")
  lines <- format_bibtex_entry(rec)
  testthat::expect_match(lines[1L], "^@misc\\{nfdb,")
})

testthat::test_that("format_bibtex_entry() returns verbatim bibtex_entry when supplied", {
  raw <- "@article{Foo:2026,\n  title = {Foo},\n  author = {Smith, J.},\n  year = {2026},\n}"
  rec <- make_record(citation = list(bibtex_key = "Foo:2026", bibtex_entry = raw))
  lines <- format_bibtex_entry(rec)
  testthat::expect_equal(lines[1L], "@article{Foo:2026,")
  testthat::expect_equal(lines[length(lines)], "}")
})

testthat::test_that("format_bibtex_entry() rewrites the key in a verbatim entry when bibtex_key differs", {
  raw <- "@article{Old:Key,\n  title = {Foo},\n}"
  rec <- make_record(citation = list(bibtex_key = "New:Key", bibtex_entry = raw))
  lines <- format_bibtex_entry(rec)
  testthat::expect_equal(lines[1L], "@article{New:Key,")
})

## as_bibentry + citation_text ------------------------------------------------------------------

testthat::test_that("as_bibentry() returns a bibentry with the manifest's fields", {
  rec <- make_record(citation = list(bibtex_key = "TestKey"))
  b <- as_bibentry(rec)
  testthat::expect_s3_class(b, "bibentry")
  testthat::expect_equal(b$key, "TestKey")
  testthat::expect_equal(b$title, "Test Dataset")
})

testthat::test_that("citation_text() returns formatted prose", {
  rec <- make_record(citation = list(bibtex_key = "TestKey"))
  out <- citation_text(rec)
  testthat::expect_true(any(grepl("Test Dataset", out)))
})

## bibtex_keys_in_file --------------------------------------------------------------------------

testthat::test_that("bibtex_keys_in_file() returns the keys in a .bib", {
  testthat::skip_if_not_installed("withr")
  path <- withr::local_tempfile(fileext = ".bib")
  writeLines(
    c(
      "% leading comment",
      "@article{Smith:2026,",
      "  title = {Foo},",
      "}",
      "",
      "@misc{Jones:2025,",
      "  title = {Bar},",
      "}"
    ),
    path
  )
  testthat::expect_setequal(bibtex_keys_in_file(path), c("Smith:2026", "Jones:2025"))
})

testthat::test_that("bibtex_keys_in_file() returns character(0) when the file is absent", {
  testthat::expect_equal(bibtex_keys_in_file(tempfile(fileext = ".bib")), character(0))
})

## sync_manifest_to_bibtex ----------------------------------------------------------------------

setup_sync_fixture <- function(curated_keys = character(0), envir = parent.frame()) {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("withr")
  ## Tie tempfile lifetimes to the caller (the test), not this helper.
  manifest_path <- withr::local_tempfile(fileext = ".json", .local_envir = envir)
  out_path <- withr::local_tempfile(fileext = ".bib", .local_envir = envir)
  refs_path <- withr::local_tempfile(fileext = ".bib", .local_envir = envir)

  writeLines(
    vapply(
      curated_keys,
      function(k) sprintf("@article{%s,\n  title = {Curated},\n}", k),
      character(1L)
    ),
    refs_path
  )

  list(manifest = manifest_path, out = out_path, refs = refs_path)
}

testthat::test_that("sync_manifest_to_bibtex() writes eligible records sorted by key", {
  paths <- setup_sync_fixture()

  recs <- list(
    make_record(id = "gamma", citation = list(bibtex_key = "Gamma:2026")),
    make_record(id = "alpha", citation = list(bibtex_key = "Alpha:2026")),
    make_record(id = "beta", citation = list(bibtex_key = "Beta:2026"))
  )
  for (r in recs) {
    register_input(r, paths$manifest)
  }

  res <- sync_manifest_to_bibtex(paths$manifest, paths$out, paths$refs)

  testthat::expect_equal(res$written, 3L)
  testthat::expect_equal(res$skipped_external, 0L)
  testthat::expect_equal(res$skipped_collision, character(0))

  keys <- bibtex_keys_in_file(paths$out)
  testthat::expect_equal(keys, c("Alpha:2026", "Beta:2026", "Gamma:2026"))
})

testthat::test_that("sync_manifest_to_bibtex() skips records without citation", {
  paths <- setup_sync_fixture()
  register_input(make_record(id = "alpha"), paths$manifest)
  register_input(
    make_record(id = "beta", citation = list(bibtex_key = "Beta:2026")),
    paths$manifest
  )

  res <- sync_manifest_to_bibtex(paths$manifest, paths$out, paths$refs)
  testthat::expect_equal(res$written, 1L)
  testthat::expect_equal(bibtex_keys_in_file(paths$out), "Beta:2026")
})

testthat::test_that("sync_manifest_to_bibtex() skips external = TRUE records silently", {
  paths <- setup_sync_fixture()
  register_input(
    make_record(id = "alpha", citation = list(bibtex_key = "Alpha:2026", external = TRUE)),
    paths$manifest
  )
  register_input(
    make_record(id = "beta", citation = list(bibtex_key = "Beta:2026")),
    paths$manifest
  )

  res <- sync_manifest_to_bibtex(paths$manifest, paths$out, paths$refs)
  testthat::expect_equal(res$written, 1L)
  testthat::expect_equal(res$skipped_external, 1L)
  testthat::expect_equal(bibtex_keys_in_file(paths$out), "Beta:2026")
})

testthat::test_that("sync_manifest_to_bibtex() warns on key collision with references.bib", {
  paths <- setup_sync_fixture(curated_keys = "Collide:2026")
  register_input(
    make_record(id = "alpha", citation = list(bibtex_key = "Collide:2026")),
    paths$manifest
  )
  register_input(
    make_record(id = "beta", citation = list(bibtex_key = "Beta:2026")),
    paths$manifest
  )

  testthat::expect_warning(
    res <- sync_manifest_to_bibtex(paths$manifest, paths$out, paths$refs),
    regexp = "Collide:2026.*external = TRUE"
  )
  testthat::expect_equal(res$written, 1L)
  testthat::expect_equal(res$skipped_collision, "Collide:2026")
  testthat::expect_equal(bibtex_keys_in_file(paths$out), "Beta:2026")
})

testthat::test_that("sync_manifest_to_bibtex() writes a banner identifying the file as auto-generated", {
  paths <- setup_sync_fixture()
  register_input(
    make_record(id = "alpha", citation = list(bibtex_key = "Alpha:2026")),
    paths$manifest
  )
  sync_manifest_to_bibtex(paths$manifest, paths$out, paths$refs)
  lines <- readLines(paths$out)
  testthat::expect_match(lines[1L], "AUTO-GENERATED")
  testthat::expect_match(lines[1L], "sync_manifest_to_bibtex")
})

testthat::test_that("sync_manifest_to_bibtex() produces a banner-only file when the manifest is empty", {
  paths <- setup_sync_fixture()
  sync_manifest_to_bibtex(paths$manifest, paths$out, paths$refs)
  testthat::expect_equal(bibtex_keys_in_file(paths$out), character(0))
  lines <- readLines(paths$out)
  testthat::expect_match(lines[1L], "AUTO-GENERATED")
})
