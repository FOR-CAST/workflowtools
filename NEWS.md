# workflowtools 0.0.13

## Reconcile with `main`

* Carries forward functions that landed on `main` while `development`
  was working through the 0.0.10 → 0.0.12 sequence:
  - `download_once()` and `drive_download_once()` skip the download
    when the destination already exists locally.
  - `archive_extract_once()` skips re-extracting an archive whose
    files are already present.
* New `air.toml` + `.vscode/` configuration carried forward from
  `main`.
* `archive` added to `Imports`.

# workflowtools 0.0.12

## Input-data manifest

* New manifest API for tracking project input datasets in a JSON file at
  `data/input_manifest.json`:
  `register_input()` (idempotent upsert keyed by `id`),
  `read_input_manifest()`, `write_input_manifest()` (stable, sorted record
  order), `input_manifest_record()` (validated constructor),
  `input_manifest_schema()`. Manifest format carries a `@context` field for
  forward-compatible JSON-LD interpretation; current readers ignore it.
* `sync_manifest_to_bibtex()` is a one-way, conservative exporter that
  writes eligible manifest citations to a sidecar `.bib` (typically
  `citations/data-sources.bib`), deduplicated against a curated
  `references.bib`. Records marked `citation.external = TRUE` are skipped
  silently; key collisions warn and skip. `format_bibtex_entry()`,
  `bibtex_keys_in_file()`, `as_bibentry()`, and `citation_text()` cover
  the per-record formatting cases.
* `extract_metadata()` S3 generic turns a source-specific object into a
  manifest record. Methods shipped: `.character` (URL + optional HEAD
  probe for `Last-Modified`) and `.bcdc_record` (from
  `bcdata::bcdc_get_record()`). `metadata_bcdata(record_id)` is the
  user-facing wrapper -- fetch via bcdata, extract, and cache to
  `tools::R_user_dir("workflowtools", which = "cache")/metadata/` for 30
  days (override via `workflowtools.metadata_cache.ttl_days`).
* `jsonlite` promoted from Suggests to Imports. `bcdata` added to
  Suggests.

## Vignettes

* New: `input-data-manifest` walks through the full lifecycle.
* Removed: the long-obsolete `Managing large SpaDES projects` vignette.

# workflowtools 0.0.11

## Microsoft Teams / SharePoint helpers

* New `teams_*()` family for syncing project files with a Microsoft Teams
  channel's file area (the SharePoint document library), mirroring the existing
  Google Drive helpers:
  - `teams_connect()` authenticates and resolves a team/channel/root folder.
  - `teams_download_folder()` recursively mirrors a remote folder locally
    (walks the tree itself, since the built-in recursive download silently
    skips nested subfolders), with an `exclude` list.
  - `teams_upload_files()` uploads local files into a destination folder.
  - `teams_ensure_folder()` creates a (possibly nested) folder path.
  - `Microsoft365R` added to `Suggests` (guarded with `requireNamespace()`).
* New `with_retry()` retries a flaky network call with a short backoff and
  applies HTTP/1.1 hardening. The curl "HTTP2 framing layer" workaround
  previously inlined in `drive_download_folder()` / `drive_upload_folder()` is
  now centralised in the internal `.use_http11()` helper used by all three.

# workflowtools 0.0.10

## snake_case migration with prefix-grouped families

* All exported functions are renamed to snake_case. Two new families group
  related functions for discoverability:
  - **`project_*()`** for project-root accessors: `project_path()` (was
    `findProjectPath()`) and `project_name()` (was `findProjectName()`).
  - **`info_*()`** for introspection: `info_git()` (was `gitInfo()`),
    `info_submodules()` (was `submoduleInfo()`), `info_session()` (was
    `sessInfo()`), `info_spatial_libs()` (was `spatialLibs()`), and the
    aggregator `info_project()` (was `projectSessionInfo()`).
  - `reproducibility_receipt()` (was `reproducibilityReceipt()`) stays as a
    standalone wrapper that uses the `info_*()` family.
* The old camelCase names remain available as deprecation shims that emit a
  one-time-per-session warning via `lifecycle::deprecate_warn()` and delegate
  to the new names. Update callers at your own pace.
* `lifecycle` added to `Imports`. Internal helpers `modList` and `normPath`
  hard-renamed to `mod_list` and `norm_path` (not exported, no shim).

# workflowtools 0.0.9

* fix shell quoting for Windows in `gitInfo()` (#4);

# workflowtools 0.0.8

* replace `"NULL"` with `NA` in `packages_from_snapshot()` (#2);

# workflowtools 0.0.7

* add `tibble` and `tidyr` to imports;
* allow use of new `renv` lockfile format (#1);

# workflowtools 0.0.6

* add `glue` to imports;
* add internal helpers for parsing GitHub user, repo, and branch/tag ref;
* improve `use_module()` messaging and code output;

# workflowtools 0.0.5

* add `purrr` to Imports;
* add `drive_download_folder()` to recursively download Google Drive folders;

# workflowtools 0.0.4

* add `cli` to Imports for improved messaging;
* add project and machine setup helpers `setup_machine()` and `setup_project()`;
* fixed issue writing GitHub package metadata in `description()`;
* fixed and improved `get_module_packages()` to work with nested/multiple module paths;
* new function `find_modules()` to assist with module path discovery;
* added tests;
* improved documentation;

# workflowtools 0.0.3

* add `packages_from_snapshot()`, `get_module_packages()`, and `check_project_packages()`
  to compare module and project packages;
* allow user to pass existing pkg snapshot to `description()`;
* improved documentation;

# workflowtools 0.0.2

* move `Require` to Suggests;
* `description()` can use `renv` or `Require`;
* improved documentation;

# workflowtools 0.0.1

* initial version;
* moved various project utils from `SpaDES.project` since that package is no longer
  intended to be used to work with existing (large/complex) projects;
