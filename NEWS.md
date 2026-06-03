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

* added `packages_from_snapshot()`, `get_module_packages()`, and `check_project_packages()` to compare module and project packages;
* allow user to pass existing pkg snapshot to `description()`;
* improved documentation;

# workflowtools 0.0.2

* move `Require` to Suggests;
* `description()` can use `renv` or `Require`;
* improved documentation;

# workflowtools 0.0.1

* initial version;
* moved various project utils from `SpaDES.project` since that package is no longer intended to be used to work with existing (large/complex) projects;
