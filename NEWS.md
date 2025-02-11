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
