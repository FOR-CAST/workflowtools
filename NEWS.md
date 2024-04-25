# workflowtools (development version)

* add `cli` to Imports for improved messaging;
* add project and machine setup helpers `setup_machine()` and `setup_project()`;
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
