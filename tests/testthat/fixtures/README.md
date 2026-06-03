# Fixture lockfiles for `packages_from_snapshot()`

The two `renv-v*.lock` files in this directory are minimal renv lockfiles
used by `tests/testthat/test-packages.R` to exercise the
`packages_from_snapshot()` parser against both the v1 (renv < 1.1.0) and
v2 (renv >= 1.1.0) lockfile formats without driving a live
`renv::init()` + `renv::install()` + `renv::snapshot()` ceremony.

## Why fixtures instead of live snapshots?

The previous tests built a temporary renv project and ran `renv::snapshot()`
inside it. That approach was:

* slow (several minutes per test on a clean library),
* network-dependent (needed CRAN + GitHub),
* brittle (renv's pre-flight snapshot validation aborts whenever the temp
  library's dependency graph is incomplete, e.g. when transitive deps
  haven't been pre-installed in the host R).

Fixture files make these tests deterministic and offline-friendly.

## What they cover

Both fixtures contain entries for:

* `class`, `KernSmooth`, `MASS` (base / recommended packages),
* `data.table`, `dplyr` (and a sampling of `dplyr`'s transitive deps),
* `sf` (a GitHub-sourced package; exercises the `Remote*` field path) and
  a sampling of `sf`'s transitive deps.

The v2 fixture additionally includes per-package `Requirements` arrays and
a top-level `Bioconductor` section, both of which `packages_from_snapshot()`
should silently drop.

## How to refresh the fixtures

If renv changes its lockfile format and these need updating, generate a
fresh pair in a working R session with renv installed and writable:

```r
## v1 lockfile
withr::with_options(list(renv.lockfile.version = 1L), {
  prj <- withr::local_tempdir("renv_v1_")
  setwd(prj)
  renv::init(prj, load = FALSE, restart = FALSE)
  renv::install(c("class", "data.table", "dplyr", "KernSmooth", "MASS", "r-spatial/sf"),
                project = prj, prompt = FALSE)
  renv::snapshot(project = prj, prompt = FALSE, force = TRUE)
  file.copy(file.path(prj, "renv.lock"), "<path-to>/fixtures/renv-v1.lock")
})

## v2 lockfile -- same but with renv.lockfile.version = 2L
```

After regenerating, prune any non-essential packages from the resulting
JSON so the fixtures stay small and readable.
