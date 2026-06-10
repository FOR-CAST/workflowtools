# workflowtools

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/FOR-CAST/workflowtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FOR-CAST/workflowtools/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/FOR-CAST/workflowtools/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/FOR-CAST/workflowtools/actions/workflows/test-coverage.yaml)
[![pkgdown](https://github.com/FOR-CAST/workflowtools/actions/workflows/pkgdown.yaml/badge.svg)](https://for-cast.github.io/workflowtools/)
<!-- badges: end -->

`workflowtools` provides a small set of opinionated helpers for managing large,
multi-source R projects: tracking input datasets and their provenance,
recording reproducibility information for every build, syncing files with
cloud storage, and bootstrapping new projects. It is intentionally narrow:
each function is small, single-purpose, and composable with `{targets}`,
`{renv}`, and similar tooling.

## Installation

```r
# Install the development version from GitHub:
remotes::install_github("FOR-CAST/workflowtools@development")
```

## Getting started

```r
library(workflowtools)
```

### Track input datasets with a manifest

Record where each input dataset came from, when it was retrieved, its
vintage, license, and citation. Downstream tooling (provenance appendices,
auto-generated bibliographies) reads the manifest so input documentation
stays in sync with what was actually fetched.

```r
# Construct a record by hand:
nfdb <- input_manifest_record(
  id          = "nfdb",
  name        = "Canadian National Fire Database",
  source      = list(type = "http_download", url = "https://cwfis.cfs.nrcan.gc.ca/en_CA/nfdb"),
  local_path  = "data/processed/nfdb.gpkg",
  license     = "OGL-Canada-2.0",
  citation    = list(bibtex_key = "CFS:NFDB")
)
register_input(nfdb)            # idempotent upsert, keyed by id

# Or extract metadata from a BC Data Catalogue record:
vri <- metadata_bcdata("2ebb35d8-c82f-4a17-9c96-612ac3532d55")
register_input(vri)

# Sync eligible manifest citations to a sidecar .bib (deduplicated against
# the project's curated references.bib):
sync_manifest_to_bibtex()
```

See the [`input-data-manifest`](https://for-cast.github.io/workflowtools/articles/input-data-manifest.html)
vignette for the full lifecycle, including custom S3 methods for new source
types.

### Reproducibility receipts

Record git, R session, spatial-library, and timestamp metadata for every
build so any output PDF, report, or simList carries a stable build identity.

```r
reproducibility_receipt(writeTo = "INFO.md")

# Or use the individual collectors:
info_project()        # full bundle
info_git()            # repository state
info_session()        # R session info
info_spatial_libs()   # GEOS / GDAL / PROJ versions
```

### Project + module helpers

Bootstrap a new project, declare its module dependencies, and verify them
against the project library:

```r
setup_machine()                  # one-time host setup checklist
setup_project()                  # new-project setup checklist
description()                    # generate a project-level DESCRIPTION
check_project_packages()         # verify module deps vs renv.lock
get_module_packages(path = ".")  # parse `reqdPkgs` from SpaDES-style modules
```

### Cloud file sync

Mirror folders to Google Drive or a Microsoft Teams / SharePoint document
library:

```r
drive_upload_folder(folder = "outputs/figures", drive_path = "...")
drive_download_folder(drive_folder = "...", path = "data/")

folder <- teams_connect(team = "Project Team", channel = "General")
teams_upload_files(folder, files = list.files("outputs", full.names = TRUE))
teams_download_folder(folder, path = "data/")
```

## Documentation

Full function reference and vignettes are at
<https://for-cast.github.io/workflowtools/>.

## Code of conduct

Please note that this project is released with a
[Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
By contributing, you agree to abide by its terms.
