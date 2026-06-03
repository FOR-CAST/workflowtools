testthat::test_that("get_module_packages() works", {
  # test standard/simple project paths ----------------------------------------------------------

  prjPath <- "~/GitHub/Ontario_AOU_ROF"
  mpath <- file.path(prjPath, "modules")

  testthat::skip_if_not(dir.exists(prjPath))

  mp1 <- get_module_packages(module = "canClimateData", path = mpath)
  testthat::expect_identical(NROW(mp1), length(unique(mp1$Package)))

  mp2 <- get_module_packages(module = "fireSense_EscapeFit", path = mpath)
  testthat::expect_identical(NROW(mp2), length(unique(mp2$Package)))

  mp3 <- get_module_packages(module = "fireSense_dataPrepFit", path = mpath)
  testthat::expect_identical(NROW(mp3), length(unique(mp3$Package)))

  mp <- get_module_packages(path = mpath) ## all packages from all modules
  testthat::expect_identical(NROW(mp), length(unique(mp$Package)))

  pkgs0 <- unique(mp$Package) |> sort()
  pkgs1 <- unique(c(mp2$Package, mp2$Package, mp3$Package)) |> sort()
  testthat::expect_contains(pkgs0, pkgs1)

  rm(prjPath, mpath, mp1, mp2, mp3, mp, pkgs0, pkgs1)

  # test nested project paths (scfm) -------------------------------------------------------------

  prjPath <- "~/GitHub/BC_HRV"
  mpath1 <- file.path(prjPath, "modules")
  mpath2 <- file.path(mpath1, "scfm", "modules")

  testthat::skip_if_not(dir.exists(prjPath))

  mp1 <- get_module_packages(module = "Biomass_borealDataPrep", path = mpath1)
  testthat::expect_identical(NROW(mp1), length(unique(mp1$Package)))

  mp2 <- get_module_packages(module = "scfmSpread", path = mpath2) ## look directly in nested subdir
  testthat::expect_identical(NROW(mp2), length(unique(mp2$Package)))

  mp3 <- get_module_packages(module = "scfmSpread", path = mpath1) ## nested in subdir
  testthat::expect_identical(NROW(mp3), length(unique(mp3$Package)))

  mp <- get_module_packages(path = mpath1)
  testthat::expect_identical(NROW(mp), length(unique(mp$Package)))
})

## Fixture-based tests for packages_from_snapshot().
##
## Rather than driving a live renv::init() + renv::install() + renv::snapshot()
## ceremony (which is slow, network-dependent, and brittle to renv version
## drift on minimal libraries), we ship pre-built minimal lockfiles under
## tests/testthat/fixtures/ and check that the parser extracts the expected
## fields from each format. See tests/testthat/fixtures/README.md for the
## contract and instructions on how to refresh the fixtures if renv's
## lockfile format changes.

testthat::test_that("packages_from_snapshot() parses an old (v1) renv lockfile", {
  testthat::skip_if_not_installed("jsonlite")

  lockfile <- testthat::test_path("fixtures", "renv-v1.lock")
  testthat::expect_true(file.exists(lockfile))

  pkg_df <- packages_from_snapshot(lockfile)

  ## Core requested packages parsed.
  expected <- c("class", "data.table", "dplyr", "KernSmooth", "MASS", "sf")
  testthat::expect_true(all(expected %in% pkg_df[["Package"]]))

  ## Result is a data.table with the parser's documented columns.
  testthat::expect_s3_class(pkg_df, "data.table")
  expected_cols <- c(
    "Package", "RemoteHost", "RemoteRef", "RemoteRepo", "RemoteSha",
    "RemoteType", "RemoteUsername", "Repository", "Source", "Version"
  )
  testthat::expect_true(all(expected_cols %in% colnames(pkg_df)))

  ## CRAN package: Source = "Repository", no Remote* fields.
  dt_row <- pkg_df[pkg_df$Package == "data.table", ]
  testthat::expect_equal(dt_row$Source, "Repository")
  testthat::expect_equal(dt_row$Repository, "CRAN")
  testthat::expect_true(is.na(dt_row$RemoteUsername))

  ## GitHub package: Source = "GitHub", Remote* fields populated.
  sf_row <- pkg_df[pkg_df$Package == "sf", ]
  testthat::expect_equal(sf_row$Source, "GitHub")
  testthat::expect_equal(sf_row$RemoteUsername, "r-spatial")
  testthat::expect_equal(sf_row$RemoteRepo, "sf")
})

testthat::test_that("packages_from_snapshot() parses a new (v2) renv lockfile", {
  testthat::skip_if_not_installed("jsonlite")

  lockfile <- testthat::test_path("fixtures", "renv-v2.lock")
  testthat::expect_true(file.exists(lockfile))

  pkg_df <- packages_from_snapshot(lockfile)

  ## Core requested packages parsed.
  expected <- c("class", "data.table", "dplyr", "KernSmooth", "MASS", "sf")
  testthat::expect_true(all(expected %in% pkg_df[["Package"]]))

  ## v2 lockfiles include per-package Requirements; the parser does not retain
  ## that column (it only keeps the Remote*/Repository/Source/Version columns),
  ## so confirm the parser silently drops unknown fields.
  testthat::expect_false("Requirements" %in% colnames(pkg_df))

  ## CRAN + GitHub round-trips work the same as in v1.
  testthat::expect_equal(pkg_df[pkg_df$Package == "dplyr", ]$Source, "Repository")
  sf_row <- pkg_df[pkg_df$Package == "sf", ]
  testthat::expect_equal(sf_row$Source, "GitHub")
  testthat::expect_equal(sf_row$RemoteSha, "f78ddcfa4a08c3bd4b91bdaa75a3a82af8b5d2c0")
})
