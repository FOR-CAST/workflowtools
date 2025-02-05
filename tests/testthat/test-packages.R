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

testthat::test_that("packages_from_snapshot() works with old renv lockfile", {
  testthat::skip_if_not(getRversion() >= "4.4")
  testthat::skip_if_not_installed("renv")
  testthat::skip_if_not_installed("withr")

  ## minimal renv lockfile (renv < 1.1.0)
  withr::local_options(list(
    renv.lockfile.version = 1L
  ))

  tmpPrjPath <- withr::local_tempdir("renv_old_")
  withr::local_dir(tmpPrjPath)

  lockfile <- file.path(tmpPrjPath, "renv.lock")
  tmpPrjLib <- renv::paths$library()

  renv::init(tmpPrjPath, load = FALSE, restart = FALSE)
  renv::install(
    packages = c("class", "data.table", "dplyr", "KernSmooth", "MASS", "r-spatial/sf"),
    library = tmpPrjLib,
    project = tmpPrjPath,
    lock = TRUE,
    prompt = FALSE
  )

  pkgs <- dir(tmpPrjLib, include.dirs = TRUE)
  renv::snapshot(
    project = tmpPrjPath,
    library = tmpPrjLib,
    lockfile = lockfile,
    packages = pkgs,
    prompt = FALSE
  )

  testthat::expect_no_error(
    pkg_df <- packages_from_snapshot(lockfile)
  )
  testthat::expect_true(all(pkgs %in% pkg_df[["Package"]]))

  withr::deferred_run()
})

testthat::test_that("packages_from_snapshot() works with new renv lockfile", {
  testthat::skip_if_not(getRversion() >= "4.4")
  testthat::skip_if_not_installed("renv")
  testthat::skip_if_not_installed("withr")

  ## minimal renv lockfile (renv >= 1.1.0)
  withr::local_options(list(
    renv.lockfile.version = 2L
  ))

  tmpPrjPath <- withr::local_tempdir("renv_new_")
  withr::local_dir(tmpPrjPath)

  lockfile <- file.path(tmpPrjPath, "renv.lock")
  tmpPrjLib <- renv::paths$library()

  renv::init(tmpPrjPath, load = FALSE, restart = FALSE)
  renv::install(
    packages = c("class", "data.table", "dplyr", "KernSmooth", "MASS", "r-spatial/sf"),
    library = tmpPrjLib,
    project = tmpPrjPath,
    lock = TRUE,
    prompt = FALSE
  )

  pkgs <- dir(tmpPrjLib, include.dirs = TRUE)
  renv::snapshot(
    project = tmpPrjPath,
    library = tmpPrjLib,
    lockfile = lockfile,
    packages = pkgs,
    prompt = FALSE
  )

  testthat::expect_no_error(
    pkg_df <- packages_from_snapshot(lockfile)
  )
  testthat::expect_true(all(pkgs %in% pkg_df[["Package"]]))

  withr::deferred_run()
})
