test_that("get_module_packages() works", {
  # test standard/simple project paths ----------------------------------------------------------

  prjPath <- "~/GitHub/Ontario_AOU_ROF"
  mpath <- file.path(prjPath, "modules")

  skip_if_not(dir.exists(prjPath))

  mp1 <- get_module_packages(module = "canClimateData", path = mpath)
  expect_identical(NROW(mp1), length(unique(mp1$Package)))

  mp2 <- get_module_packages(module = "fireSense_EscapeFit", path = mpath)
  expect_identical(NROW(mp2), length(unique(mp2$Package)))

  mp3 <- get_module_packages(module = "fireSense_dataPrepFit", path = mpath)
  expect_identical(NROW(mp3), length(unique(mp3$Package)))

  mp <- get_module_packages(path = mpath) ## all packages from all modules
  expect_identical(NROW(mp), length(unique(mp$Package)))

  pkgs0 <- unique(mp$Package) |> sort()
  pkgs1 <- unique(c(mp2$Package, mp2$Package, mp3$Package)) |> sort()
  expect_contains(pkgs0, pkgs1)

  rm(prjPath, mpath, mp1, mp2, mp3, mp, pkgs0, pkgs1)

  # test nested project paths (scfm) -------------------------------------------------------------

  prjPath <- "~/GitHub/BC_HRV"
  mpath1 <- file.path(prjPath, "modules")
  mpath2 <- file.path(mpath1, "scfm", "modules")

  skip_if_not(dir.exists(prjPath))

  mp1 <- get_module_packages(module = "Biomass_borealDataPrep", path = mpath1)
  expect_identical(NROW(mp1), length(unique(mp1$Package)))

  mp2 <- get_module_packages(module = "scfmSpread", path = mpath2) ## look directly in nested subdir
  expect_identical(NROW(mp2), length(unique(mp2$Package)))

  mp3 <- get_module_packages(module = "scfmSpread", path = mpath1) ## nested in subdir
  expect_identical(NROW(mp3), length(unique(mp3$Package)))

  mp <- get_module_packages(path = mpath1)
  expect_identical(NROW(mp), length(unique(mp$Package)))
})
