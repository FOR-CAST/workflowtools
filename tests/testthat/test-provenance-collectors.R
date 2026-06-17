test_that(".prov_truncate truncates only when needed", {
  expect_identical(.prov_truncate("short", max = 24L), "short")
  expect_identical(.prov_truncate(strrep("x", 30L), max = 10L), "xxxxxxx...")
  expect_identical(.prov_truncate(NA_character_), NA_character_)
})

test_that("prov_repository_state returns the expected shape in this repo", {
  skip_if(unname(Sys.which("git")) == "", "git not available")
  df <- prov_repository_state()
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("Field", "Value"))
  expect_identical(df$Field, c("HEAD", "Branch", "Submodules"))
})

test_that("prov_build_identity returns the expected shape", {
  skip_if(unname(Sys.which("git")) == "", "git not available")
  df <- prov_build_identity(scenario = "demo")
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("Field", "Value"))
  expect_in("Scenario", df$Field)
})

test_that("prov_toolchain returns the expected shape", {
  df <- prov_toolchain()
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("Component", "Version"))
  expect_identical(df$Component, c("R", "GEOS / GDAL / PROJ", "Pandoc / Quarto"))
})

test_that("prov_r_packages reports installed and missing packages", {
  df <- prov_r_packages(packages = c("workflowtools", "definitely.not.a.package.xyz"))
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("Package", "Version"))
  expect_identical(df$Version[[2L]], "n/a")
})
