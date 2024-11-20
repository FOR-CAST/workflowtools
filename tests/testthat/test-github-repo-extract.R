test_that("extract GitHub repo components works", {
  ## 1. package repository, with branch
  ## 2. package repository, without branch
  ## 3. module repository, with branch
  ## 4. module repository, without branch

  repos <- c(
    "PredictiveEcology/SpaDES.core@development",
    "achubaty/grainscape",
    "PredictiveEcology/Biomass_core@development",
    "FOR-CAST/SBW_dispersal"
  )

  expect_identical(.github_user(repos[1]), "PredictiveEcology")
  expect_identical(.github_repo(repos[1]), "SpaDES.core")
  expect_identical(.github_ref(repos[1]), "development")

  expect_identical(.github_user(repos[2]), "achubaty")
  expect_identical(.github_repo(repos[2]), "grainscape")
  expect_identical(.github_ref(repos[2]), "")

  expect_identical(.github_user(repos[3]), "PredictiveEcology")
  expect_identical(.github_repo(repos[3]), "Biomass_core")
  expect_identical(.github_ref(repos[3]), "development")

  expect_identical(.github_user(repos[4]), "FOR-CAST")
  expect_identical(.github_repo(repos[4]), "SBW_dispersal")
  expect_identical(.github_ref(repos[4]), "")
})
