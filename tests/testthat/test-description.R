## A temporary project directory, made the working directory and the active usethis project until
## the calling test ends. Its RStudio project file stops project_path() there, so description()
## cannot walk up to an enclosing git repository and write a DESCRIPTION outside it.
local_description_project <- function(env = parent.frame()) {
  prj <- withr::local_tempdir("description_", .local_envir = env)
  writeLines("Version: 1.0", file.path(prj, "test.Rproj"))
  withr::local_dir(prj, .local_envir = env)
  withr::local_options(usethis.quiet = TRUE, .local_envir = env)
  usethis::local_project(prj, quiet = TRUE, setwd = FALSE, force = TRUE, .local_envir = env)
  prj
}

## A small renv (v2) lockfile: two CRAN packages and two GitHub packages.
write_fixture_lockfile <- function(path) {
  writeLines(
    c(
      '{',
      '  "Version": "2",',
      '  "R": {',
      '    "Version": "4.5.0",',
      '    "Repositories": [{"Name": "CRAN", "URL": "https://cloud.r-project.org"}]',
      '  },',
      '  "Packages": {',
      '    "cli": {',
      '      "Package": "cli",',
      '      "Version": "3.6.3",',
      '      "Source": "Repository",',
      '      "Repository": "CRAN",',
      '      "Requirements": []',
      '    },',
      '    "data.table": {',
      '      "Package": "data.table",',
      '      "Version": "1.16.2",',
      '      "Source": "Repository",',
      '      "Repository": "CRAN",',
      '      "Requirements": []',
      '    },',
      '    "ghpkg": {',
      '      "Package": "ghpkg",',
      '      "Version": "0.1.0.9001",',
      '      "Source": "GitHub",',
      '      "RemoteType": "github",',
      '      "RemoteHost": "api.github.com",',
      '      "RemoteUsername": "example-user",',
      '      "RemoteRepo": "ghpkg",',
      '      "RemoteRef": "development",',
      '      "RemoteSha": "0123456789abcdef0123456789abcdef01234567",',
      '      "Requirements": ["cli"]',
      '    },',
      '    "sf": {',
      '      "Package": "sf",',
      '      "Version": "1.0-19",',
      '      "Source": "GitHub",',
      '      "RemoteType": "github",',
      '      "RemoteHost": "api.github.com",',
      '      "RemoteUsername": "r-spatial",',
      '      "RemoteRepo": "sf",',
      '      "RemoteRef": "main",',
      '      "RemoteSha": "f78ddcfa4a08c3bd4b91bdaa75a3a82af8b5d2c0",',
      '      "Requirements": ["data.table"]',
      '    }',
      '  }',
      '}'
    ),
    path
  )
}

split_field <- function(x) trimws(strsplit(x, ",", fixed = TRUE)[[1]])

test_that("description(take_snapshot = FALSE) pins the lockfile's packages and leaves it as is", {
  skip_if_not_installed("jsonlite")

  prj <- local_description_project()
  expect_identical(fs::path_real(project_path()), fs::path_real(prj))

  lockfile <- file.path(prj, "renv.lock")
  write_fixture_lockfile(lockfile)
  md5_before <- tools::md5sum(lockfile)

  description(
    fields = list(
      Title = "Test project",
      Description = "A project used in a test.",
      Version = "0.0.1",
      License = "MIT"
    ),
    snapshot = lockfile,
    take_snapshot = FALSE
  )

  expect_identical(tools::md5sum(lockfile), md5_before)

  desc_file <- file.path(prj, "DESCRIPTION")
  expect_true(file.exists(desc_file))
  dcf <- read.dcf(desc_file, fields = c("Imports", "Remotes"))
  expect_setequal(
    split_field(dcf[, "Imports"]),
    c("cli (== 3.6.3)", "data.table (== 1.16.2)", "ghpkg (== 0.1.0.9001)", "sf (== 1.0-19)")
  )
  expect_setequal(
    split_field(dcf[, "Remotes"]),
    c(
      "example-user/ghpkg@0123456789abcdef0123456789abcdef01234567",
      "r-spatial/sf@f78ddcfa4a08c3bd4b91bdaa75a3a82af8b5d2c0"
    )
  )

  ## nothing else was written into the project
  expect_setequal(
    list.files(prj, all.files = TRUE, no.. = TRUE),
    c("DESCRIPTION", "renv.lock", "test.Rproj")
  )
})

test_that("description(take_snapshot = FALSE) stops unless `snapshot` names an existing file", {
  prj <- local_description_project()
  lockfile <- file.path(prj, "renv.lock")

  expect_error(
    description(fields = list(Title = "Test project"), snapshot = lockfile, take_snapshot = FALSE),
    "must name an existing"
  )
  expect_error(
    description(fields = list(Title = "Test project"), snapshot = NULL, take_snapshot = FALSE),
    "must name an existing"
  )

  expect_false(file.exists(lockfile))
  expect_false(file.exists(file.path(prj, "DESCRIPTION")))
})
