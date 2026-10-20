## `rprojroot::from_wd` matches every directory it is given, so including it in a `|` criterion
## makes the search stop at the working directory and never walk up. These cases pin the search
## itself, not just the fallback: they fail if the criterion degenerates that way again.

make_project <- function(marker = c(".git", "demo.Rproj"), depth = 2) {
  marker <- match.arg(marker)
  root <- withr::local_tempdir(.local_envir = parent.frame())
  ## the temp dir may itself sit under a git repo on some machines; an inner marker still wins
  ## because the search stops at the first match walking up
  if (identical(marker, ".git")) {
    dir.create(file.path(root, ".git"))
  } else {
    writeLines("Version: 1.0", file.path(root, marker))
  }
  sub <- do.call(file.path, c(list(root), as.list(paste0("level", seq_len(depth)))))
  dir.create(sub, recursive = TRUE)
  list(root = norm_path(root), sub = sub)
}

test_that("project_path() walks up to a git root from a subdirectory", {
  p <- make_project(".git")

  withr::with_dir(p$sub, {
    expect_identical(norm_path(project_path()), p$root)
  })
})

test_that("project_path() walks up to an RStudio project from a subdirectory", {
  p <- make_project("demo.Rproj")

  withr::with_dir(p$sub, {
    expect_identical(norm_path(project_path()), p$root)
  })
})

test_that("project_path() returns the root when already at it", {
  p <- make_project(".git")

  withr::with_dir(p$root, {
    expect_identical(norm_path(project_path()), p$root)
  })
})

test_that("project_name() is the root's basename, not the working directory's", {
  p <- make_project(".git")

  withr::with_dir(p$sub, {
    expect_identical(project_name(), basename(p$root))
  })
})

test_that("project_path() falls back to the working directory when there is no project", {
  ## `/` has no marker above it, so nothing can be found from a directory whose only ancestor is /
  dir <- withr::local_tempdir()

  withr::with_dir(dir, {
    found <- norm_path(project_path())
    ## either a real root above the temp dir, or the fallback -- never an error, and always
    ## a directory that actually contains the working directory
    expect_true(dir.exists(found))
    expect_true(startsWith(norm_path(getwd()), found))
  })
})

test_that("project_relative_path() maps resolved paths back through the project's symlinks", {
  skip_on_os("windows")
  base <- withr::local_tempdir()
  proj <- file.path(base, "proj")
  ## a sibling target whose name starts with the project's: matching folder names alone gets this wrong
  share <- file.path(base, "proj-out")
  dir.create(file.path(proj, "inputs"), recursive = TRUE)
  dir.create(file.path(share, "prep"), recursive = TRUE)
  file.symlink(share, file.path(proj, "outputs"))
  f_in <- file.path(proj, "inputs", "a.tif")
  f_out <- file.path(share, "prep", "b.tif")
  file.create(f_in, f_out)

  expect_equal(project_relative_path(f_in, proj), "inputs/a.tif")
  expect_equal(
    project_relative_path(as.character(fs::path_real(f_out)), proj),
    "outputs/prep/b.tif"
  )
  expect_equal(
    project_relative_path(file.path(proj, "outputs", "prep", "b.tif"), proj),
    "outputs/prep/b.tif"
  )
  expect_equal(project_relative_path(c(f_in, f_out), proj), c("inputs/a.tif", "outputs/prep/b.tif"))
})

test_that("project_relative_path() falls back to a directory named like the project", {
  base <- withr::local_tempdir()
  proj <- file.path(base, "work", "myproj")
  shared <- file.path(base, "mnt", "projects", "grp", "myproj", "outputs")
  dir.create(proj, recursive = TRUE)
  dir.create(shared, recursive = TRUE)
  f <- file.path(shared, "c.tif")
  file.create(f)
  ## no symlink in the project, but the shared storage is named after it
  expect_equal(project_relative_path(f, proj), "outputs/c.tif")
})

test_that("project_relative_path() keeps a plain relative path otherwise", {
  base <- withr::local_tempdir()
  proj <- file.path(base, "a", "proj")
  other <- file.path(base, "b")
  dir.create(proj, recursive = TRUE)
  dir.create(other)
  f <- file.path(other, "d.tif")
  file.create(f)
  expect_equal(project_relative_path(f, proj), "../../b/d.tif")
  expect_equal(project_relative_path(file.path(proj, "missing", "e.tif"), proj), "missing/e.tif")
})
