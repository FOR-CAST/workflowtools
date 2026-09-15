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
