test_that("build_stamp_date formats a clean-tree stamp for a fixed date", {
  ## Stub build_stamp_git so the result is independent of the working tree.
  local_mocked_bindings(build_stamp_git = function(repo = ".") {
    list(sha = "a1b2c3d", branch = "main", dirty = FALSE)
  })
  expect_identical(build_stamp_date(date = as.Date("2026-06-03")), "2026-06-03 (commit a1b2c3d)")
})

test_that("build_stamp_date flags a dirty tree", {
  local_mocked_bindings(build_stamp_git = function(repo = ".") {
    list(sha = "a1b2c3d", branch = "main", dirty = TRUE)
  })
  expect_identical(
    build_stamp_date(date = as.Date("2026-06-03")),
    "2026-06-03 (commit a1b2c3d, uncommitted changes)"
  )
})

test_that("build_stamp_git returns the expected shape in this repo", {
  skip_if(unname(Sys.which("git")) == "", "git not available")
  g <- build_stamp_git()
  expect_named(g, c("sha", "branch", "dirty"))
  expect_length(g$sha, 1L)
  expect_type(g$sha, "character")
  expect_length(g$branch, 1L)
  expect_type(g$branch, "character")
  expect_length(g$dirty, 1L)
  expect_type(g$dirty, "logical")
})
