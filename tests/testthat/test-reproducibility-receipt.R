test_that("info_git() records the HEAD commit line, not git's exit status", {
  skip_if(unname(Sys.which("git")) == "", "git not available")
  repo <- withr::local_tempdir()
  git <- function(...) {
    out <- system2("git", c("-C", repo, ...), stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(out, "status"))) {
      stop(paste(out, collapse = "\n"))
    }
    out
  }
  git("init", "-q")
  writeLines("x", file.path(repo, "a.txt"))
  git("add", "a.txt")
  git(
    "-c",
    "user.name=t",
    "-c",
    "user.email=t@example.com",
    "-c",
    "commit.gpgsign=false",
    "commit",
    "-q",
    "--no-verify",
    "-m",
    "first-commit"
  )
  git("remote", "add", "origin", "https://example.com/demo.git")

  g <- info_git(repo)

  expect_type(g$Head, "character")
  expect_length(g$Head, 1L)
  expect_match(g$Head, "^\\[[0-9a-f]{7,}\\] [0-9]{4}-[0-9]{2}-[0-9]{2}: first-commit$")
})
