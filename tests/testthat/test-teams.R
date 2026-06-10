test_that("with_retry returns on first success", {
  calls <- 0L
  res <- with_retry(
    function() {
      calls <<- calls + 1L
      "ok"
    },
    http11 = FALSE
  )
  expect_equal(res, "ok")
  expect_equal(calls, 1L)
})

test_that("with_retry retries then succeeds", {
  calls <- 0L
  res <- suppressMessages(with_retry(
    function() {
      calls <<- calls + 1L
      if (calls < 3L) {
        stop("boom")
      }
      "ok"
    },
    times = 3L,
    wait = 0,
    http11 = FALSE
  ))
  expect_equal(res, "ok")
  expect_equal(calls, 3L)
})

test_that("with_retry re-raises the error after exhausting attempts", {
  calls <- 0L
  expect_error(
    suppressMessages(with_retry(
      function() {
        calls <<- calls + 1L
        stop("boom")
      },
      times = 2L,
      wait = 0,
      http11 = FALSE
    )),
    "boom"
  )
  expect_equal(calls, 2L)
})

test_that("with_retry http11 hardening does not error and restores cleanly", {
  ## .use_http11() sets the httr curl config and restores it via on.exit when
  ## with_retry returns; a missing/broken restore would error here.
  expect_equal(with_retry(function() "ok", http11 = TRUE), "ok")
  ## a second call still works (config was restored, not left dangling)
  expect_equal(with_retry(function() "ok2", http11 = TRUE), "ok2")
})

test_that("teams_download_folder mirrors recursively and honours exclude", {
  tmp <- withr::local_tempdir()
  tree <- list(
    "<root>" = data.frame(
      name = c("dirA", "f1.txt", "secret"),
      isdir = c(TRUE, FALSE, TRUE),
      size = c(0, 10, 0),
      stringsAsFactors = FALSE
    ),
    "dirA" = data.frame(name = "f2.txt", isdir = FALSE, size = 5, stringsAsFactors = FALSE),
    "secret" = data.frame(name = "s1.txt", isdir = FALSE, size = 5, stringsAsFactors = FALSE)
  )
  folder <- fake_download_folder(tree)

  got <- suppressMessages(teams_download_folder(folder, tmp, exclude = "secret"))

  expect_setequal(basename(got), c("f1.txt", "f2.txt"))
  expect_true(file.exists(file.path(tmp, "f1.txt")))
  expect_true(file.exists(file.path(tmp, "dirA", "f2.txt")))
  expect_false(file.exists(file.path(tmp, "secret", "s1.txt")))
  expect_false(dir.exists(file.path(tmp, "secret")))
})

test_that("teams_download_folder skips existing files when overwrite = FALSE", {
  tmp <- withr::local_tempdir()
  tree <- list(
    "<root>" = data.frame(name = "f1.txt", isdir = FALSE, size = 10, stringsAsFactors = FALSE)
  )
  folder <- fake_download_folder(tree)
  writeLines("KEEP", file.path(tmp, "f1.txt"))

  got <- suppressMessages(teams_download_folder(folder, tmp, overwrite = FALSE))

  expect_length(got, 0)
  expect_equal(readLines(file.path(tmp, "f1.txt")), "KEEP")
})

test_that("teams_upload_files uploads existing files and skips missing", {
  f1 <- withr::local_tempfile(fileext = ".pdf")
  writeLines("a", f1)
  f2 <- withr::local_tempfile(fileext = ".pdf")
  writeLines("b", f2)
  missing <- file.path(tempdir(), "does-not-exist-xyz.pdf")

  target <- fake_upload_target()
  manifest <- suppressMessages(teams_upload_files(target, c(f1, f2, missing)))

  expect_setequal(target$uploaded, c(basename(f1), basename(f2)))
  expect_equal(nrow(manifest), 2L)
  expect_setequal(manifest$file, c(basename(f1), basename(f2)))
  expect_true(all(c("file", "uploaded_at") %in% names(manifest)))
})

test_that("teams_upload_files returns an empty manifest when nothing exists", {
  manifest <- teams_upload_files(fake_upload_target(), file.path(tempdir(), "nope-abc.pdf"))
  expect_equal(nrow(manifest), 0L)
  expect_named(manifest, c("file", "uploaded_at"))
})

test_that("teams_ensure_folder creates each missing level in order", {
  drive <- fake_drive()
  item <- suppressMessages(teams_ensure_folder(drive, "Phase 3 outputs/rendered-reports"))
  expect_equal(item$path, "Phase 3 outputs/rendered-reports")
  expect_equal(drive$created, c("Phase 3 outputs", "Phase 3 outputs/rendered-reports"))
})

test_that("teams_ensure_folder is idempotent for existing levels", {
  drive <- fake_drive(existing = "Phase 3 outputs")
  item <- suppressMessages(teams_ensure_folder(drive, "Phase 3 outputs/rendered-reports"))
  expect_equal(item$path, "Phase 3 outputs/rendered-reports")
  expect_equal(drive$created, "Phase 3 outputs/rendered-reports")
})

test_that("teams_connect resolves team, channel, and folder", {
  skip_if_not_installed("Microsoft365R")

  fake_folder <- new.env(parent = emptyenv())
  fake_channel <- new.env(parent = emptyenv())
  fake_channel$get_folder <- function() fake_folder
  fake_team <- new.env(parent = emptyenv())
  fake_team$get_channel <- function(channel) fake_channel

  testthat::local_mocked_bindings(
    get_team = function(team, tenant, app, auth_type) fake_team,
    .package = "Microsoft365R"
  )

  conn <- teams_connect(team = "T", channel = "General", tenant = "org")

  expect_identical(conn$team, fake_team)
  expect_identical(conn$channel, fake_channel)
  expect_identical(conn$folder, fake_folder)
})
