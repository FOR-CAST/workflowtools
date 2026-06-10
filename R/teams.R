## Microsoft Teams / SharePoint helpers.
##
## Generic transport layer for syncing project files with a Microsoft Teams
## channel's file area (a SharePoint document library under the hood), mirroring
## the Google Drive helpers in download.R / upload.R. Project-specific config
## (which team/channel, which files, where they go) is supplied by the caller.
##
## Microsoft365R is in Suggests: these functions guard with requireNamespace()
## so the core package install stays light for users who only need the Drive or
## reproducibility helpers.

#' Force HTTP/1.1 for the duration of the calling function
#'
#' Works around the curl "HTTP2 framing layer" error seen with both
#' \pkg{googledrive} and Microsoft Graph (\pkg{AzureGraph}/\pkg{Microsoft365R}).
#' Sets the \pkg{httr} curl config and restores the previous value via
#' [base::on.exit()] registered in `envir` (the caller's frame by default), so a
#' single call near the top of a function hardens every request it makes.
#'
#' The previous config object is embedded as a literal in the restoration call,
#' so no helper variable leaks into the caller's frame and no \pkg{withr}
#' dependency is needed.
#'
#' @param envir The frame in which to register restoration; defaults to the
#'   caller.
#'
#' @return Invisibly, the previous \pkg{httr} config.
#' @keywords internal
#' @noRd
.use_http11 <- function(envir = parent.frame()) {
  ## http_version = 2 corresponds to CURL_HTTP_VERSION_1_1
  old <- httr::set_config(httr::config(http_version = 2L))
  do.call(
    base::on.exit,
    list(substitute(httr::set_config(o), list(o = old)), add = TRUE),
    envir = envir
  )
  invisible(old)
}

#' Retry a flaky network call with a hardened curl config
#'
#' Wraps a network-facing call so a transient failure (token refresh, throttling,
#' dropped socket, HTTP/2 framing error) is retried with a short backoff rather
#' than aborting a long recursive walk or multi-file transfer. When `http11` is
#' `TRUE` (the default) it also forces HTTP/1.1 for the duration of the attempts
#' (the curl "HTTP2 framing layer" workaround) via an internal helper.
#'
#' @param fn A zero-argument function performing the network call.
#' @param times Integer; maximum number of attempts.
#' @param wait Numeric; seconds to sleep between attempts.
#' @param http11 Logical; force HTTP/1.1 for the call.
#'
#' @return The value of `fn()` on the first successful attempt. The last error is
#'   re-raised if every attempt fails.
#' @export
with_retry <- function(fn, times = 3L, wait = 3, http11 = TRUE) {
  if (isTRUE(http11)) {
    .use_http11()
  }
  for (attempt in seq_len(times)) {
    res <- tryCatch(fn(), error = function(e) e)
    if (!inherits(res, "error")) {
      return(res)
    }
    if (attempt < times) {
      cli::cli_inform(
        "Attempt {attempt}/{times} failed: {conditionMessage(res)}; retrying in {wait}s"
      )
      Sys.sleep(wait)
    } else {
      stop(res)
    }
  }
}

#' Connect to a Microsoft Teams channel's file area
#'
#' Authenticates against Microsoft Graph and resolves a team, one of its
#' channels, and that channel's root drive folder. \pkg{Microsoft365R} (via
#' \pkg{AzureAuth}) caches the token under the AzureR cache dir, so after a
#' successful device-code login subsequent calls reuse the cached token.
#'
#' @param team Team name or id (passed to [Microsoft365R::get_team()]).
#' @param channel Channel name (e.g. `"General"`).
#' @param tenant Azure tenant (e.g. an organisation short name).
#' @param app Azure app (client) id. Defaults to the well-known Azure CLI
#'   public-client id, which sidesteps needing a tenant admin to approve a
#'   bespoke app registration (see the \pkg{Microsoft365R} auth vignette).
#' @param auth_type Authentication flow; defaults to `"device_code"`.
#'
#' @return A named list with elements `team` (an `ms_team`), `channel` (an
#'   `ms_channel`), and `folder` (the channel root `ms_drive_item`).
#' @export
teams_connect <- function(
  team,
  channel,
  tenant,
  app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
  auth_type = "device_code"
) {
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg Microsoft365R} is required for Teams access.",
      "i" = "Install it with {.run install.packages(\"Microsoft365R\")}."
    ))
  }

  team_obj <- with_retry(function() {
    Microsoft365R::get_team(team, tenant = tenant, app = app, auth_type = auth_type)
  })
  chan <- team_obj$get_channel(channel)
  folder <- chan$get_folder()

  list(team = team_obj, channel = chan, folder = folder)
}

#' Ensure a (possibly nested) Teams folder exists
#'
#' Creates each missing level of `path` by name against its resolved parent item
#' (`parent$create_folder(name)`) rather than passing a multi-segment path to
#' `create_folder()`, so we never rely on the Graph API auto-creating
#' intermediate folders. This matters when the whole path is new.
#'
#' @param folder The channel root `ms_drive_item` (from [teams_connect()]).
#' @param path Folder path relative to `folder`, "/" separated.
#'
#' @return The `ms_drive_item` for the final (deepest) folder in `path`.
#' @export
teams_ensure_folder <- function(folder, path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]

  parent <- folder
  cur <- ""
  for (p in parts) {
    cur <- if (nzchar(cur)) paste(cur, p, sep = "/") else p
    item <- tryCatch(folder$get_item(cur), error = function(e) NULL)
    if (is.null(item)) {
      cli::cli_inform("Creating Teams folder: {.path {cur}}")
      with_retry(function() parent$create_folder(p))
      item <- with_retry(function() folder$get_item(cur))
    }
    parent <- item
  }
  parent
}

#' Recursively mirror a Teams folder to a local directory
#'
#' Walks the remote tree itself (list items, recreate directories, download
#' files one at a time, each wrapped in [with_retry()]). This is more reliable
#' than the built-in `ms_drive_item$download(recursive = TRUE)`, which has been
#' observed to silently skip nested subfolders.
#'
#' @param folder The channel root `ms_drive_item` (from [teams_connect()]).
#' @param path Local directory to mirror into.
#' @param remote_path Remote path relative to `folder`, "/" separated; `""`
#'   (the default) means the folder root.
#' @param exclude Character vector of remote paths (relative to `folder`, "/"
#'   separated) to skip entirely; matching files or whole subtrees are not
#'   descended into or downloaded.
#' @param overwrite Logical; when `FALSE`, existing local files are kept
#'   (resumable across flaky auth).
#' @param retries Integer; per-call retry count passed to [with_retry()].
#'
#' @return Invisibly, a character vector of the local paths downloaded/updated.
#' @export
teams_download_folder <- function(
  folder,
  path,
  remote_path = "",
  exclude = character(0),
  overwrite = TRUE,
  retries = 3L
) {
  items <- with_retry(
    function() folder$list_files(path = remote_path, full_names = FALSE),
    times = retries
  )
  if (is.null(items) || nrow(items) == 0L) {
    return(invisible(character(0)))
  }

  downloaded <- character(0)
  for (i in seq_len(nrow(items))) {
    nm <- items$name[i]
    rp <- if (nzchar(remote_path)) paste(remote_path, nm, sep = "/") else nm
    lp <- file.path(path, rp)

    if (rp %in% exclude) {
      cli::cli_inform("Excluding: {.path {rp}}")
      next
    }

    if (isTRUE(items$isdir[i])) {
      dir.create(lp, recursive = TRUE, showWarnings = FALSE)
      downloaded <- c(
        downloaded,
        teams_download_folder(folder, path, rp, exclude, overwrite, retries)
      )
    } else {
      dir.create(dirname(lp), recursive = TRUE, showWarnings = FALSE)
      if (isTRUE(overwrite) || !file.exists(lp)) {
        cli::cli_inform("Downloading: {.path {rp}}")
        with_retry(
          function() folder$get_item(rp)$download(dest = lp, overwrite = TRUE),
          times = retries
        )
        downloaded <- c(downloaded, lp)
      } else {
        cli::cli_inform("Skipping (exists): {.path {rp}}")
      }
    }
  }
  invisible(downloaded)
}

#' Upload local files into a Teams folder
#'
#' Uploads each existing file in `files` into the destination `folder` (an
#' `ms_drive_item`, e.g. from [teams_ensure_folder()]). Missing files are
#' silently skipped. An upload replaces any existing item of the same name. Each
#' upload is wrapped in [with_retry()].
#'
#' @param folder The destination `ms_drive_item`.
#' @param files Character vector of local file paths to upload.
#' @param retries Integer; per-call retry count passed to [with_retry()].
#'
#' @return A data frame manifest with columns `file` (basename) and
#'   `uploaded_at` (a timestamp string), one row per uploaded file.
#' @export
teams_upload_files <- function(folder, files, retries = 3L) {
  files <- files[file.exists(files)]
  if (length(files) == 0L) {
    return(data.frame(file = character(0), uploaded_at = character(0), stringsAsFactors = FALSE))
  }

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  for (f in files) {
    cli::cli_inform("Uploading: {.file {basename(f)}}")
    with_retry(function() folder$upload(src = f, dest = basename(f)), times = retries)
  }

  data.frame(file = basename(files), uploaded_at = now, stringsAsFactors = FALSE)
}
