#' Extract package info from a snapshot
#'
#' @param snapshot character. path to a \pkg{renv} or \pkg{Require} snapshot file.
#'
#' @return a `data.table` object
#'
#' @export
packages_from_snapshot <- function(snapshot = "renv.lock") {
  if (tools::file_ext(snapshot) == "lock") {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      pkgs <- jsonlite::fromJSON(txt = snapshot)[["Packages"]] |>
        lapply(as.data.frame) |>
        rbindlist(fill = TRUE)
      set(pkgs, NULL, "Requirements", NULL)
    } else {
      stop("Suggested package 'jsonlite' is not installed.")
    }
  } else {
    pkgs <- utils::read.csv(snapshot)
    colnames(pkgs) <- colnames(pkgs) |>
      gsub("Github", "Remote", x = _) |>
      gsub("SHA1", "Sha", x = _)
    pkgs <- as.data.table(pkgs)
  }

  pkgs <- pkgs[!duplicated(pkgs)]

  return(pkgs)
}

#' Get module package dependencies
#'
#' @param module character, specifying a module name.
#'               if not specified, all modules in `path` will be scanned for package dependencies.
#'
#' @param path character, specifying the path to the modules directory.
#'
#' @param verbose logical indicating whether additional debugging messages should be printed.
#'
#' @return a `data.table` with package name, repo, and minimum versions
#'
#' @export
#' @seealso [check_project_packages()]
get_module_packages <- function(module = NULL, path, verbose = FALSE) {
  if (is.null(module)) {
    module <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  }

  pkgdt <- lapply(module, function(m) {
    if (isTRUE(verbose)) {
      message("Resolving package dependencies for module ", m, ".")
    }

    mod <- parse(file.path(path, m, paste0(m, ".R")))
    id1 <- vapply(mod, function(x) grepl("reqdPkgs", x) |> any(), logical(1)) |> which()
    id2 <- sapply(mod[id1], function(x) grepl("reqdPkgs", x)) |> which()
    pkgs <- eval(mod[[id1]][[id2]][["reqdPkgs"]]) |> unlist()

    if (!is.null(pkgs)) {
      pkg_name <- lapply(strsplit(pkgs, "\\("), function(x) {
        ifelse(grepl("(/|@)", x[1]), remotes::parse_github_repo_spec(x[1])[["repo"]], x[[1]][1])
      }) |> unlist()
      pkg_repo <- lapply(strsplit(pkgs, "\\("), function(x) {
        ifelse(grepl("(/|@)", x[1]), remotes::parse_github_repo_spec(x[1])[["username"]], NA_character_)
      }) |> unlist()
      pkg_ref <- lapply(strsplit(pkgs, "\\("), function(x) {
        ifelse(grepl("(/|@)", x[1]), remotes::parse_github_repo_spec(x[1])[["ref"]], NA_character_)
      }) |> unlist()
      pkg_vers <- lapply(strsplit(pkgs, "\\("), function(x) {
        ifelse(length(x) > 1, x[[2]][1], "0") |>
          gsub("(==|>=|>)", "", x = _) |>
          gsub("\\)", "", x = _) |>
          trimws()
      }) |>
        unlist()

      data.table(
        # Module = m, ## include when debugging
        Package = pkg_name,
        Repo = pkg_repo,
        # Ref = pkg_ref, ## include when debugging
        Version = pkg_vers
      )
    } else {
      data.table(
        # Module = character(0), ## include when debugging
        Package = character(0),
        Repo = character(0),
        # Ref = character(0), ## include when debugging
        Version = character(0)
      )
    }
  }) |>
    rbindlist() |>
    unique() |>
    setkey("Package")

  pkgdt[, Version := as.character(base::max(as.numeric_version(Version), na.rm = TRUE)), by = "Package"]
  pkgdt <- unique(pkgdt)

  return(pkgdt)
}

#' Check a project's packages against module dependencies
#'
#' @param path character specifying the project path
#'
#' @param snapshot character. path to a \pkg{renv} or \pkg{Require} snapshot file.
#'
#' @return `NULL`, invisibly. will signal an error if there is a discrepancy between
#'         project and module packages.
#'
#' @export
check_project_packages <- function(path = NULL, snapshot = NULL) {
  if (is.null(path)) {
    path <- findProjectPath()
  }

  if (is.null(snapshot)) {
    snapshot <- file.path(path, "renv.lock")
  }

  stopifnot(file.exists(snapshot))

  mod_pkgs <- file.path(path, "modules") |> get_module_packages(path = _)
  mod_pkgs <- mod_pkgs[, .(Package, Version)]

  prj_pkgs <- packages_from_snapshot(snapshot)
  prj_pkgs <- prj_pkgs[, .(Package, Version)]

  pkgs <- mod_pkgs[prj_pkgs, nomatch = NULL] |>
    setnames("i.Version", "Version_Prj") |>
    setnames("Version", "Version_Mod")
  pkgs <- pkgs[as.numeric_version(Version_Mod) > as.numeric_version(Version_Prj), ]

  if (NROW(pkgs) > 0) {
    stop(
      "Module package dependencies greater than those in project:\n",
      paste0(capture.output(pkgs), "\n"), "\n",
      "Try updating selected packages using:\n",
      paste0("renv::update(c(", paste0("'", pkgs$Package, "'", collapse = ", "), "))")
    )
  }

  return(invisible(NULL))
}
