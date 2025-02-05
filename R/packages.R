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
        lapply(function(x) {
          tibble::enframe(x) |>
            tidyr::pivot_wider()
        }) |>
        rbindlist(fill = TRUE)
      cols2keep <- c("Package", "RemoteHost", "RemoteRef", "RemoteRepo",
                     "RemoteSha", "RemoteType",  "RemoteUsername",
                     "Repository", "Source", "Version")
      cols2drop <- colnames(pkgs)[!(colnames(pkgs) %in% cols2keep)]
      set(pkgs, NULL, cols2drop, NULL)
      pkgs <- pkgs[, lapply(.SD, as.character)] ## convert cols from list to char
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

#' Find modules
#'
#' Determine the relative path of modules discovered in `path`.
#'
#' @param path character, specifying the path to the modules directory.
#'
#' @return character vector
#'
#' @export
find_modules <- function(path) {
  all_R <- fs::dir_ls(path, type = "file", recurse = TRUE, regexp = "[.]R$")
  is_mod <- basename(fs::path_dir(all_R)) == tools::file_path_sans_ext(fs::path_file(all_R))
  module <- fs::path_dir(all_R[is_mod]) |> fs::path_rel(path) |> as.character()
  names(module) <- fs::path_dir(all_R[is_mod]) |> as.character() |> basename()

  return(module)
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
  valid_modules <- find_modules(path)

  if (is.null(module)) {
    module_valid <- valid_modules
  } else {
    module_valid <- valid_modules[names(valid_modules) %in% module] ## with path
    module_invalid <- module[!module %in% names(valid_modules)] ## w/o path - used for messaging
    message_invalid <- paste0("The following modules are not found in ", path, ":\n",
                              "    ", paste(module_invalid, collapse = "\n    "))

    if (length(module_valid) == 0) {
      stop(message_invalid)
    }

    if (length(module_invalid) > 0) {
      warning(message_invalid)
    }
  }

  pkgdt <- lapply(module_valid, function(m) {
    if (isTRUE(verbose)) {
      message("Resolving package dependencies for module ", m, ".")
    }

    name <- basename(m)
    mod <- parse(file.path(path, m, paste0(name, ".R")))
    id1 <- vapply(mod, function(x) grepl("reqdPkgs", x) |> any(), logical(1)) |> which()
    if (length(id1) > 0) {
      id2 <- sapply(mod[id1], function(x) grepl("reqdPkgs", x)) |> which()
      pkgs <- eval(mod[[id1]][[id2]][["reqdPkgs"]]) |> unlist()
    } else {
      ## no 'reqdPkgs' field in metadata; e.g., parent module
      pkgs <- NULL
    }

    if (!is.null(pkgs)) {
      pkg_name <- lapply(strsplit(pkgs, "\\("), function(x) {
        .github_repo(x)
      }) |> unlist()
      pkg_repo <- lapply(strsplit(pkgs, "\\("), function(x) {
        .github_user(x)
      }) |> unlist()
      pkg_ref <- lapply(strsplit(pkgs, "\\("), function(x) {
        .github_ref(x)
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
  pkgdt[, Repo := unique(na.omit(Repo)), by = "Package"] ## assume want GitHub version
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
