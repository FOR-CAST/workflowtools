#' Use a SpaDES module with a project
#'
#' Adds
#'
#' @param repo character. GitHub repository specification of the form `<user|org>/<repo>@<branch>`.
#'
#' @param path character specifying the module path (e.g. `"modules"`).
#'
#' @return `NULL` invisibly. invoked for side-effect of adding a git submodule
#'
#' @export
#' @examples
#' use_module("PredictiveEcology/Biomass_core@development", "modules")
#' use_module("FOR-CAST/SBW_dispersal")
use_module <- function(repo = NULL, path = NULL) {
  if (is.null(path)) {
    path <- file.path(findProjectPath(), "modules") |> fs::path_rel()
  }

  gh_url <- "https://github.com"
  mod_name <- .github_repo(repo)
  mod_repo <- .github_user(repo)
  mod_ref <- .github_ref(repo)

  cli({
    cli_text("not yet implemented. please add manually using e.g.,")
    cli_code(
      glue("git -C {path} add submodule {gh_url}/{mod_repo}/{mod_name} {path}/{mod_name}"),
      if (nzchar(mod_ref)) glue("git -C {path}/{mod_name} checkout {mod_ref}"),
      language = "bash"
    )
  })

  return(invisible(NULL))
}
