#' Use a SpaDES module with a project
#'
#' Adds
#'
#' @param path character specifying the module path (e.g. `"modules"`)
#'
#' @param repo character. GitHub repository specification of the form `<user|org>/<repo>@<branch>`.
#'
#' @return `NULL` invisibly. invoked for side-effect of adding a git submodule
#'
#' @export
use_module <- function(path = NULL, repo = NULL) {
  ## TODO: add module from 'repo' as git submodule
  cli({
    cli_text("not yet implemented. please add manually using e.g.,")
    cli_code(
      "cd <path>",
      "git add submodule <user>/<repo>",
      "cd <repo>",
      "git switch <branch>",
      language = "bash"
    )
  })

  return(invisible(NULL))
}
