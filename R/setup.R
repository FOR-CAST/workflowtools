#' Create a new project
#'
#' Every project will be a little different, and will require some level of customization.
#' Instead of trying to do everything for user, we provide suggestions for project setup.
#' Most of these follow the project setup suggestions in \pkg{usethis}.
#'
#' - `setup_machine()` prints a set of commands recommended for first time computer setup;
#' - `setup_project()` prints a set of commands recommended for initializing a new project.
#'
#' @param path character. path to the new project directory
#'
#' @param repo character. GitHub repository specification of the form `<user|org>/<repo>@<branch>`.
#'
#' @return `NULL`, invisibly. Used for the side-effect of printing project setup messages to screen.
#'
#' @export
#' @rdname setup
setup_machine <- function() {
  cli::cli({
    cli::cli_h1("Machine setup")

    cli::cli_li("reduce the risk of accidentally committing sensitive files.")
    cli::cli_code("usethis::git_vaccinate()")
    cli::cli_text("\n")

    cli::cli_li("use ssh instead of https when connecting to e.g. GitHub.")
    cli::cli_code("usethis::use_git_protocol('ssh')")
    cli::cli_text("\n")

    cli::cli_li("configures R to warn on partial matches.")
    cli::cli_code("usethis::use_partial_warnings()")
    cli::cli_text("\n")

    cli::cli_li("configure Rstudio from the commandline.")
    cli::cli_code("install.packages('rstudio.prefs')")
    cli::cli_code("?rstudio.prefs::use_rstudio_prefs")
  })
}

#' @export
#' @rdname setup
setup_project <- function(path = NULL, repo = NULL) {
  path <- ifelse(is.null(path), "", paste0("'", path, "'"))
  repo <- ifelse(is.null(repo), "", paste0("'", repo, "'"))

  cli::cli({
    ## machine setup
    setup_machine()

    ## new projects
    cli::cli_h1("New projects")
    cli::cli_li("create a new project.")
    cli::cli_code(paste0("usethis::create_project(", path, ")"))
    cli::cli_text("\n")

    ### modules and packages
    cli::cli_h2("Adding modules")
    cli::cli_li("use a SpaDES module within the project.")
    cli::cli_code("use_module(path = 'modules', repo = '<user|org>/<repo>@<branch>')")
    cli::cli_text("\n")

    cli::cli_li("Identify module package dependencies.")
    cli::cli_code(paste0("get_module_packages(path = 'modules')"))
    cli::cli_text("\n")

    ### additional files
    cli::cli_h2("Additional files")
    cli::cli_li("use README with this project.")
    cli::cli_code("usethis::use_readme_md()")
    cli::cli_text("\n")

    cli::cli_li("use NEWS with this project.")
    cli::cli_code("usethis::use_news_md()")
    cli::cli_text("\n")

    cli::cli_li("create DESCRIPTION file that captures package dependencies")
    cli::cli_code("description()")
    cli::cli_text("\n")

    ### git and github
    cli::cli_h2("Git and GitHub")
    cli::cli_li("use git with this project.")
    cli::cli_code("usethis::use_git()")
    cli::cli_text("\n")

    cli::cli_li("connect this project to a new GitHub repository.")
    cli::cli_code("usethis::use_github()")

    ### activate
    cli::cli_h2("Activate the project")
    cli::cli_li("activate the new project (will restart Rstudio).")
    cli::cli_code(paste0("usethis::proj_activate(", path, ")"))
    cli::cli_text("\n")

    ## existing projects
    cli::cli_h1("Existing projects")
    cli::cli_code(paste0("usethis::create_from_github(", repo, ")"))
    cli::cli_li("")
    cli::cli_text("\n")
  })
}
