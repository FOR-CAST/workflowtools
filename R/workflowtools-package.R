utils::globalVariables(c(
  ".", "Package", "Repo", "Version", "Version_Mod", "Version_Prj"
))

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import methods
#' @importFrom cli cli cli_code cli_h1 cli_h2 cli_li cli_text
#' @importFrom data.table := as.data.table data.table rbindlist set setkey setnames
#' @importFrom dplyr bind_rows filter pull
#' @importFrom fs dir_info dir_ls path_dir path_expand path_file path_norm path_rel
#' @importFrom furrr future_map_dfr future_map2_dfr
#' @importFrom googledrive drive_ls drive_mkdir drive_put
#' @importFrom httr config set_config
#' @importFrom remotes parse_github_repo_spec
#' @importFrom rprojroot find_root from_wd is_git_root is_rstudio_project
#' @importFrom sessioninfo session_info
#' @importFrom stats na.omit
#' @importFrom tools file_ext file_path_sans_ext toTitleCase
#' @importFrom usethis proj_activate use_news_md use_readme_md
#' @importFrom utils capture.output globalVariables modifyList read.csv
## usethis namespace: end
NULL
