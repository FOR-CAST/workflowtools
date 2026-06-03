utils::globalVariables(c(
  ".", "Package", "Repo", "Version", "Version_Mod", "Version_Prj"
))

#' @keywords internal
"_PACKAGE"

## All function calls in R/ are namespaced via pkg::fun().
## Only the operators / NSE symbols / sentinel values that cannot be called
## with `::` are imported here.

## usethis namespace: start
#' @importFrom data.table := .SD
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
