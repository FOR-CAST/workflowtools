#' Deprecated function names
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were renamed in workflowtools 0.0.10 to use snake_case
#' and prefix-grouped families (`project_*()` for project accessors,
#' `info_*()` for introspection). Use the new names instead; the old names
#' delegate to them with a one-time-per-session warning.
#'
#' | Old                       | New                     |
#' |---------------------------|-------------------------|
#' | `findProjectPath()`       | [project_path()]        |
#' | `findProjectName()`       | [project_name()]        |
#' | `gitInfo()`               | [info_git()]            |
#' | `submoduleInfo()`         | [info_submodules()]     |
#' | `sessInfo()`              | [info_session()]        |
#' | `spatialLibs()`           | [info_spatial_libs()]   |
#' | `projectSessionInfo()`    | [info_project()]        |
#' | `reproducibilityReceipt()`| [reproducibility_receipt()] |
#'
#' @keywords internal
#' @name workflowtools-deprecated
NULL

#' @export
#' @rdname workflowtools-deprecated
findProjectPath <- function() {
  lifecycle::deprecate_warn("0.0.10", "findProjectPath()", "project_path()")
  project_path()
}

#' @export
#' @rdname workflowtools-deprecated
findProjectName <- function() {
  lifecycle::deprecate_warn("0.0.10", "findProjectName()", "project_name()")
  project_name()
}

#' @export
#' @rdname workflowtools-deprecated
gitInfo <- function(prjDir = NULL) {
  lifecycle::deprecate_warn("0.0.10", "gitInfo()", "info_git()")
  info_git(prjDir = prjDir)
}

#' @export
#' @rdname workflowtools-deprecated
submoduleInfo <- function(prjDir = NULL) {
  lifecycle::deprecate_warn("0.0.10", "submoduleInfo()", "info_submodules()")
  info_submodules(prjDir = prjDir)
}

#' @export
#' @rdname workflowtools-deprecated
sessInfo <- function() {
  lifecycle::deprecate_warn("0.0.10", "sessInfo()", "info_session()")
  info_session()
}

#' @export
#' @rdname workflowtools-deprecated
spatialLibs <- function() {
  lifecycle::deprecate_warn("0.0.10", "spatialLibs()", "info_spatial_libs()")
  info_spatial_libs()
}

#' @export
#' @rdname workflowtools-deprecated
projectSessionInfo <- function(prjDir = NULL) {
  lifecycle::deprecate_warn("0.0.10", "projectSessionInfo()", "info_project()")
  info_project(prjDir = prjDir)
}

#' @export
#' @rdname workflowtools-deprecated
reproducibilityReceipt <- function(
  prjDir = NULL,
  title = "Reproducibility receipt",
  writeTo = NULL
) {
  lifecycle::deprecate_warn("0.0.10", "reproducibilityReceipt()", "reproducibility_receipt()")
  reproducibility_receipt(prjDir = prjDir, title = title, writeTo = writeTo)
}
