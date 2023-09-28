.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.workflowtools <- list(
    workflowtools.useRequire = FALSE
  )
  toset <- !(names(opts.workflowtools) %in% names(opts))
  if (any(toset)) options(opts.workflowtools[toset])

  invisible()
}
