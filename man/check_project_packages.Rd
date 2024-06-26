% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packages.R
\name{check_project_packages}
\alias{check_project_packages}
\title{Check a project's packages against module dependencies}
\usage{
check_project_packages(path = NULL, snapshot = NULL)
}
\arguments{
\item{path}{character specifying the project path}

\item{snapshot}{character. path to a \pkg{renv} or \pkg{Require} snapshot file.}
}
\value{
\code{NULL}, invisibly. will signal an error if there is a discrepancy between
project and module packages.
}
\description{
Check a project's packages against module dependencies
}
