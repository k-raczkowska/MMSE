% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/committerMetrics.R
\name{sccfa}
\alias{sccfa}
\title{Compute the shortest chain of past failed committer's builds}
\usage{
sccfa(currentBuild)
}
\arguments{
\item{currentBuild}{id of travis-ci build}
}
\description{
This function computes the shortest chain of past failed
committer's builds.
Past failed committer's builds are builds which:
\itemize{
 \item{ran on the same repository as current build,}
 \item{commit which triggered these builds was made
 by the same person who made commit which triggered current build,}
 \item{completed with status 'failed' or 'errored'}
}
}
\examples{
sccfa(123456)
}

