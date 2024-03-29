% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requestHandling.R
\name{checkESTAPIError}
\alias{checkESTAPIError}
\title{Get Google API errors}
\usage{
checkESTAPIError(
  req,
  ok_content_types = getOption("EventStudy.ok_content_types"),
  batched = FALSE
)
}
\arguments{
\item{req}{a httr request}

\item{ok_content_types}{Expected content type of request}

\item{batched}{called from gar_batch or not}
}
\description{
Get Google API errors
}
\keyword{internal}
