.onLoad <- function(libname, pkgname) {
  op <- options()
  op.EventStudy <- list(
    EventStudy.URL                     = "https://api.eventstudytools.com/",
    EventStudy.KEY                     = NULL,
    EventStudy.verbose                 = 3,
    EventStudy.tryAttempts             = 5,
    EventStudy.ok_content_types        = c("application/json", ("text/html; charset=UTF-8")),
    EventStudy.rawResponse             = FALSE,
    EventStudy.jsonlite.simplifyVector = TRUE,
    EventStudy.test_statistics         = c("Patell Z",
                                           "Generalized Sign Z", 
                                           "Csect T", 
                                           "StdCSect Z", 
                                           "Rank Z",
                                           "Generalized Rank T",
                                           "Adjusted Patell Z",
                                           "Adjusted StdCSect Z",
                                           "Generalized Rank Z",
                                           "Skewness Corrected T")
  )
  toset <- !(names(op.EventStudy) %in% names(op))
  if(any(toset)) options(op.EventStudy[toset])
  invisible()
}

.onAttach = function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Dates before 01.01.1970 are currently not working.")
  }
}
