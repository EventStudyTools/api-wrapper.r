.onLoad <- function(libname, pkgname) {
  op <- options()
  op.EventStudy <- list(
    EventStudy.URL                     = "http://api.eventstudytools.com",
    EventStudy.KEY                     = NULL,
    EventStudy.verbose                 = 3,
    EventStudy.tryAttempts             = 5,
    EventStudy.ok_content_types        = c("application/json", ("text/html; charset=UTF-8")),
    EventStudy.rawResponse             = FALSE,
    EventStudy.jsonlite.simplifyVector = TRUE
  )
  toset <- !(names(op.EventStudy) %in% names(op))
  if(any(toset)) options(op.EventStudy[toset])
  invisible()
}

.onAttach = function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("EventStudy is still in alpha version - APIs can be changed. \nDates before 01.01.1970 are currently not working.")
  }
}
