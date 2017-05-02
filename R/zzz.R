.onLoad <- function(libname, pkgname) {
  
  op <- options()
  op.EventStudy <- list(
    EventStudy.URL = "http://api.eventstudytools.com",
    EventStudy.verbose = 3,
    EventStudy.tryAttempts = 5
  )
  toset <- !(names(op.EventStudy) %in% names(op))
  if(any(toset)) options(op.EventStudy[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  
  default_scopes <- getOption("googleAuthR.scopes.selected")
  
  googleAuthR::gar_attach_auto_auth(default_scopes)
  
}

.onAttach = function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("EventStudy is still in beta version - APIs can be changed.")
  }
}
