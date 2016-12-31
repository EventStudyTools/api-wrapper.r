.onAttach = function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("EventStudy is still in beta version - APIs can be changed.")
  }
}
