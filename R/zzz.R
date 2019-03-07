.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the Calcium Phosphate simulator, have Fun!")
  addResourcePath('logos', system.file("logos", package = "CaPO4Sim"))
}
