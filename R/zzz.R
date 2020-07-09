.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the Cardio-Renal applications, have Fun!")
  addResourcePath('logos', system.file("logos", package = "CardioRenalSim"))
  addResourcePath('CardioRenal_network', system.file("CardioRenal_network", package = "CardioRenalSim"))
}
