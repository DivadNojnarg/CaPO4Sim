#' @title Launch the virtual patient simulator
#'
#' @description Unleash the virtual patient simulator
#'
#' @param context Choose between \code{c("introduction", "virtual-patient")}.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'  run_CardioRenalSim(context = "introduction")
#'  run_CardioRenalSim(context = "virtual-patient")
#' }
run_CardioRenalSim <- function(context = c("introduction", "virtual-patient")) {

  context <- match.arg(context)

  pkgs <- c(
    "shiny",
    "shinyjqui",
    "visNetwork",
    "plotly",
    "deSolve",
    "shinyjs",
    "shinycssloaders",
    "shinyWidgets",
    "shinyEffects",
    "bsplus",
    "purrr"
  )

  # set packages
  pkgs <- switch (
    context,
    "introduction" = pkgs <- c(pkgs, "shinydashboard", "shinydashboardPlus", "rintrojs", "magrittr", "DT"),
    "virtual-patient" = pkgs <- c(pkgs, "stringr", "shinyFeedback", "bs4Dash", "dplyr", "V8")
  )

  # handle missing packages
  lapply(seq_along(pkgs), FUN = function(i) {
    if (!requireNamespace(package = pkgs[[i]]))
      message(paste0("Package '", pkgs[[i]], "' is required to run this function"))
  })

  # run apps
  if (context == "introduction") {
    runApp(appDir = system.file("entry_level", package = 'CardioRenalSim', mustWork = TRUE))
  } else {
    runApp(appDir = system.file("virtual_patient_simulator/bs4", package = 'CardioRenalSim', mustWork = TRUE))
  }
}
