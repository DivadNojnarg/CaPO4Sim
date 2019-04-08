#' @title Launch the virtual patient simulator
#'
#' @description Unleash the virtual patient simulator
#'
#' @param context Choose between \code{c("introduction", "virtual-patient")}.
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'  run_CaPO4Sim(context = "introduction")
#'  run_CaPO4Sim(context = "virtual-patient")
#' }
run_CaPO4Sim <- function(context = c("introduction", "virtual-patient")) {

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
    "virtual-patient" = pkgs <- c(pkgs, "stringr", "shinyFeeback", "bs4Dash", "dplyr", "V8")
  )

  # handle missing packages
  lapply(seq_along(pkgs), FUN = function(i) {
    if (!requireNamespace(package = pkgs[[i]]))
      message(paste0("Package '", pkgs[[i]], "' is required to run this function"))
  })

  # run apps
  if (context == "introduction") {
    runApp(appDir = system.file("entry_level", package = 'CaPO4Sim', mustWork = TRUE))
  } else {
    runApp(appDir = system.file("virtual_patient_simulator/bs4", package = 'CaPO4Sim', mustWork = TRUE))
  }
}
