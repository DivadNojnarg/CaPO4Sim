#' @title Launch the virtual patient simulator
#'
#' @description Unleash the virtual patient simulator
#'
#' @param context Choose between \code{c("introduction", "virtual-patient")}.
#' @param lib Which Bootstrap version to use. Either "bs3", default or "bs4"
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'  run_CaPO4Sim(context = "introduction")
#'  run_CaPO4Sim(context = "virtual-patient", lib = "bs4")
#' }
run_CaPO4Sim <- function(context = c("introduction", "virtual-patient"), lib = "bs3") {

  context <- match.arg(context)

  pkgs <- c(
    "shiny",
    "shinyjqui",
    "shinydashboard",
    "shinydashboardPlus",
    "visNetwork",
    "plotly",
    "deSolve",
    "shinyjs",
    "shinycssloaders",
    "shinyWidgets",
    "shinyEffects",
    "bsplus",
    "sweetalertR",
    "shinytoastr",
    "purrr",
    "rintrojs",
    "shinyFeedback"
  )

  # set packages
  pkgs <- switch (context,
    "introduction" = pkgs <- c(pkgs, "magrittr", "DT"),
    "virtual-patient" = pkgs
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
    if (lib == "bs3") {
      runApp(appDir = system.file("virtual_patient_simulator/bs3", package = 'CaPO4Sim', mustWork = TRUE))
    } else {
      runApp(appDir = system.file("virtual_patient_simulator/bs4", package = 'CaPO4Sim', mustWork = TRUE))
    }
  }
}
