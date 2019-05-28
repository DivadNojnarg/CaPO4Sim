#' @title Create a fullScreen UI module
#'
#' @description Trigger a fullScreen mode. Based on
#' https://stackoverflow.com/questions/42371164/how-to-run-r-shiny-app-in-full-sized-window
#'
#' @param id module id.
#'
#' @export
fullScreenUI <- function(id) {
  ns <- NS(id)
  tags$li(
    title = "",
    class = "dropdown",
    tagAppendAttributes(
      shinyWidgets::actionBttn(
        inputId = ns("fullscreen"),
        label = "",
        icon = icon("expand"),
        style = "fill",
        color = "default",
        size = "lg",
        block = FALSE,
        no_outline = TRUE
      ),
      onclick = "shinyjs.toggleFullScreen();"
    )
  )
}





#' @title Create a fullScreen server logic
#'
#' @description Nothing is contained inside for now...
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @export
fullScreen <- function(input, output, session) {}
