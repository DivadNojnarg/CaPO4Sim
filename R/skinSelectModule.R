#' @title Dashboard skin selector, ui side
#'
#' @description Select the shinydashboard skin you want
#'
#' @param id module id.
#'
#' @export
skinSelectUi <- function(id) {

  ns <- NS(id)

  selectInput(
    inputId = ns("skin"),
    label = "Select a skin",
    choices = c(
      "blue", "black", "purple",
      "green", "red", "yellow"
    ),
    selected = "black"
  )
}




#' @title Dashboard skin selector, server side
#'
#' @description Select the shinydashboard skin you want
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @export
skinSelect <- function(input, output, session) {

  # change the dashboard skin
  current_skin <- reactiveValues(color = NULL)
  previous_skin <- reactiveValues(color = NULL)

  observeEvent(input$skin, {
    # the first time, previous_skin$color is set to the first
    # skin value at opening. Same thing for current value
    if (is.null(previous_skin$color)) {
      previous_skin$color <- current_skin$color <- input$skin
    } else {
      current_skin$color <- input$skin
      # if the old skin is the same as the current selected skin
      # then, do nothing
      if (previous_skin$color == current_skin$color) {
        NULL
      } else {
        # otherwise, remove the old CSS class and add the new one
        removeClass(
          selector = "body",
          class = paste("skin", previous_skin$color, sep = "-"))
        addClass(
          selector = "body",
          class = paste("skin", current_skin$color, sep = "-"))
        # the current skin is added to previous_skin to be ready for
        # the next change
        previous_skin$color <- c(previous_skin$color, current_skin$color)[-1]
      }
    }
  })
}
