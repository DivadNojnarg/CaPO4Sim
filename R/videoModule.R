#' @title Create a movie UI module
#'
#' @description Contains php1, hypopara, hypoD3
#'
#' @param id module id.
#' @param data Video data.
#'
#' @importFrom shiny NS tagList h6 fluidRow tags
#' @importFrom shinyWidgets prettyCheckbox
#' @importFrom shinydashboardPlus carousel carouselItem
#'
#' @export
videoUi <- function(id, data) {

  # useless here, unless we add inputs in the future
  ns <- NS(id)

  fluidRow(
    carousel(
      id = ns("carousel"),
      .list = lapply(seq_along(video_data$caption), FUN = function(i) {
        carouselItem(
          caption = video_data$caption[[i]],
          tags$iframe(
            width = "100%",
            height = "450",
            src = video_data$src[[i]],
            frameborder = "0",
            `allowfullscreen` <- NA
          )
        )
      })
    )
  )
}




#' @title Create a video server logic
#'
#' @description Nothing is contained inside for now...
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @importFrom shiny reactive
#'
#' @export
video <- function(input, output, session) {}
