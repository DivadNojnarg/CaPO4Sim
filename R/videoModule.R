#' @title Create a movie UI module
#'
#' @description Contains php1, hypopara, hypoD3
#'
#' @param id module id.
#' @param data Video data.
#'
#' @export
videoUi <- function(id, data) {

  # useless here, unless we add inputs in the future
  ns <- NS(id)

  fluidRow(
    shinydashboardPlus::carousel(
      id = ns("carousel"),
      .list = lapply(seq_along(video_data$caption), FUN = function(i) {
        shinydashboardPlus::carouselItem(
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
#' @export
video <- function(input, output, session) {}
