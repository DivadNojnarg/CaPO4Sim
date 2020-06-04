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
    myCarousel(
      id = ns("carousel"),
      data.ride = FALSE,
      data.interval = FALSE,
      .list = lapply(seq_along(data$caption), FUN = function(i) {
        shinydashboardPlus::carouselItem(
          caption = data$caption[[i]],
          tags$iframe(
            src = data$src[[i]],
            frameborder="0",
            allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
            allowfullscreen=NA
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



#' @title carousel container
#'
#' @description Creates a carousel. Adapted from shinydashboardplus to also allow control of the carousel animation
#'
#' @param ... Slot for \link(carouselItem)
#' @param id Carousel id. Must be unique.
#' @param indicators Whether to display left and right indicators.
#' @param width Carousel width. 6 by default.
#' @param .list Should you need to pass \link{carouselItem} via \link{lapply} or similar,
#' put these item here instead of passing them in ...
#' @param data.interval specify data-interval in ms. 5000ms by default, set to "false" to prevent automated animation of the slides.
#' @param data.ride specify data-ride. "carousel" by default.
#'
#' @export
myCarousel <- function(..., id, indicators = TRUE, width = 6, .list = NULL, data.interval=5000, data.ride="carousel") {

  items <- c(list(...), .list)
  indicatorsId <- paste0("#", id)

  items[[1]]$attribs$class <- "item active"

  carouselTag <- shiny::tags$div(
    class = "carousel slide",
    id = id,
    `data-interval` = tolower(as.character(data.interval)),
    `data-ride` = tolower(as.character(data.ride)),
    shiny::tags$ol(
      class="carousel-indicators",
      lapply(
        seq_along(items),
        FUN = function(i) {
          shiny::tags$li(
            `data-target` = indicatorsId,
            `data-slide-to` = i - 1,
            class = ""
          )
        }
      )
    ),
    shiny::tags$div(class = "carousel-inner", items),
    # display indicators if needed
    if (indicators) {
      shiny::tagList(
        shiny::tags$a(
          class = "left carousel-control",
          href= indicatorsId,
          `data-slide` = "prev",
          shiny::tags$span(class="fa fa-angle-left")
        ),
        shiny::tags$a(
          class = "right carousel-control",
          href= indicatorsId,
          `data-slide` = "next",
          shiny::tags$span(class="fa fa-angle-right")
        )
      )
    }
  )

  shiny::column(width = width, carouselTag)

}




