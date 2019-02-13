#' @title Create a disease selector UI module
#'
#' @description Contains php1, hypopara, hypoD3
#'
#' @param id module id.
#'
#' @export
diseaseSelectUi <- function(id) {

  ns <- NS(id)

  tagList(
    h6("Steady-state simulations"),
    # primary hyperparathyroidism
    prettyCheckbox(
      inputId = ns("run_php1"),
      label = "Primary hyperparathyroidism",
      value = FALSE,
      animation = "pulse",
      thick = TRUE,
      status = "primary"
    ),

    # hypoparathyroidism
    prettyCheckbox(
      inputId = ns("run_hypopara"),
      label = "hypoparathyroidism",
      value = FALSE,
      animation = "pulse",
      thick = TRUE,
      status = "primary"
    ),

    # vitamin D3 deficiency
    prettyCheckbox(
      inputId = ns("run_hypoD3"),
      label = "25(OH)D deficiency",
      value = FALSE,
      animation = "pulse",
      thick = TRUE,
      status = "primary"
    )
  )
}




#' @title Create a disease selector server logic
#'
#' @description Only returns inputs associated with php1, hypopara, hypoD3
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @export
diseaseSelect <- function(input, output, session) {

  #  Prevent user from selecting multiple boxes using shinyjs functions
  observeEvent(c(input$run_php1, input$run_hypopara, input$run_hypoD3), {

    diseases <- list(php1 = input$run_php1, hypopara = input$run_hypopara, hypoD3 = input$run_hypoD3)
    # extract the list of simulations and the current one as well as its index
    # to properly select boxes to enable/disable
    current_simulation <- unlist(
      lapply(seq_along(diseases), FUN = function(i) {
        if (diseases[[i]]) names(diseases)[[i]]
      })
    )
    index <- which(names(diseases) == current_simulation)

    # if one simulation run, disable all boxes that are not related to that one
    if (!is.null(current_simulation)) {
      map(diseases[-index], disable)
    } else {# if no simulation runs, all boxes are available
      map(diseases, enable)
    }
  })


  return(
    list(
      php1 = reactive(input$run_php1),
      hypopara = reactive(input$run_hypopara),
      hypoD3 = reactive(input$run_hypoD3)
    )
  )
}
