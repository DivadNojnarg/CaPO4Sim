#' @title Create a disease selector UI module
#'
#' @description Contains php1, hypopara, hypoD3
#'
#' @param id module id.
#'
#' @importFrom shiny NS tagList h6
#' @importFrom shinyWidgets prettyCheckbox
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
#' @importFrom shiny reactive observeEvent
#' @importFrom purrr map
#'
#' @export
diseaseSelect <- function(input, output, session) {

  #  Prevent user from selecting multiple boxes using shinyjs functions
  observeEvent(eval(parse(text = paste0("input$", extract_running_sim(input)[[2]]))), {

    # extract the list of simulations and the current one as well as its index
    # to properly select boxes to enable/disable
    sim_list <- extract_running_sim(input)[[2]]
    current_simulation <- extract_running_sim(input)[[1]]
    index <- which(sim_list == current_simulation)

    # if one simulation run, disable all boxes that are not related to that one
    if (!is_empty(current_simulation)) {
      temp <- eval(parse(text = paste0("input$", current_simulation)))
      if (temp == "TRUE") {
        map(sim_list[-index], disable)
      }
    } else {# if no simulation runs, all boxes are available
      map(sim_list, enable)
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
