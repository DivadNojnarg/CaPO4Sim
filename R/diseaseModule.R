#' @title Create a disease selector UI module
#'
#' @description Contains php1, hypopara, hypoD3
#'
#' @param id module id.
#'
#' @export
diseaseSelectUi <- function(id) {

  ns <- NS(id)

  diseases <- list(
    labels = c("Baroreceptor Stimulation", "Haemorrhage", "High/Low Salt Diet"),
    ids = c("run_php1", "run_hypopara", "run_hypoD3")
  )

  tagList(
    h6("Case Studies"),
    lapply(seq_along(diseases$ids), function(i) {
      diseaseCheckBox(inputId = ns(diseases$ids[[i]]), label = diseases$labels[[i]])
    }),
    # reset button
    shinyWidgets::actionBttn(
      inputId = ns("cure"),
      label = "Cure Patient",
      style = "fill",
      icon = icon("medkit"),
      color = "success",
      block = TRUE,
    )
  )
}


#' @title Create a checkbox for \link{diseaseSelectUi}
#'
#' @description Create a \link[shinyWidgets]{prettyCheckbox}.
#'
#' @param inputId Checkbox Input id.
#' @param label Checkbox label.
#'
diseaseCheckBox <- function(inputId, label) {
  shinyWidgets::prettyCheckbox(
    inputId = inputId,
    label = label,
    value = FALSE,
    animation = "pulse",
    thick = TRUE,
    status = "primary"
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

    diseases <- list(
      run_php1 = input$run_php1,
      run_hypopara = input$run_hypopara,
      run_hypoD3 = input$run_hypoD3
    )
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
      lapply(seq_along(diseases[-index]), FUN = function(i) {
        shinyjs::disable(id = names(diseases[-index])[[i]])
      })
    } else {# if no simulation runs, all boxes are available
      lapply(seq_along(diseases), FUN = function(i) {
        shinyjs::enable(id = names(diseases)[[i]])
      })
    }
  })


  # reset rat state
  observeEvent(input$cure, {
    ids <- c("run_php1", "run_hypopara", "run_hypoD3")
    purrr::map(ids, shinyjs::reset)
  })


  return(
    list(
      php1 = reactive(input$run_php1),
      hypopara = reactive(input$run_hypopara),
      hypoD3 = reactive(input$run_hypoD3)
    )
  )
}
