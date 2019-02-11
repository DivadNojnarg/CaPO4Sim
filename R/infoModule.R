#' @title Info UI module
#'
#' @description Create modals, alerts, ...
#'
#' @param id module id.
#'
#' @importFrom shiny tagList
#' @importFrom shinyWidgets prettySwitch
#'
#' @export
infoUi <- function(id) {

  ns <- NS(id)

  tagList(
    # Enable/Disable informations
    prettySwitch(
      inputId = ns("notif2_switch"),
      label = "Notifications?",
      value = TRUE,
      status = "success",
      slim = TRUE,
      bigger = TRUE
    ),
    prettySwitch(
      inputId = ns("modal_switch"),
      label = "Descriptions?",
      value = TRUE,
      status = "success",
      slim = TRUE,
      bigger = TRUE
    )
  )
}



#' @title Info server module
#'
#' @description Create modals, alerts, ...
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @importFrom shiny observeEvent observe showModal
#'
#' @export
info <- function(input, output, session) {
  #-------------------------------------------------------------------------
  #
  #  4) Educational content: modals, network and graph notifications,
  #     glossary
  #
  #-------------------------------------------------------------------------

  # Modal for primary hyperparathyroidism, hypopara, ...
  # gives the user some extra information
  observeEvent(c(input$run_php1, input$run_hypopara, input$run_hypoD3),{

    # extract only the last part of the simulation
    # so gives php1, hypopara, hypoD3, ...
    current_sim <- extract_running_sim(input)[[1]] %>%
      str_extract("_\\w+") %>%
      str_replace("_", "")

    req(current_sim)
    if (input$modal_switch == TRUE) {
      showModal(eval(parse(text = paste("modal", current_sim, sep = "_"))))
    }
  })


  # Notification events for PHP1, hypoD3 and hypopara in the CaPO4 network
  # as well as the graph
  observeEvent(
    c(input$run_php1, input$run_hypopara, input$run_hypoD3,
      counter_nav$diagram, input$notif2_switch), {

        current_simulation <- extract_running_sim(input)[[1]]
        req(current_simulation)

        if (input$run_php1 == "TRUE" | input$run_hypopara == "TRUE" |
            input$run_hypoD3 == "TRUE") {

          generate_notification(
            counter = counter_nav$diagram,
            simulation = current_simulation,
            allowed = input$notif2_switch
          )
          # make it draggable
          shinyjqui::jqui_draggable(selector = "#shiny-notification-notifid")
        }
      }, priority = 10)


  # indicates the user to enable regulations when he launches case studies
  # if they are not already enabled
  observeEvent(
    c(input$run_php1, input$run_hypopara, input$run_hypoD3), {

      current_simulation <- extract_running_sim(input)[[1]]
      input_current_simulation <- paste0("input$", extract_running_sim(input)[[1]])
      if (!is_empty(current_simulation)) {
        if (eval(parse(text = input_current_simulation)) &
            input$network_hormonal_choice == FALSE) {
          sendSweetAlert(
            session = session,
            type = "error",
            title = NULL,
            text = HTML(
              paste(
                "Before going deeper in the case study,
                please enable hormonal regulations
                in the option part and select both Ca and Pi",
                icon("sliders")
              )
            ),
            html = TRUE
          )
        }
      }
    })

  # indicate when a user has finished the current activity
  observe({
    if (counter_nav$diagram == 6) {
      current_simulation <- extract_running_sim(input)[[1]] %>%
        str_extract("_\\w+") %>%
        str_replace("_", "")
      input_current_simulation <- paste0("input$", extract_running_sim(input)[[1]])
      sendSweetAlert(
        session = session,
        type = "success",
        title = NULL,
        text = HTML(
          paste(
            "You just finished", current_simulation, "activity.
            You are free to replay the animation or choose another
            activity. Click on the <b>next</b> button again to reset the current
            activity"
          )
        ),
        html = TRUE
      )
    }
  })
}
