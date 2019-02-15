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
infosUi <- function(id) {

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
#' @param diseases Shiny input disease selector. See \link{diseaseSelect}.
#' @param animation_counter Give the current temporal state of the animation. See \link{networkCaPO4}.
#' @param regulations Shiny input to toggle hormone display. See \link{networkOptions}.
#'
#' @importFrom shiny observeEvent observe showModal
#'
#' @export
infos <- function(input, output, session, diseases, animation_counter, regulations) {

  #-------------------------------------------------------------------------
  #  Educational content: modals, network and graph notifications,
  #     glossary
  #-------------------------------------------------------------------------

  # Modal for primary hyperparathyroidism, hypopara, ...
  # gives the user some extra information
  observeEvent(c(diseases$php1(), diseases$hypopara(), diseases$hypoD3()), {

    # show the modal related to the current running simulation
    current_sim <- extract_running_sim(diseases)
    req(current_sim)
    if (input$modal_switch) {
      showModal(eval(parse(text = paste("modal", current_sim, sep = "_"))))
    }
  })


  # Notification events for PHP1, hypoD3 and hypopara in the CaPO4 network
  # as well as the graph
  observeEvent(
    c(diseases$php1(), diseases$hypopara(), diseases$hypoD3(),
      animation_counter(), input$notif2_switch), {

        current_simulation <- extract_running_sim(diseases)
        req(current_simulation)

        if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {

          generate_notification(
            counter = animation_counter(),
            simulation = current_simulation,
            allowed = input$notif2_switch
          )
          # make it draggable
          jqui_draggable(selector = "#shiny-notification-notifid")
        }
      }, priority = 10)


  # indicates the user to enable regulations when he launches case studies
  # if they are not already enabled
  observeEvent(c(diseases$php1(), diseases$hypopara(), diseases$hypoD3()), {

      current_simulation <- extract_running_sim(diseases)
      input_current_simulation <- paste0("diseases$", current_simulation, "()")
      if (!is_empty(current_simulation)) {
        if (eval(parse(text = input_current_simulation)) & !regulations()) {
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
    if (animation_counter() == 6) {
      current_simulation <- extract_running_sim(diseases)
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
