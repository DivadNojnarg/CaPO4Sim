#' @title Help UI module
#'
#' @description Create a help button
#'
#' @param id module id.
#'
#' @export
helpCaPO4Ui <- function(id) {

  ns <- NS(id)

  tags$li(
    title = "",
    class = "dropdown",
    introBox(
      actionBttn(
        inputId = ns("help"),
        label = "Help",
        icon = NULL,
        style = "fill",
        color = "danger",
        size = "lg",
        block = FALSE,
        no_outline = TRUE
      ),
      data.step = 7,
      data.intro = help_text[7]
    )
  )
}




#' @title Help server module
#'
#' @description Create the help section
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @export
helpCaPO4 <- function(input, output, session) {
  # help animation with introjs
  # options are provided to control the size of the help
  # Do not forget to wrap the event content in I('my_function')
  # otherwise it will fail
  observeEvent(input$help,{
    introjs(
      session,
      options = list(
        "nextLabel" = "Next step!",
        "prevLabel" = "Did you forget something?",
        "tooltipClass" = "newClass",
        #"highlightClass" = "newClass",
        "showProgress" = TRUE,
        "showBullets" = FALSE
      ),
      events = list(
        # reset the session to hide sliders and back/next buttons
        "oncomplete" = I('history.go(0)'),
        "onbeforchange" = I("function(steps) { Shiny.onInputChange('current_step', data-stepnumber); }"),
        "onbeforechange" = I('
            if (targetElement.getAttribute("data-step") === "2") {
                $(".newClass").css("max-width", "800px").css("min-width","800px");
            } else {
                $(".newClass").css("max-width", "500px").css("min-width","500px");
        }')
      )
    )
  })


  #  Toggle the sidebar when a user press the help button
  observe({
    shinyjs::toggleClass(selector = "body", class = "control-sidebar-open", condition = input$help)
    shinyjs::runjs("$('.control-sidebar-tabs li:eq(0)').addClass('active')")
    shinyjs::runjs("$('.control-sidebar-tabs li:not(:eq(0))').removeClass('active')")
    #shinyjs::runjs("$('.control-sidebar-tabs li:eq(0) a').tab('show')")
    shinyjs::runjs("$('.controlbar.tab-content div:eq(0)').addClass('active')")
    shinyjs::runjs("$('.controlbar.tab-content div:not(:eq(0))').removeClass('active')")
  })

  return(reactive(input$help))

}
