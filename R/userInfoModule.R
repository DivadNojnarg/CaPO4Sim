#' @title CaPO4 user info UI module
#'
#' @description Create a CaPO4 user info card
#'
#' @param id module id.
#'
#' @export
userInfoUi <- function(id) {

  ns <- NS(id)

  tagAppendAttributes(
    userOutput(ns("user")),
    style = "margin-left: 100px; border:none;"#,
    #uiOutput(ns("cureUser"))
  )
}




#' @title CaPO4 user info server module
#'
#' @description Create a CaPO4 user info card
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @export
userInfo <- function(input, output, session) {

  # generate a patient profile
  output$user <- renderUser({

    ns <- session$ns

    if (!is.null(input$run_php1) | !is.null(input$run_hypopara) | !is.null(input$run_hypoD3)) {
      dashboardUser(
        name = "Rat State",
        src = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
          generate_userFields(input)$image
        } else {
          "images_patient_info/happy.png"
        },
        title = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
          generate_userFields(input)$description
        } else {
          "healthy"
        },
        subtitle = if (input$run_php1) {
          "Rat has primary-hyperparathyroidism"
        } else if (input$run_hypopara) {
          "Rat suffers from hypoparathyroidism"
        } else if (input$run_hypoD3) {
          "Rat has vitamin D3 defficiency"
        } else {
          "nothing to declare!"
        },
        if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
          dashboardUserItem(width = 6, generate_userFields(input)$stat1)
        } else {
          dashboardUserItem(
            width = 6,
            HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
          )
        },
        if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
          dashboardUserItem(width = 6, generate_userFields(input)$stat2)
        } else {
          dashboardUserItem(
            width = 6,
            HTML(paste(withMathJax(p("$$[P_i]_p$$ 3 mM")), "<br/>", "(2.2-3.5 mM)"))
          )
        },
        if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
          dashboardUserItem(width = 12, generate_userFields(input)$stat3)
        } else {
          dashboardUserItem(
            width = 12,
            HTML(paste(withMathJax(p("$$[PTH]_p$$ 66 ng/l")), "<br/>", "(20-70 ng/l)"))
          )
        },
        dashboardUserItem(
          width = 12,
          actionBttn(
            inputId = ns("cure"),
            label = "Cure Rat",
            style = "fill",
            color = "success"
          )
        )
      )
    }
  })

  # reset all parameters
  observeEvent(input$cure,{
    reset("right_sidebar")
  })

}
