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
#' @param diseases Shiny input disease selector. See \link{diseaseSelect}.
#'
#' @export
userInfo <- function(input, output, session, diseases) {

  # generate a patient profile
  output$user <- renderUser({

    ns <- session$ns

    req(!is.null(diseases$php1()) | !is.null(diseases$hypopara()) | !is.null(diseases$hypoD3()))

    dashboardUser(
      name = "Rat State",
      src = if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        generate_userFields(input)$image
      } else {
        "images_patient_info/happy.png"
      },
      title = if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        generate_userFields(input)$description
      } else {
        "healthy"
      },
      subtitle = if (diseases$php1()) {
        "Rat has primary-hyperparathyroidism"
      } else if (diseases$hypopara()) {
        "Rat suffers from hypoparathyroidism"
      } else if (diseases$hypoD3()) {
        "Rat has vitamin D3 defficiency"
      } else {
        "nothing to declare!"
      },
      if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        dashboardUserItem(width = 6, generate_userFields(disease)$stat1)
      } else {
        dashboardUserItem(
          width = 6,
          HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
        )
      },
      if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        dashboardUserItem(width = 6, generate_userFields(disease)$stat2)
      } else {
        dashboardUserItem(
          width = 6,
          HTML(paste(withMathJax(p("$$[P_i]_p$$ 3 mM")), "<br/>", "(2.2-3.5 mM)"))
        )
      },
      if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        dashboardUserItem(width = 12, generate_userFields(disease)$stat3)
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
  })

  # reset all parameters
  observeEvent(input$cure,{
    reset("right_sidebar")
  })

}
