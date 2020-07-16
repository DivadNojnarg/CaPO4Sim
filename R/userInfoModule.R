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
    shinydashboardPlus::userOutput(ns("user")),
    style = "margin-left: 100px; border:none;"
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
#' @param sliderDisease Shiny input disease severity selector. See \link{plotBox}.
#' @param help Help input.
#'
#' @export
userInfo <- function(input, output, session, diseases, sliderDisease, help) {

  # generate a patient profile
  output$user <- shinydashboardPlus::renderUser({

    ns <- session$ns

    req(!is.null(diseases$php1()) | !is.null(diseases$hypopara()) | !is.null(diseases$hypoD3()))

    shinydashboardPlus::dashboardUser(
      name = "Vital Parameters",
      src = if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        generate_userFields(diseases, sliderDisease)$image
      } else {
        "images_patient_info/happy.png"
      },
      title = if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        generate_userFields(diseases, sliderDisease)$description
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
        shinydashboardPlus::dashboardUserItem(width = 6, generate_userFields(diseases, sliderDisease)$stat1)
      } else {
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          HTML(paste("<p style=\"text-align:center;line-height:2.0\">",
                     "<font face =\"TimesNewRoman\" size=\"+1\">[<em><b>Ca<sup>2+</sup></b></em>]<sub><em><b>p</b></em></sub></font>","<br>",
                     "1.21 mM","<br>",
                     "(1.1-1.4 mM)"))
        )
      },
      if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        shinydashboardPlus::dashboardUserItem(width = 6, generate_userFields(diseases, sliderDisease)$stat2)
      } else {
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          HTML(paste("<p style=\"text-align:center;line-height:2.0\">",
                     "<font face =\"TimesNewRoman\" size=\"+1\">[<em><b>P<sub>i</sub></b></em>]<sub><em><b>p</b></em></sub></font>","<br>",
                     "2.96 mM","<br>",
                     "(2.1-3.4 mM)"))
        )
      },
      if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
        shinydashboardPlus::dashboardUserItem(width = 12, generate_userFields(diseases, sliderDisease)$stat3)
      } else {
        shinydashboardPlus::dashboardUserItem(
          width = 12,
          HTML(paste("<br>",
                     "<p style=\"text-align:center;line-height:2.0\">",
                     "<font face =\"TimesNewRoman\" size=\"+1\">[<em><b>PTH</b></em>]<sub><em><b>p</b></em></sub></font>","<br>",
                     "6.87 pM","<br>",
                     "(3-16 pM)"))
        )
      },
      br()
    )
  })


  # Open the userInfo menu. Useless since rintrojs does not work with modules
  #observeEvent(help(), {
  #  shinyjs::toggleClass(
  #    id = "user",
  #    class = "user-menu open",
  #    condition = help()
  #  )
  #})

}
