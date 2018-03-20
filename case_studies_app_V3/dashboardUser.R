#' Create a dashboard user profile.
#'
#' @param name userName.
#' @param image user profile picture.
#'
#' @seealso \code{\link{userOutput}} and \code{\link{renderUser}} for dynamically-generating \code{\link{dashboardUser}}.
#' @export
dashboardUser <- function(name = "Guang Yang",
                          image = paste0("ygdashboard", "-", as.character(utils::packageVersion("ygdashboard")), "/img/yg.jpg"),
                          description = "Alchemist",
                          sub_description = "Member since Aug. 2015",
                          stat1 = "100 Posts",
                          stat2 = "2048 Followers",
                          stat3 = "1024 Following",
                          stat4 = "Blabla",
                          btn1 = "Profile",
                          btn2 = "Sign Out") {
  
  # create user account menu
  tags$li(
    class = "dropdown user user-menu",
    style = "padding-right: 200px;",
    # menu toggle button
    tags$a(
      href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
      # user img and name in navbar (controlbar - header)
      tags$img(src = image, class = "user-image", alt = "User Image"),
      tags$span(class = "hidden-xs", name)
    ),
    # menu dropdown main
    tags$ul(
      class = "dropdown-menu",
      # user img in the menu
      tags$li(
        class = "user-header",
        tags$img(src = image, class = "img-circle", alt = "User Image"),
        tags$p(paste0(name, " - ", description), tags$small(sub_description))
      ),
      # menu body
      tags$li(
        class = "user-body",
        tags$div(
          class = "row",
          tags$div(
            class = "col-xs-6 text-center",
            tags$a(href = "#", stat1)
          ),
          tags$div(
            class = "col-xs-6 text-center",
            tags$a(href = "#", stat2)
          )
        ),
        tags$hr(),
        tags$div(
          class = "row",
          tags$div(
            class = "col-xs-6 text-center",
            tags$a(href = "#", stat3)
          ),
          tags$div(
            class = "col-xs-6 text-center",
            tags$a(href = "#", stat4)
          )
        )
      ),
      # menu footer. Do not show if the patient is healthy
      if (description %in% c("sick", "dead")) {
        tags$li(
          class = "user-footer",
          tags$div(
            #class = "pull-left",
            column(12, align = "center",
                   uiOutput("userbttn1")
            )
          )
        )
      }
    )
  )
}

#' Create a dynamic user output for ygdashboard (client side)
#'
#' This can be used as a placeholder for dynamically-generated \code{\link{dashboardUser}}.
#'
#' @param outputId Output variable name.
#' @param tag A tag function, like \code{tags$li} or \code{tags$ul}.
#'
#' @seealso \code{\link{renderUser}} for the corresponding server side function
#'   and examples.
#' @family user outputs
#' @export
userOutput <- function(outputId) {
  moduleOutput(outputId, tag = tags$li)
}

#' Create dynamic user output (server side)
#'
#' @inheritParams shiny::renderUI
#'
#' @seealso \code{\link{userOutput}} for the corresponding client side function
#'   and examples.
#' @family user outputs
#' @export
renderUser <- shiny::renderUI