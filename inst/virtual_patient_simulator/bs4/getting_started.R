getting_started <- function() {
  withMathJax(
    HTML(
      paste(
        "<u><b>Hi there! Need some help? 
        (Click on the <i class = 'fa fa-info'></i> above to toggle me)</b></u>", br(), br(),
        "<b>1) Regulatory mechanisms:</b>", br(),
        shiny::tags$ul(
          shiny::tags$li(
            img(src = "help_img/node_help.svg", 
                height = "70px", width = "70px"), 
            "Organs involved in Ca and \\(P_i\\) metabolism"
          ),
          shiny::tags$li(
            img(src = "help_img/regulation_help.svg", 
                height = "60px", width = "60px"), 
            "Regulatory hormones and ions"
          ),
          shiny::tags$li(
            img(src = "help_img/dashed_arrow_help_promotor.svg", 
                height = "70px", width = "70px"), 
            "Promotor"
          ),
          shiny::tags$li(
            img(src = "help_img/dashed_arrow_help_inhibitor.svg", 
                height = "70px", width = "70px"), 
            "Inhibitor"
          ),
          shiny::tags$li(
            img(src = "help_img/dashed_arrow_help.svg", 
                height = "70px", width = "70px"), 
            "Mixed effect or opposite effects on \\([Ca]_p\\) and \\([P_i]_p\\)."
          )
        ),
        br(), 
        "<b>2) FLuxes and concentrations:</b>", br(),
        "\\([...]_p\\)", "Plasma concentrations", br(),
        shiny::tags$ul(
          shiny::tags$li(
            img(src = "help_img/arrow_help.svg", 
                height = "70px", width = "70px"), 
            "Ca and \\(P_i\\) fluxes"
          )
        ), 
        "Visualize changes in regulations: the arrow thickness increases if 
        the regulation is stronger, decreases if it is weaker:",
        shiny::tags$ul(
          shiny::tags$li(
            img(src = "help_img/red_arrow_help.svg", 
                height = "70px", width = "70px"),
            "if the flux is decreased", ","),
          shiny::tags$li(
            img(src = "help_img/arrow_help.svg", 
                height = "70px", width = "70px"),
            "if it is unaltered"
          ),
          shiny::tags$li(
            img(src = "help_img/green_arrow_help.svg", 
                height = "70px", width = "70px"),
            "if it is increased."
          )
        )
      )
    )
  )
}