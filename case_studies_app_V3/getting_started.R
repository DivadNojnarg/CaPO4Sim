getting_started <- function() {
  withMathJax(
    HTML(
      paste(
        "<u><b>Mathematical model of calcium and phosphate homeostasis in the rat:</b></u>", br(), br(),
        "1) Regulatory mechanisms:", br(),
        img(src = "rintrojs_help/node_help.svg", 
            height = "70px", width = "70px"), 
        "Organs involved in Ca and \\(P_i\\) metabolism", br(),
        img(src = "rintrojs_help/regulation_help.svg", 
            height = "70px", width = "70px"), 
        "Regulatory hormones and ions", br(),
        img(src = "rintrojs_help/dashed_arrow_help_promotor.svg", 
            height = "70px", width = "70px"), 
        "Promotor", br(),
        img(src = "rintrojs_help/dashed_arrow_help_inhibitor.svg", 
            height = "70px", width = "70px"), 
        "Inhibitor", br(),
        img(src = "rintrojs_help/dashed_arrow_help.svg", 
            height = "70px", width = "70px"), 
        "Mixed effect or opposite effects on \\([Ca]_p\\) and \\([P_i]_p\\). 
        Click on the detailed cellular view to see individual actions", br(),
        "\\([...]_p\\)", "Plasma concentrations", br(),
        img(src = "rintrojs_help/arrow_help.svg", 
            height = "70px", width = "70px"), 
        "Ca and \\(P_i\\) fluxes", br(), br(),
        "2) <mark><font color=\"#FF0000\"><b>Mouse over</b></font></mark> the organs to visualize detailed 
        intra-cellular regulatory pathways.", br(), br(),
        "3) Visualize the consequences of selected 
        <mark><font color=\"#FF0000\"><b>pathological 
        disorders</b></font></mark>", br(),
        shiny::tags$ul(
          shiny::tags$li(
            HTML(
              paste(
                "Got to tool", icon("gears"), ",", 
                "click on case studies", icon("map"), "and select the pathology")
            )
          ),
          shiny::tags$li(
            paste(
              "Visualize changes in regulations: The arrow thickness increases if 
              the regulation is stronger, decreases if it is weaker")
          ),
          shiny::tags$li(
            "Visualize changes in Ca and \\(P_i\\) fluxes",
            shiny::tags$ul(
              shiny::tags$li(
                img(src = "rintrojs_help/red_arrow_help.svg", 
                    height = "70px", width = "70px"),
                "if the flux is decreased"), br(),
              shiny::tags$li(
                img(src = "rintrojs_help/arrow_help.svg", 
                    height = "70px", width = "70px"),
                "if the flux is unaltered"), br(),
              shiny::tags$li(
                img(src = "rintrojs_help/green_arrow_help.svg", 
                    height = "70px", width = "70px"),
                "if the flux is increased"), br()
            )
          )
        )
      )
    )
  )
}