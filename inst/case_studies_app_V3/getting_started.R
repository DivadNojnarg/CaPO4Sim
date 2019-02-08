getting_started <- function() {
  withMathJax(
    HTML(
      paste(
        "<u><b>Mathematical model of calcium and phosphate homeostasis in the rat:</b></u>", br(), br(),
        "<b>1) Regulatory mechanisms:</b>", br(),
        img(src = "rintrojs_help/node_help.svg", 
            height = "70px", width = "70px"), 
        "Organs involved in Ca and \\(P_i\\) metabolism", br(),
        img(src = "rintrojs_help/regulation_help.svg", 
            height = "60px", width = "60px"), 
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
        Click on the detailed cellular view to see individual actions", br(), br(),
        "<b>2) FLuxes and concentrations:</b>", br(),
        "\\([...]_p\\)", "Plasma concentrations", br(),
        img(src = "rintrojs_help/arrow_help.svg", 
            height = "70px", width = "70px"), 
        "Ca and \\(P_i\\) fluxes", br(), br(),
        "<b>3) Explore <mark><font color=\"#FF0000\">regulatory pathways</font></mark>:</b>", br(),
        shiny::tags$ul(
          shiny::tags$li(
            HTML(
              paste(
                "<b><mark><font color=\"#FF0000\">Mouse 
                over</font></mark></b> the organs to visualize detailed 
                intra-cellular regulatory pathways</b>"
              )
            )
          ),
          shiny::tags$li(HTML(paste("Open the right sidebar by clicking on", icon("gears")))),
          shiny::tags$li(HTML(paste("Select the first tab", icon("sliders")))),
          shiny::tags$li(paste("Play with the different options (enable/disable regulations,
        display/hide organs)"))
        ),
        br(), br(),
        "<b>4) Visualize the consequences of selected 
        <mark><font color=\"#FF0000\">pathological 
        disorders</font></mark>:</b>", br(),
        shiny::tags$ul(
          shiny::tags$li(
            HTML(
              paste(
                "Open the right sidebar", icon("gears"), ",", 
                "click on case studies", icon("map"), "and select the pathology")
            )
          ),
          shiny::tags$li(
            paste(
              "Visualize changes in regulations: the arrow thickness increases if 
              the regulation is stronger, decreases if it is weaker")
          ),
          shiny::tags$li(
            "Visualize changes in Ca and \\(P_i\\) fluxes:", br(),
            img(src = "rintrojs_help/red_arrow_help.svg", 
                height = "70px", width = "70px"),
            "if the flux is decreased", ",",
            img(src = "rintrojs_help/arrow_help.svg", 
                height = "70px", width = "70px"),
            "if it is unaltered", "or",
            img(src = "rintrojs_help/green_arrow_help.svg", 
                height = "70px", width = "70px"),
            "if it is increased."
          )
        )
      )
    )
  )
}