#' @title plot box UI module
#'
#' @description Create modals, alerts, ...
#'
#' @param id module id.
#'
#' @export
plotBoxUi <- function(id) {

  ns <- NS(id)

  column(
    width = 6,
    offset = 0,
    style = 'padding:0px;',
    box(
      id = "tabset1",
      width = 12,
      solidHeader = TRUE,
      height = "950px",

      # show some text to tell the user how to print the graphs
      uiOutput(ns("info")),

      conditionalPanel(
        # be careful about the namespace
        # (need to update manually if the father module id is updated)
        condition = "
          input['diseases-run_php1'] |
          input['diseases-run_hypopara'] |
          input['diseases-run_hypoD3'] |
          input['help_section-help']
        ",
        # main graph
        column(
          width = 12,
          HTML(
            paste(
              "<b><mark><font color=\"#FF0000\">Steady-state</font></mark> concentrations and fluxes
              normalized by the baseline normal values:</b>", br(),
              tags$ul(
                tags$li("Values > 1 : higher than normal"),
                tags$li("Values < 1 : lower than normal")
              )
            )
          ),
          hr(),
          introBox(
            withSpinner(
              plotlyOutput(
                outputId = ns("plot"),
                height = "600px"
              ),
              size = 2,
              type = 8,
              color = "#000000"),
            data.step = 5,
            data.intro = help_text[5],
            data.position = "left"
          ),
          hr(),
          HTML("<u><b>Mouse over the curves</b></u> or <u><b>move the slider</b></u> below
          to read normalized plasma concentrations and fluxes
          corresponding to different severities of the disease:"
          )
        ),
        column(width = 4, align = "left"),

        # slider to control the disease intensity
        column(
          width = 4, align = "center",
          br(),
          introBox(
            uiOutput(
              outputId = ns("slider_disease"),
              class = "theme-orange"
            ),
            data.step = 6,
            data.intro = help_text[6],
            data.position = "left"
          )
        ),
        column(width = 4, align = "right")
      )
    )
  )
}





#' @title plot box server module
#'
#' @description Create modals, alerts, ...
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#' @param diseases Shiny input disease selector. See \link{diseaseSelect}.
#' @param help Help input.
#'
#' @export
plotBox <- function(input, output, session, diseases, help) {

  ns <- session$ns

  #-------------------------------------------------------------------------
  # Create slider for diseases (needed by plots)
  #-------------------------------------------------------------------------

  # Generate sliders for php1, hypopara and hypoD3 and even help
  slider <- reactive({

    req(!is.null(diseases))

    current_sim <- extract_running_sim(diseases)

    if (diseases$php1() | diseases$hypopara() | diseases$hypoD3() | help()) {


      sliderChoices <- if (diseases$php1() | help()) c(20, 100, 200) else c(0.5, 0.1, 0)
      sliderValue <- if (help()) {
        100
      } else {
        if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
          if (diseases$php1()) {
            100
          } else {
            0
          }
        } else {
          1
        }
      }

      sliderId <- ifelse(help(), "slider_help", paste0("slider_", current_sim))

      sliderTag <- sliderTextInput(
        inputId = ns(sliderId),
        label = if (diseases$php1() | help()) {
          "PTH mRNA synthesis fold increase"
        } else if (diseases$hypopara()) {
          "PTH mRNA synthesis fold decrease"
        } else if (diseases$hypoD3()) {
          "25(OH)D stock"
        },
        choices = sliderChoices,
        selected = sliderValue,
        grid = TRUE
      )

      return(list(sliderTag, sliderId))

    }
  })


  output$slider_disease <- renderUI(slider()[[1]])

  #-------------------------------------------------------------------------
  # Create plots
  #-------------------------------------------------------------------------

  observe({
    #current_sim <- extract_running_sim(diseases)
    #req(current_sim)
    #print(input[[slider()[[3]]]])
  })

  # draw each of the 6 plots as a function of the selected simulation
  output$plot <- renderPlotly({

    # extract the current simulation
    current_sim <- extract_running_sim(diseases)
    req(current_sim)
    # take dependency on the related slider and store its value
    sliderValue <- input[[slider()[[2]]]]
    req(sliderValue)

    # avoid that plotly returns an error when current_sim is empty
    if (!is_empty(current_sim)) {
      eval(parse(text = paste0("make_plot_", current_sim, "(sliderVal = ", sliderValue, ")")))
    } else {
      if (help()) {
        make_plot_php1(sliderVal = sliderValue)
      }
    }
  })



  # Print a short help text in the graph part
  output$info <- renderUI({
    if (sum(c(diseases$php1(), diseases$hypopara(), diseases$hypoD3())) == 0 && help() == 0) {
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
            tags$ul(
              tags$li(
                HTML(
                  paste(
                    "<b><mark><font color=\"#FF0000\">Mouse
                over</font></mark></b> the organs to visualize detailed
                intra-cellular regulatory pathways</b>"
                  )
                )
              ),
              tags$li(HTML(paste("Open the right sidebar by clicking on", icon("gears")))),
              tags$li(HTML(paste("Select the first tab", icon("sliders")))),
              tags$li(paste("Play with the different options (enable/disable regulations,
        display/hide organs)"))
            ),
            br(), br(),
            "<b>4) Visualize the consequences of selected
        <mark><font color=\"#FF0000\">pathological
        disorders</font></mark>:</b>", br(),
            tags$ul(
              tags$li(
                HTML(
                  paste(
                    "Open the right sidebar", icon("gears"), ",",
                    "click on case studies", icon("map"), "and select the pathology")
                )
              ),
              tags$li(
                paste(
                  "Visualize changes in regulations: the arrow thickness increases if
              the regulation is stronger, decreases if it is weaker")
              ),
              tags$li(
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
  })

  # return slider disease
  return(reactive(input[[slider()[[2]]]]))
}
