# *------------------------------------------------------------------
# | PROGRAM NAME: graph_box.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the code for the box containing 
# |           all graphs, the slider to control disease intensity 
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

graph_box <- function() {
  column(
    width = 6, offset = 0, style = 'padding:0px;',
    box(
      id = "tabset1", width = 12, solidHeader = TRUE, height = "950px",
      
      # show some text to tell the user how to print the graphs
      uiOutput("info"),
      
      conditionalPanel(
        condition = "input.run_php1 | input.run_hypopara | 
                     input.run_hypoD3 | input.run_Ca_inject | 
                     input.run_PO4_inject | input.run_PO4_gav | input.help",
        
        # main graph
        column(
          width = 12,
          HTML(
            paste(
              "<b><mark><font color=\"#FF0000\">Steady-state</font></mark> concentrations and fluxes 
              normalized by the baseline normal values:</b>", br(),
              shiny::tags$ul(
                shiny::tags$li("Values > 1 : higher than normal"),
                shiny::tags$li("Values < 1 : lower than normal")
              )
            )
          ),
          hr(),
          introBox(
            withSpinner(
              plotlyOutput(
                outputId = "plot", 
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
              outputId = "slider", 
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