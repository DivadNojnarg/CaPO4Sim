graph_box <- function() {
  column(
    width = 6, offset = 0, style = 'padding:0px;',
    box(
      id = "tabset1", width = 12, solidHeader = TRUE, height = "950px",
      #verbatimTextOutput("test"),
      uiOutput("info"),
      
      conditionalPanel(
        condition = "input.run_php1 | input.run_hypopara | 
             input.run_hypoD3 | input.run_Ca_inject | 
             input.run_PO4_inject | input.run_PO4_gav | input.help",
        
        column(12, align = "center",
               introBox(
                 withSpinner(plotlyOutput("plot", height = "600px"), 
                             size = 2, type = 8, color = "#000000"),
                 data.step = 5,
                 data.intro = help_text[5]
               )
        ),
        column(4, align = "left"),
        column(4, align = "center",
               br(), br(), br(), br(), br(),
               introBox(
                 uiOutput("slider", class = "theme-orange"),
                 data.step = 6,
                 data.intro = help_text[6],
                 data.position = "left"
               )
        ),
        column(4, align = "right")
      )
    )
  )
}