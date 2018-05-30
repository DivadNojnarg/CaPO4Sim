# *------------------------------------------------------------------
# | PROGRAM NAME: network_box.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This codes generates the network_box
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

network_box <- function() {
  column(
    width = 6, offset = 0, style = 'padding:0px;',
    box(
      id = "boxinfo", width = 12, solidHeader = TRUE,
      
      # back button for case studies
      column(
        width = 4, align = "left",
        conditionalPanel(
          condition = "input.run_php1 | input.run_hypopara | 
                            input.run_hypoD3 | input.help",
          introBox(
            actionBttn(
              inputId = "back1", 
              label = "Back", 
              style = "stretch", 
              color = "danger", 
              size = "md", 
              icon = icon("step-backward")
            ),
            data.step = 3,
            data.intro = help_text[3]
          )
        )
      ),
      
      # slider input for dynamic case studies
      column(
        width = 4, align = "center",
        introBox(
          uiOutput("counter_progress"),
          data.step = 4,
          data.intro = help_text[4]
        )
      ),
      
      # next button for case studies
      column(
        width = 4, align = "right",
        conditionalPanel(
          condition = "input.run_php1 | input.run_hypopara | 
                            input.run_hypoD3 | input.help",
          actionBttn(
            inputId = "next1", 
            label = "Next", 
            style = "stretch", 
            color = "danger", 
            size = "md", 
            icon = icon("step-forward")
          )
        )
      ),
      
      br(),
      
      # Main network
      introBox(
        div(
          id = "network_cap", # to insert a background image if needed
          withSpinner(
            visNetworkOutput(
              outputId = "network_Ca", 
              height = "900px"), 
            size = 2, 
            type = 8, 
            color = "#000000"
          )
        ),
        data.step = 2,
        data.intro = help_text[2],
        data.position = "right"
      )
    ) 
  )
}