dashboardControlbar <- rightSidebar(
  background = "dark",
  width = 300,
  rightSidebarTabContent(
    active = TRUE,
    id = "options",
    icon = "cogs",
    h4("Network options"),
    fluidRow(
      column(
        width = 6,
        uiOutput(outputId = "size_nodes_organs")
      ),
      column(
        width = 6,
        uiOutput(outputId = "size_nodes_hormones")
      )
    ),
    fluidRow(
      column(
        width = 6,
        uiOutput(outputId = "width_arrows_organs")
      ),
      column(
        width = 6,
        uiOutput(outputId = "width_arrows_hormones")
      )
    ),
    hr(),
    h4("Solver options"),
    numericInput(
      inputId = "tmax",
      label = "Maximum simulated time",
      value = 500,
      min = 0
    ),
    sliderInput(
      inputId = "t_now",
      label = "Time after simulation",
      min = 1,
      max = 500,
      value = 500
    ) %>%
      shinyInput_label_embed(
        icon("undo") %>%
          actionBttn(
            inputId = "reset_t_now",
            label = "", 
            color = "danger", 
            size = "xs"
          )
      )
    
  )
)