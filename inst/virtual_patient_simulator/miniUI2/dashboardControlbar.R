dashboardControlbar <- bs4DashControlbar(

  skin = "light",
  title = NULL,
  width = 250,

  h4("Network options", align = "center"), br(),
  prettyCheckboxGroup(
    inputId = "background_choice",
    label = "Network background",
    choices = c("human"),
    thick = TRUE,
    animation = "pulse",
    selected = "human",
    inline = TRUE
  ),
  prettyCheckboxGroup(
    inputId = "network_Ca_choice",
    label = "Select a network",
    choices = c(
      "Ca" = "Ca",
      "Pi" = "PO4",
      "PTH" = "PTH",
      "D3" = "D3",
      "FGF23" = "FGF23"
    ),
    thick = TRUE,
    animation = "pulse",
    selected = "rat",
    inline = TRUE
  ),
  prettySwitch(
    inputId = "network_hormonal_choice",
    label = "Display hormones",
    value = TRUE,
    slim = TRUE,
    bigger = TRUE
  ),
  prettySwitch(
    inputId = "network_organ_choice",
    label = "Display organs",
    value = TRUE,
    slim = TRUE,
    bigger = TRUE
  ),
  hr(),
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
  h4("Solver options", align = "center"), br(),
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
