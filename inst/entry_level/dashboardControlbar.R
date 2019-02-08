# *------------------------------------------------------------------
# | PROGRAM NAME: dashboardControlbar.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the right dashboard code
# |
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)
# |
# |
# *------------------------------------------------------------------
rightsidebar <- rightSidebar(
  background = "dark",
  width = 230,
  rightSidebarTabContent(
    id = 1,
    active = TRUE,
    icon = "sliders",
    # background choice
    introBox(
      data.step = 10,
      data.intro = help_text[10],
      prettyCheckboxGroup(
        inputId = "background_choice",
        label = "Background",
        choices = c("rat", "human"),
        animation = "pulse",
        thick = TRUE,
        status = "primary",
        inline = TRUE,
        selected = "rat"
      )
    ),

    hr(),

    # enable hormones?
    introBox(
      data.step = 11,
      data.intro = help_text[11],
      prettySwitch(
        inputId = "network_hormonal_choice",
        label = "Regulations?",
        value = TRUE,
        status = "success",
        slim = TRUE,
        bigger = TRUE
      ),

      # enable organs
      prettySwitch(
        inputId = "network_organ_choice",
        label = "Organs?",
        value = TRUE,
        status = "success",
        slim = TRUE,
        bigger = TRUE
      )
    ),

    # filter elements to display
    introBox(
      data.step = 12,
      data.intro = help_text[12],
      prettyCheckboxGroup(
        inputId = "network_Ca_choice",
        label = "Choose your Network",
        choices = c("Ca", "PO4", "PTH", "D3", "FGF23"),
        animation = "pulse",
        thick = TRUE,
        status = "primary",
        inline = TRUE
      )
    ),

    hr(),

    # Control Nodes size
    h4("Nodes size"),
    introBox(
      data.step = 13,
      data.intro = help_text[13],
      fluidRow(
        column(
          width = 6,
          uiOutput(outputId = "size_nodes_organs")
        ),
        column(
          width = 6,
          uiOutput(outputId = "size_nodes_hormones")
        )
      )
    ),

    # Control arrow properties
    h4("Arrow width"),
    introBox(
      data.step = 14,
      data.intro = help_text[14],
      fluidRow(
        column(
          width = 6,
          uiOutput(outputId = "width_arrows_organs")
        ),
        column(
          width = 6,
          uiOutput(outputId = "width_arrows_hormones")
        )
      )
    )
  ),
  rightSidebarTabContent(
    id = 2,
    icon = "map",
    h6("Steady-state simulations"),

    # primary hyperparathyroidism
    prettyCheckbox(
      inputId = "run_php1",
      label = "Primary hyperparathyroidism",
      value = FALSE,
      animation = "pulse",
      thick = TRUE,
      status = "primary"
    ),

    # hypoparathyroidism
    prettyCheckbox(
      inputId = "run_hypopara",
      label = "hypoparathyroidism",
      value = FALSE,
      animation = "pulse",
      thick = TRUE,
      status = "primary"
    ),

    # vitamin D3 deficiency
    prettyCheckbox(
      inputId = "run_hypoD3",
      label = "25(OH)D deficiency",
      value = FALSE,
      animation = "pulse",
      thick = TRUE,
      status = "primary"
    ),

    hr(),

    # Enable/Disable informations
    prettySwitch(
      inputId = "notif2_switch",
      label = "Notifications?",
      value = TRUE,
      status = "success",
      slim = TRUE,
      bigger = TRUE
    ),
    prettySwitch(
      inputId = "modal_switch",
      label = "Descriptions?",
      value = TRUE,
      status = "success",
      slim = TRUE,
      bigger = TRUE
    )
  ),
  rightSidebarTabContent(
    id = 3,
    icon = "paint-brush",
    # change the dashboard main theme
    skinSelectUi(id = "skin")
  )
)
