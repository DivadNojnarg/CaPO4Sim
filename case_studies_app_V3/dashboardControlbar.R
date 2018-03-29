dashboardControlbar <- function() {
  withTags(
    div(
      id = "sidebar_bis",
      # Control Sidebar Open
      aside(class = "control-sidebar control-sidebar-dark",
            # Create the tabs
            introBox(
              data.step = 9, data.intro = help_text[9],
              ul(class = "nav nav-tabs nav-justified control-sidebar-tabs",
                 # option tabs
                 li(class = "active",
                    a(href = "#control-sidebar-parms-tab", `data-toggle` = "tab",
                      i(class = "fa fa-sliders")
                    )
                 ),
                 # education tabs
                 li(
                   a(href = "#control-sidebar-education-tab", `data-toggle` = "tab",
                     i(class = "fa fa-map")
                   )
                 ),
                 # interface tab
                 li(
                   a(href = "#control-sidebar-interface-tab", `data-toggle` = "tab",
                     i(class = "fa fa-paint-brush")
                   )
                 )
              )
            ),
            # Tab Panels
            div(class = "tab-content",
                # Parms tab content
                div(class = "tab-pane active", id = "control-sidebar-parms-tab",
                    h3(class = "control-sidebar-heading", "CaPO4 Network Options"),
                    
                    # background choice
                    introBox(
                      data.step = 10, data.intro = help_text[10],
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
                      data.step = 11, data.intro = help_text[11],
                      prettySwitch(
                        inputId = "network_hormonal_choice",
                        label = "Regulations?",
                        value = FALSE,
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
                      data.step = 12, data.intro = help_text[12],
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
                      data.step = 13, data.intro = help_text[13],
                      fluidRow(
                        column(
                          width = 6,
                          uiOutput("size_nodes_organs")
                        ),
                        column(
                          width = 6,
                          uiOutput("size_nodes_hormones")
                        )
                      )
                    ),
                    
                    # Control arrow properties
                    h4("Arrow width"),
                    introBox(
                      data.step = 14, data.intro = help_text[14],
                      fluidRow(
                        column(
                          width = 6,
                          uiOutput("width_arrows_organs")
                        ),
                        column(
                          width = 6,
                          uiOutput("width_arrows_hormones")
                        )
                      )
                    )
                ),
                # Education tab content
                div(class = "tab-pane", id = "control-sidebar-education-tab", 
                    h3(class = "control-sidebar-heading", "Case Studies"),
                    
                    h6("Steady-state simulation"),
                    
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
                    
                    h6("Steady-state simulation"),
                    
                    # Ca intraveinous injection/ EGTA injection
                    prettyCheckbox(
                      inputId = "run_Ca_inject",
                      label = "Ca/EGTA IV injection",
                      value = FALSE,
                      animation = "pulse",
                      thick = TRUE,
                      status = "primary"
                    ),
                    
                    # PO4 intraveinous injection
                    prettyCheckbox(
                      inputId = "run_PO4_inject",
                      label = "Pi IV injection",
                      value = FALSE,
                      animation = "pulse",
                      thick = TRUE,
                      status = "primary"
                    ),
                    
                    # Pi gavage
                    prettyCheckbox(
                      inputId = "run_PO4_gav",
                      label = "Pi gavage",
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
                # Interface management tab content
                div(class = "tab-pane", id = "control-sidebar-interface-tab",
                    
                    # change the dashboard main theme
                    selectInput(
                      inputId = "skin",
                      label = "Select a skin",
                      choices = c("blue", "black", "purple", 
                                  "green", "red", "yellow"),
                      selected = "black"
                    )
                )
            )
      ),
      # control-sidebar
      # Add the sidebar"s background. This div must be placed
      # immediately after the control sidebar
      div(class = "control-sidebar-bg", "")
    )
  )
}