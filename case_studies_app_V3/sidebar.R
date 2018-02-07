#-------------------------------------------------------------------------
#  This code contains the sidebar of shinydashboard. 
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------
source("help.R")

sidebar <- dashboardSidebar(
  width = 300,
  
  introBox(
    
    sidebarMenu(
      id = "sidebar_main",
      
      #menuSegment("MAIN NAVIGATION"),
      
      menuItem("About", tabName = "about", icon = icon("info-circle")), 
      menuItem("Help", tabName = "help", icon = icon("question-circle"),
               actionBttn("help", label = "Help", icon = NULL, style = "unite",
                          color = "danger", size = "lg", block = FALSE, no_outline = TRUE)),
      menuItem("Demo", tabName = "demo", icon = icon("youtube-play")),
      menuItem("App", tabName = "main", icon = icon("home"), selected = TRUE),
      
      menuItem("Case Studies", tabName = "case_studies", icon = icon("map-o"),
               
               
               # choose basic animations
               # Uncomment when animations are ready
               # awesomeRadio(inputId = "basic_animation", 
               #              label = "Choose an animation :", 
               #              choices = c("Calcium", "Phosphate"),
               #              inline = TRUE, status = "success"),
               
               # basic case studies
               menuItem("Dysregulations", tabName = "steady_state", 
                        icon = shiny::icon("angle-double-right"),
                        
                        # awesomeCheckboxGroup(inputId = "steady_state",
                        #                      label = "Choose a disease:",
                        #                      choices = c("Primary-hyperparathyroidism",
                        #                                  "Hypoparathyroidism",
                        #                                  "25(OH)D deficiency"),
                        #                      status = "primary"),
                        
                        prettyCheckbox("run_php1",
                                        "Primary hyperparathyroidism",
                                        value = FALSE,
                                        thick = TRUE,
                                        animation = "pulse",
                                        status = "primary",
                                        width = NULL),
                        
                        prettyCheckbox("run_hypopara",
                                        "Hypoparathyroidism",
                                        value = FALSE,
                                        thick = TRUE,
                                        animation = "pulse",
                                        status = "primary",
                                        width = NULL), 
                        
                        
                        prettyCheckbox("run_hypoD3",
                                        "25(OH)D deficiency",
                                        value = FALSE,
                                        thick = TRUE,
                                        animation = "pulse",
                                        status = "primary",
                                        width = NULL)), 
               
               
               menuItem("Dynamic", tabName = "dynamic", 
                        icon = shiny::icon("angle-double-right"),
                        
                        # awesomeCheckboxGroup(inputId = "dynamic",
                        #                      label = "Choose:",
                        #                      choices = c("Ca/EGTA IV injection",
                        #                                  "PO4 IV injection",
                        #                                  "PO4 gavage"),
                        #                      status = "primary"),
                        
                        prettyCheckbox("run_Ca_inject",
                                        "Ca/EGTA IV injection",
                                        value = FALSE,
                                        thick = TRUE,
                                        animation = "pulse",
                                        status = "primary",
                                        width = NULL),
                        
                        
                        prettyCheckbox("run_PO4_inject",
                                        "PO4 IV injection",
                                        value = FALSE,
                                        thick = TRUE,
                                        animation = "pulse",
                                        status = "primary",
                                        width = NULL),
                        
                        prettyCheckbox("run_PO4_gav",
                                        "PO4 gavage",
                                        value = FALSE,
                                        thick = TRUE,
                                        animation = "pulse",
                                        status = "primary",
                                        width = NULL))
               
      ),
      # parameters
      menuItem("Settings", tabName = "settings", icon = icon("sliders"),
               
               # show background or not
               prettyCheckboxGroup(inputId = "background_choice",
                                    label = "Background",
                                    thick = TRUE,
                                    animation = "pulse",
                                    choices = c("rat","human"), inline = TRUE,
                                    status = "primary"),
               
               # select the network
               prettyCheckboxGroup(inputId = "network_Ca_choice",
                                    label = "Choose your network", 
                                    thick = TRUE,
                                    animation = "pulse",
                                    selected = "Ca",
                                    choices = c("Ca","PO4"), inline = TRUE,
                                    status = "primary"),
               
               # notification for diagram and graph part
               prettySwitch(inputId = "notif2_switch",
                              label = "Notifications?",
                              status = "success",
                              bigger = TRUE,
                              fill = TRUE,
                              value = TRUE),
               
               # selector for hormonal regulation
               prettySwitch(inputId = "network_hormonal_choice", 
                              label = "Regulations?", 
                              status = "success",
                              bigger = TRUE,
                              fill = TRUE,
                              value = FALSE)
      )
    ),
    data.step = 1,
    data.intro = help_text[1]
  )
)

