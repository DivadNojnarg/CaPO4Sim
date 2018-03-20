#-------------------------------------------------------------------------
#  This code contains the sidebar of shinydashboard. 
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

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
      menuItem("Settings", tabName = "settings", icon = icon("sliders"),
               
               # show background or not
               prettyCheckboxGroup(inputId = "background_choice",
                                   label = "Background",
                                   animation = "pulse", thick = TRUE,
                                   choices = c("rat","human"), inline = TRUE,
                                   status = "primary"),
               
               # select the network
               prettyCheckboxGroup(inputId = "network_Ca_choice",
                                   label = "Choose your network", 
                                   selected = "Ca",
                                   animation = "pulse", thick = TRUE,
                                   choices = c("Ca","PO4"), inline = TRUE,
                                   status = "primary"),
               
               # selector for hormonal regulation
               prettySwitch(inputId = "network_hormonal_choice", 
                            label = "Show regulations", 
                            status = "success",
                            bigger = TRUE,
                            fill = TRUE,
                            value = FALSE),
               
               # maximum time of integration
               numericInput("tmax",
                            "Maximum simulated time:", 
                            value = 500, 
                            min = 0, 
                            max = NA,
                            width = "100%"),
               
               # navigate to a given time
               sliderInput("t_now",
                           "Time after simulation:", 
                           value = 1, 
                           min = 1, 
                           max = 500,
                           width = "90%") %>%
                 shinyInput_label_embed(
                   icon("undo") %>%
                     actionBttn(inputId = "reset_t_now",
                                label = "", 
                                color = "danger", 
                                size = "xs")
                   )
      )
    ),
    data.step = 1,
    data.intro = help_text[1]
  )
)

