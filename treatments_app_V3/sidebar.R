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
      menuItem("Demo", tabName = "demo", icon = icon("youtube-play"),
               conditionalPanel(
                 condition = "input.help",
                 prettyCheckbox(inputId = "checkbox3",  label = "",
                                shape = "round", status = "danger",
                                fill = TRUE, value = TRUE, width = "0px"),
                 sliderInput("obs", "Number of observations:",
                             min = 0, max = 1000, value = 500
                 )
               )
      ),
      menuItem("App", tabName = "main", icon = icon("home"), selected = TRUE)
    ),
    data.step = 1,
    data.intro = help_text[1]
  )
)

