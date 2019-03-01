#-------------------------------------------------------------------------
#  This code contains the sidebar of shinydashboard. 
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 300,
  
  # user panel info
  uiOutput("user_panel"),
  
  # sidebar menu with 2 tabs
  sidebarMenu(
    id = "sidebar_main",
    menuItem(
      "About", 
      tabName = "about", 
      icon = icon("info-circle")
    ), 
    menuItem(
      "App", 
      tabName = "main", 
      icon = icon("home"), 
      selected = TRUE
    )
  )
)

