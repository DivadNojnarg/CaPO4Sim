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
      menuItem("About", tabName = "about", icon = icon("info-circle")), 
      menuItem("App", tabName = "main", icon = icon("home"), selected = TRUE)
    ),
    data.step = 1,
    data.intro = help_text[1]
  )
)

