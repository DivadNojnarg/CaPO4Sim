# *------------------------------------------------------------------
# | PROGRAM NAME: ui.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the global UI of the application. 
# |           It calls header, body, both sidebars and the footer 
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

# Define UI
dashboardPage(
  skin = "black",
  title = "CaPO4 Teaching App",
  collapse_sidebar = TRUE,
  header,
  sidebar,
  body,
  footerOutput(outputId = "dynamicFooter"),
  div(id = "controlbar", dashboardControlbar())
)
