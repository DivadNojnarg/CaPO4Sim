#-------------------------------------------------------------------------
#  This UI code contains the global UI of the application. It calls
#  header, body and sidebar (which is NULL in this case) and load all
#  javascript libraries such as shinyJS, extendShinyjs, MathJax... as well
#  as the theme by default which is cerulean (can be changed with theme selector)
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#
#  bsplus only works with R > 3.3, so pay attention to update R before installing
#  other packages. On shiny-server, always install R packages by running R in the
#  shiny folder. Put the app in src/shiny-server/myApp and access via:
#  server_ip:3838/myApp
#
#-------------------------------------------------------------------------

# Define UI
#header_box_network,
ui <- dashboardPage(
  # content
  header = navbar,
  sidebar = sidebar,
  body = body,
  controlbar = controlbar,
  footer = footer,
  preloader = list(html = spin_1(), color = "#333e48"),
  # options
  title = "Virtual Patient Simulator"
)
