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
ui <- f7Page(
  title = "Virtual Patient Simulator",
  init = f7Init(theme = "ios"),
  f7TabLayout(

    # include CSS
    includeCSS(path = "www/css/treatments-app.css"),

    # include the script for Hotjar tracking
    #tags$head(includeScript("www/hotjar.js")),
    # include the script needed to find the web browser

    # JS interactions
    useShinyjs(),
    extendShinyjs(script = "www/js/fullscreen.js"),
    extendShinyjs(script = "www/js/close.js"),
    includeScript(path = "www/js/find-navigator.js"),
    # print feedback for input
    useShinyFeedback(),
    setPulse(class = "timeline-item"),
    setPulse(class = "diagnosis-badge"),
    setShake(class = "diagnosis-badge"),

    f7Panel(
      title = "Left Panel",
      side = "left",
      theme = "light", "Blabla",
      style = "reveal",
      uiOutput("user_panel")
    ),
    rightPanel,
    navbar = navbar,
    content
  )
)
