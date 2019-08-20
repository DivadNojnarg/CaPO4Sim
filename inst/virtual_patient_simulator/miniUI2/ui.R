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
    setShadow(class = "card"),
    setZoom(class = "card", scale = 1.01),
    setPulse(class = "timeline-item"),
    setPulse(class = "diagnosis-badge"),
    setShake(class = "diagnosis-badge"),
    setShadow(class = "modal-content"),
    setZoom(class = "modal-content"),
    #setShake("post"),

    chooseSliderSkin(skin = "Flat", color = "#007cfe"),

    f7Panel(
      title = "Left Panel",
      side = "left",
      theme = "light", "Blabla",
      style = "reveal",
      uiOutput("user_panel")
    ),
    rightPanel,
    navbar = f7Navbar(
      title = "Virtual Patient Simulator",
      hairline = FALSE,
      shadow = TRUE,
      left_panel = TRUE,
      right_panel = TRUE,
      subNavbar = f7SubNavbar(
        fullScreenUI(id = "fullScreenTrigger")$children[[1]],
        uiOutput("user_game_status")
      )
    ),
    f7Tabs(
      animated = TRUE,
      #swipeable = TRUE,
      f7Tab(
        tabName = "Patient Profile",
        icon = f7Icon("home_fill"),
        active = TRUE,
        uiOutput("patient_info"),
        uiOutput("user_notebook")
      ),
      f7Tab(
        tabName = "Examination",
        icon = f7Icon("heart_fill"),
        active = FALSE,
        uiOutput("network_box"),
        uiOutput("graphs_box")
      ),
      f7Tab(
        tabName = "Events",
        icon = f7Icon("time_fill"),
        active = FALSE,
        uiOutput("recent_events")
      ),
      f7Tab(
        tabName = "About",
        icon = f7Icon("info_round_fill"),
        active = FALSE,
        div(
          id = "about_us",
          HTML(
            paste(
              "<img style=\"height: 100%; width: 100%; object-fit: contain\"
             border=\"0\" align=\"center\"  src=\"logos/about_us.jpg\"/>"
            )
          )#,
          #HTML(paste(tags$img(src = "about_us.jpg")))
        ),
        footer
      )
    )
  )
)
