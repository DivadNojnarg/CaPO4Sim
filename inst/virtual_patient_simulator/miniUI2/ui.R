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
  f7Init("ios"),
  f7TabLayout(
    f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", style = "cover"),
    f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", style = "cover"),
    f7Navbar(
      title = "Virtual Patient Simulator",
      hairline = FALSE,
      shadow = TRUE,
      left_panel = TRUE,
      right_panel = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      #swipeable = TRUE,
      f7Tab(
        tabName = "Tab 1",
        icon = "email",
        active = TRUE,
        "tab 1"
      ),
      f7Tab(
        tabName = "Tab 2",
        icon = "today",
        active = FALSE,
        "Tab 2"
      ),
      f7Tab(
        tabName = "Tab 3",
        icon = "file_upload",
        active = FALSE,
        "Tab 3"
      )
    )
  )
)
