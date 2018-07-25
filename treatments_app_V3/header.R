#-------------------------------------------------------------------------
#  This code contains the header of shinydashboard. It is modified compared
#  to classic header. Indeed, some buttons to save, load, reset, download are
#  inserted in the header bar. Moreover, users can change the global theme
#  clicking on the theme selector.
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

header <- dashboardHeaderPlus(
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "gears",
  title = tagList(
    span(class = "logo-lg", "CaPO4 Teaching Tool"), 
    img(src = "logos/online-learning.svg")
  ),
  
  titleWidth = 300,
  
  left_menu = tagList(
    uiOutput("currentTime"),
    uiOutput("user_game_status")#,
    #uiOutput("current_calcium")
  )
)