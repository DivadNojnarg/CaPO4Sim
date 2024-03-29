#-------------------------------------------------------------------------
#  This code contains the header of shinydashboard. It is modified compared
#  to classic header. Indeed, some buttons to save, load, reset, download are
#  inserted in the header bar. Moreover, users can change the global theme
#  clicking on the theme selector.
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

navbar <- bs4DashNavbar(
  skin = "light",
  status = "white",
  border = TRUE,
  uiOutput("user_game_status"),
  downloadButton(
    label = "logs",
    outputId = "download_logs"
  ),
  fixed = FALSE
)
