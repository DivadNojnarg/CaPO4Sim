# *------------------------------------------------------------------
# | PROGRAM NAME: header.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This code contains the header of shinydashboard.
# |           It is modified compared to classic header: help button,
# |           userInfo card.
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)
# |
# |
# *------------------------------------------------------------------

header <- dashboardHeaderPlus(
  title = HTML(paste0(
    '<span class = "logo-lg">Cardio-Renal Teaching Tool</span>',
    '<img src= "logos/online-learning.svg">'
  )),

  titleWidth = 300,
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "gears",
  fixed = FALSE,

  # user box (see generate_userInfo.R and model_utils.R)
  userInfoUi(id = "rat"),
  # help button
  helpCaPO4Ui(id = "help_section"),
  # full screen option
  left_menu = tagList(fullScreenUI(id = "fullScreenTrigger"))
)
