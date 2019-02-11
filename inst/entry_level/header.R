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
    '<span class = "logo-lg">CaPO4 Teaching Tool</span>',
    '<img src= "logos/online-learning.svg">'
  )),

  titleWidth = 300,
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "gears",
  fixed = FALSE,

  # user box (see generate_userInfo.R and model_utils.R)
  left_menu = tagList(userInfoUi(id = "rat")),

  # help button
  tags$li(
    title = "",
    class = "dropdown",
    introBox(
      actionBttn(
        inputId = "help",
        label = "Help",
        icon = NULL,
        style = "fill",
        color = "danger",
        size = "lg",
        block = FALSE,
        no_outline = TRUE
      ),
      data.step = 7,
      data.intro = help_text[7]
    )
  )
)
