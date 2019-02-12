# *------------------------------------------------------------------
# | PROGRAM NAME: sidebar.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the left dashboard code
# |
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)
# |
# |
# *------------------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 300,
  introBox(
    sidebarMenu(
      id = "sidebar_main",
      # this piece of code ensures that prettyWidgets are
      # rendered correctly in the right sidebar.
      # NEVER REMOVE !!!
      # shinyjs hide this div element at start of the application
      div(
        id = "prettystuff",
        prettyCheckbox(
          inputId = "checkbox3",
          label = "",
          shape = "round",
          status = "danger",
          fill = TRUE,
          value = TRUE,
          width = "0px"
        )
      ),

      #menuSegment("MAIN NAVIGATION"),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Glossary", tabName = "glossary", icon = icon("search")),
      menuItem("Video", tabName = "video", icon = icon("youtube-play")),
      menuItem("App", tabName = "main", icon = icon("home"), selected = TRUE)
    ),
    data.step = 1,
    data.intro = help_text[1]
  )
)

