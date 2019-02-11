# *------------------------------------------------------------------
# | PROGRAM NAME: dashboardControlbar.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the right dashboard code
# |
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)
# |
# |
# *------------------------------------------------------------------
rightsidebar <- rightSidebar(
  background = "dark",
  width = 230,
  rightSidebarTabContent(
    id = 1,
    active = TRUE,
    icon = "sliders",
    networkOptionsUi(id = "network_options")
  ),
  rightSidebarTabContent(
    id = 2,
    icon = "map",
    diseaseSelectUi(id = "diseases"),
    hr(),
    infoUi(id = "infos")
  ),
  rightSidebarTabContent(
    id = 3,
    icon = "paint-brush",
    # change the dashboard main theme
    skinSelectUi(id = "skin")
  )
)
