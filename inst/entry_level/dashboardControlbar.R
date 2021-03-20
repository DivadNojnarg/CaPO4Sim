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
controlbar <- dashboardControlbar(
  id = "controlbar",
  skin = "dark",
  width = 230,
  controlbarMenu(
    id = "controlbarmenu",
    controlbarItem(
      title = "Options",
      icon = icon("sliders"),
      networkOptionsUi(id = "network_options")
    ),
    controlbarItem(
      title = "Diseases",
      icon = icon("map"),
      diseaseSelectUi(id = "diseases"),
      hr(),
      infosUi(id = "infos")
    ),
    controlbarItem(
      title = "Skin",
      icon = icon("paint-brush"),
      # change the dashboard main theme
      skinSelectUi(id = "skin")
    )
  )
)
