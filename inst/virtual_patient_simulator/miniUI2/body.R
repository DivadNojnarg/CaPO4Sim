content <- f7Tabs(
  animated = TRUE,
  #swipeable = TRUE,
  f7Tab(
    tabName = "Patient Profile",
    icon = f7Icon("archivebox"),
    active = TRUE,
    uiOutput("patient_info"),
    uiOutput("user_notebook")
  ),
  f7Tab(
    tabName = "Examination",
    icon = f7Icon("heart"),
    active = FALSE,
    uiOutput("network_box"),
    uiOutput("graphs_box")
  ),
  f7Tab(
    tabName = "Events",
    icon = f7Icon("timer"),
    active = FALSE,
    uiOutput("recent_events")
  ),
  f7Tab(
    tabName = "About",
    icon = f7Icon("info_circle"),
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
