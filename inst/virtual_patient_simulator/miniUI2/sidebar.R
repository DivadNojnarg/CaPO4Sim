#-------------------------------------------------------------------------
#  This code contains the sidebar of shinydashboard.
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

sidebar <- bs4DashSidebar(

  title = HTML("<small>Virtual Patient</small>"),
  skin = "light",
  status = "primary",
  brandColor = NULL,
  url = "http://physiol-seafile.uzh.ch/",
  src = "logos/online-learning.png",
  elevation = 4,
  opacity = 0.8,

  # user panel info

  # sidebar menu with 2 tabs
  bs4SidebarMenu(
    bs4SidebarMenuItem(
      "App",
      tabName = "main",
      icon = "home"
    ),
    bs4SidebarMenuItem(
      "About",
      tabName = "about",
      icon = "info-circle"
    )
  )
)

