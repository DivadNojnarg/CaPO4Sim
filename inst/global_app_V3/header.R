#-------------------------------------------------------------------------
#  This code contains the header of shinydashboard. It is modified compared
#  to classic header. Indeed, some buttons to save, load, reset, download are
#  inserted in the header bar. Moreover, users can change the global theme
#  clicking on the theme selector.
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

header <- dashboardHeader(
  title = HTML(
    paste0(
      '<span class = "logo-lg">CaPO4 Teaching Tool</span>',
      '<img src= "online-learning.svg">'
    )
  ),
  
  titleWidth = 300,
  
  dropdownMenuOutput("parameter_changed")
)