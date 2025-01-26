source("navbar.R")
source("sidebar.R")
source("body.R")
source("footer.R")
source("dashboardControlbar.R")

ui <- bs4Dash::dashboardPage(
  # content
  header = navbar,
  sidebar = sidebar,
  body = body,
  controlbar = controlbar,
  footer = footer,
  preloader = list(html = spin_1(), color = "#333e48"),
  # options
  title = "Virtual Patient Simulator"
)
