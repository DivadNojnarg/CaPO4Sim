source("navbar.R", local = TRUE)
source("sidebar.R", local = TRUE)
source("body.R", local = TRUE)
source("footer.R", local = TRUE)
source("dashboardControlbar.R", local = TRUE)

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
