ui <- dashboardPage(
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
