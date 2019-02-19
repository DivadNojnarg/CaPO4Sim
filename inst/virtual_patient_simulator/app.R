shinyApp(
  ui = dashboardPage(
    skin = "black",
    title = "CaPO4 Teaching App",
    collapse_sidebar = TRUE,
    header,
    sidebar,
    body,
    footerOutput(outputId = "dynamicFooter"),
    div(id = "controlbar", dashboardControlbar())
  ),
  server = server
)
