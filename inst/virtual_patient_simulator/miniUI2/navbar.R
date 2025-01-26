fullScreenUI <- function() {
  tagAppendAttributes(
    f7Button(
      inputId = "fullscreen",
      label = icon("expand"),
      color = "blue",
      size = "large"
    ),
    onclick = "shinyjs.toggleFullScreen();"
  )
}

navbar <- f7Navbar(
  title = "Virtual Patient Simulator",
  leftPanel = TRUE,
  rightPanel = TRUE,
  subNavbar = NULL
)
