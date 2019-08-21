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
  hairline = FALSE,
  shadow = TRUE,
  left_panel = TRUE,
  right_panel = TRUE,
  subNavbar = NULL
)
