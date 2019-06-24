body <- bs4DashBody(

  # include CSS
  includeCSS(path = "www/css/treatments-app.css"),

  # include the script for Hotjar tracking
  #tags$head(includeScript("www/hotjar.js")),
  # include the script needed to find the web browser

  # JS interactions
  useShinyjs(),
  extendShinyjs(script = "www/js/fullscreen.js"),
  extendShinyjs(script = "www/js/close.js"),
  includeScript(path = "www/js/find-navigator.js"),
  # print feedback for input
  useShinyFeedback(),
  setShadow(class = "card"),
  setZoom(class = "card", scale = 1.01),
  setPulse(class = "timeline-item"),
  setPulse(class = "diagnosis-badge"),
  setShake(class = "diagnosis-badge"),
  setShadow(class = "modal-content"),
  setZoom(class = "modal-content"),
  #setShake("post"),

  chooseSliderSkin(skin = "Flat", color = "#007cfe"),

  bs4TabItems(
    # Network panel
    bs4TabItem(
      tabName = "main",
      uiOutput("patient_ui")
    ),
    # About section Panel
    bs4TabItem(
      tabName = "about",
      div(
        id = "about_us",
        HTML(
          paste(
            "<img style=\"height: 100%; width: 100%; object-fit: contain\"
             border=\"0\" align=\"center\"  src=\"logos/about_us.jpg\"/>"
          )
        )#,
        #HTML(paste(tags$img(src = "about_us.jpg")))
      )
    )
  )
)
