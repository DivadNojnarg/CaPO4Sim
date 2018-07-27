#-------------------------------------------------------------------------
#  This code contains the body of shinydashboard. It is an advanced dashboard
#  using several advanced javascript properties such as MathJax display,
#  organize each Boxes relatively to each others via jqui commands (shinyjqui), 
#  trigger some help modals when required. 
#
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

body <- dashboardBody(
  
  # include CSS
  includeCSS(path = "www/css/treatments-app.css"),

  # include the script for Hotjar tracking
  #tags$head(includeScript("www/hotjar.js")),
  # include the script needed to find the web browser
  
  # JS interactions
  useShinyjs(),
  extendShinyjs(script = "www/js/close.js"),
  includeScript(path = "www/js/find-navigator.js"),
  # print feedback for input
  useShinyFeedback(),
  setShadow("box"),
  setShadow("dropdown-menu"),
  setZoom("box", scale = 1.01),
  setPulse("timeline-item"),
  setPulse("diagnosis-badge"),
  setShake("diagnosis-badge"),
  setShadow("modal-content"),
  setZoom("modal-content"),

  tabItems(
    # Network panel
    tabItem(
      tabName = "main",
      
      fluidRow(
        # left colum
        column(
          width = 3,
          style = 'padding:0px;',
          
          # profile box
          uiOutput("patient_info"),
          # user notebook
          uiOutput("user_notebook"),
          # info box (previous diseases, treatments)
          uiOutput("patient_history")
        ),
        
        # patient operation table
        column(
          width = 6,
          style = 'padding:0px;',
          uiOutput("network_box")
        ),
        # event/results column
        column(
          width = 3,
          style = 'padding:0px;',
          # results box
          uiOutput("graphs_box"),
          # timeline event box
          uiOutput("recent_events")
        )
      )
    ),
    # About section Panel
    tabItem(
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