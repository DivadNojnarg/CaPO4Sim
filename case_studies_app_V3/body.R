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
  
  # load the css
  app_css(),
  # perform some javascript events such as show/hide ...
  useShinyjs(), 
  # load the help animation library
  introjsUI(),
  # make beautiful notifications, replace showNotifications by shiny
  useToastr(),
  withMathJax(),
  use_bs_popover(),
  use_bs_tooltip(),
  
  # include the script for Hotjar tracking
  #tags$head(includeScript("www/hotjar.js")),
  tags$head(includeScript("www/rintrojs_count.js")),
  
  # include hotjar tracking
  tags$head(includeScript("www/hotjar.js")),
  
  # print feedback for input
  useShinyFeedback(),
  useSweetAlert(),
  
  # Main application Panel
  tabItems(
    tabItem(
      tabName = "main",
      fluidRow(
        # load the CaPO4 network box
        network_box(),
        # load the graph box
        graph_box()
      )
    ),
    
    # Video panels
    tabItem(
      tabName = "demo",
      
      fluidRow(
        box(id = "ca_movie", solidHeader = TRUE,
            column(12, align = "center", 
                   HTML('<iframe width="560" height="315"
                        src="https://youtube.com/embed/wdWSedADgRw" 
                        frameborder="0" allowfullscreen></iframe>')
            )
        ),
        box(id = "PO4_movie", solidHeader = TRUE,
            column(12, align = "center",
                   HTML('<iframe width="560" height="315"
                        src="https://youtube.com/embed/z5A33wEYgbw"
                        frameborder="0" allowfullscreen></iframe>')
            )
        )
      ),
      fluidRow(
        box(id = "PTH_movie", solidHeader = TRUE,
            column(12, align = "center",
                   HTML('<iframe width="560" height="315"
                        src="https://youtube.com/embed/TiibPBsxV0E"
                        frameborder="0" allowfullscreen></iframe>')
            )
        )
      )
    ),
    
    # About section Panel
    tabItem(
      tabName = "about",
      div(id = "about_us",
          HTML(
            paste("<img style=\"height: 100%; width: 100%; object-fit: contain\" 
                      border=\"0\" align=\"center\"  src=\"/logos/about_us.jpg\"/> ")
          )
      )
    ),
    # Glossary Panel
    tabItem(
      tabName = "glossary",
      div(id = "glossary",
          box(id = "boxglossary", solidHeader = TRUE, width = 12, height = "50%",
              dataTableOutput("glossary")
          )
      )
    )
  )
)