# *------------------------------------------------------------------
# | PROGRAM NAME: body.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  Contains the dashboard body
# |*------------------------------------------------------------------
# | DATA USED:  /www/rintrojs_count.js
# |             /www/hotjar.js
# |             3 youtube links for movies
# |             /logos/about_us.jpg
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  load CSS, shinyjs, introjs, MathJax, bs_popovers, bs_tooltips
# |           shinyFeedback, sweetalerts
# |  PART 2:  load js scripts (hotjar tracking, ...)
# |  PART 3:  main network_box, graph_box, load other tabPanels
# |           like movies, glossary, about_section
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

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
  
  # print feedback for input
  useShinyFeedback(),
  useSweetAlert(),
  
  # include the script for Hotjar tracking
  #tags$head(includeScript("www/hotjar.js")),
  tags$head(includeScript("www/rintrojs_count.js")),
  
  # include hotjar tracking
  tags$head(includeScript("www/hotjar.js")),
  
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
        # Ca movie
        box(id = "ca_movie", solidHeader = TRUE,
            column(12, align = "center", 
                   HTML('<iframe width="560" height="315"
                        src="https://youtube.com/embed/wdWSedADgRw" 
                        frameborder="0" allowfullscreen></iframe>')
            )
        ),
        # PO4 movie
        box(id = "PO4_movie", solidHeader = TRUE,
            column(12, align = "center",
                   HTML('<iframe width="560" height="315"
                        src="https://youtube.com/embed/z5A33wEYgbw"
                        frameborder="0" allowfullscreen></iframe>')
            )
        )
      ),
      fluidRow(
        # PTH movie
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
                      border=\"0\" align=\"center\"  src=\"logos/about_us.jpg\"/> ")
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