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
  app_css(),
  
  tags$head(
    tags$script(
      "$(document).on('shiny:connected', function(event) {
          var isMobile = /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
          Shiny.onInputChange('isMobile', isMobile);
       });
      "
    )
  ),
  
  # include the script for Hotjar tracking
  #tags$head(includeScript("www/hotjar.js")),
  # include the script needed to find the web browser
  #tags$head(includeScript("www/find_navigator.js")),
  
  # to print help
  introjsUI(),
  # JS interactions
  useShinyjs(),
  #extendShinyjs(text = jsResetCode),
  # print feedback for input
  useShinyFeedback(),
  useSweetAlert(),
  setShadow("box"),
  setShadow("dropdown-menu"),
  setZoom("box", scale = 1.01),
  setPulse("timeline-item"),

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
          # info box (previous diseases, treatments)
          uiOutput("patient_history")
        ),
        
        
        # patient operation table
        column(
          width = 6,
          style = 'padding:0px;',
          
          boxPlus(
            title = tagList(
              img(
                class = "img-circle img-bordered-sm", 
                src = "monitor-2.svg", 
                width = "40px", 
                height = "40px"
              ),
              actionBttn(
                inputId = "run",
                size = "lg",
                label = "Run",
                style = "fill",
                color = "primary",
                icon = icon("play")
              ),
              actionBttn(
                inputId = "resetAll",
                size = "lg",
                label = " Reset",
                style = "fill",
                color = "danger",
                icon = icon("trash")
              ),
              dropdownButton(
                label = "Treatments",
                icon = icon("sliders"),
                status = "primary",
                circle = FALSE,
                prettyCheckboxGroup(
                  inputId = "treatment_selected",
                  label = "Select a treatment",
                  choices = c(
                    "parathyroid surgery" = "PTX",
                    "D3 iv injection" = "D3_inject",
                    "Ca supplementation" = "Ca_food",
                    "Ca iv injection" = "Ca_inject",
                    "Pi iv injection" = "P_inject",
                    "Pi supplementation" = "P_food",
                    "cinacalcet" = "cinacalcet" 
                  ),
                  thick = TRUE,
                  animation = "pulse",
                  inline = TRUE
                ),
                uiOutput(outputId = "sliderInject")
              )
            ),
            solidHeader = FALSE, 
            status = "primary", 
            width = 12,
            closable = TRUE,
            enable_sidebar = TRUE,
            sidebar_content = tagList(),
            introBox(
              div(
                id = "network_cap",
                withSpinner(
                  visNetworkOutput(
                    "network_Ca", 
                    height = "900px"
                  ), 
                  size = 2, 
                  type = 8, 
                  color = "#000000"
                )
              ),
              data.step = 2,
              data.intro = help_text[2]
            ),
            footer = NULL
          )
        ),
        # event/results column
        column(
          width = 3,
          style = 'padding:0px;',
          # results box
          boxPlus(
            width = 12, 
            solidHeader = FALSE, 
            status = "primary", 
            collapsible = TRUE,
            withSpinner(
              plotlyOutput(
                "plot_node", 
                height = "300px", 
                width = "100%"
              ),
              size = 2,
              type = 8,
              color = "#000000"
            )
          ),
          # timeline event box
          uiOutput("recent_events")
        )
      )
    ),
    # About section Panel
    tabItem(
      tabName = "about",
      div(id = "about_us",
          HTML(paste("<img style=\"height: 100%; width: 100%; object-fit: contain\" 
                      border=\"0\" align=\"center\"  src=\"about_us.jpg\"/> "))#,
          #HTML(paste(tags$img(src = "about_us.jpg")))
      )
    )
  )
)