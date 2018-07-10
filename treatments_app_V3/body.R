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
  
  # include the script for Hotjar tracking
  #tags$head(includeScript("www/hotjar.js")),
  # include the script needed to find the web browser
  #tags$head(includeScript("www/find_navigator.js")),
  
  # to print help
  introjsUI(),
  # JS interactions
  useShinyjs(),
  # print feedback for input
  useShinyFeedback(),
  useSweetAlert(),
  setShadow("box"),
  
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
          # info box (previous diseases, treatements)
          uiOutput("patient_history")
        ),
        
        
        # patient operation table
        column(
          width = 6,
          style = 'padding:0px;',
          
          widgetUserBox(
            title = "Examination Table",
            subtitle = tagList(
              #starBlock(grade = 5, color = "blue"),
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
              )
            ),
            type = NULL,
            width = 12,
            src = "https://thumbs.dreamstime.com/b/red-heart-pulse-heart-rate-athlete-gym-workout-single-icon-cartoon-style-vector-symbol-stock-web-90353642.jpg",
            color = "yellow",
            closable = TRUE,
            fluidRow(
              column(
                width = 3,
                boxPad(
                  color = NULL,
                  prettyCheckboxGroup(
                    inputId = "background_choice",
                    label = "Network background",
                    choices = c("rat", "human"),
                    thick = TRUE,
                    animation = "pulse",
                    selected = "rat",
                    inline = TRUE
                  ),
                  prettyCheckboxGroup(
                    inputId = "network_Ca_choice",
                    label = "Select a network",
                    choices = c(
                      "Ca" = "Ca",
                      "Pi" = "PO4",
                      "PTH" = "PTH",
                      "D3" = "D3",
                      "FGF23" = "FGF23"
                    ),
                    thick = TRUE,
                    animation = "pulse",
                    selected = "rat",
                    inline = TRUE
                  ),
                  prettySwitch(
                    inputId = "network_hormonal_choice",
                    label = "Hormonal regulation",
                    value = TRUE,
                    slim = TRUE,
                    bigger = TRUE
                  ),
                  prettySwitch(
                    inputId = "network_organ_choice",
                    label = "Display organs",
                    value = TRUE,
                    slim = TRUE,
                    bigger = TRUE
                  ),
                  hr(),
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
              column(
                width = 9,
                introBox(
                  div(
                    id = "network_cap",
                    withSpinner(
                      visNetworkOutput("network_Ca", height = "900px"), 
                      size = 2, 
                      type = 8, 
                      color = "#000000"
                    )
                  ),
                  data.step = 2,
                  data.intro = help_text[2]
                )
              )
            ),
            footer = NULL #tagList(
              #dashboardLabel("Label 1", status = "info"),
              #dashboardLabel("Label 2", status = "success"),
              #dashboardLabel("Label 3", status = "warning"),
              #dashboardLabel("Label 4", status = "primary"),
              #dashboardLabel("Label 5", status = "danger")
            #)
          )
        ),
        
        # event/results column
        column(
          width = 3,
          style = 'padding:0px;',
          
          # results box
          boxPlus(
            width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE,
            withSpinner(
              plotlyOutput("plot_node", height = "300px", width = "300px"),
              size = 2,
              type = 8,
              color = "#000000"
            )
          ),
          
          # timeline event box
          boxPlus(
            width = 12, solidHeader = FALSE, status = "primary",
            collapsible = TRUE,
            enable_label = TRUE,
            label_text = 3,
            label_status = "danger",
            style = "overflow-y: scroll;",
            title = "Recent Events",
            timelineBlock(
              style = "height: 600px",
              timelineEnd(color = "danger"),
              timelineLabel(2018, color = "teal"),
              tagAppendAttributes(
                timelineItem(
                 title = "Item 1",
                 icon = "gears",
                 color = "olive",
                 time = "now",
                 timelineItemMedia(
                  src = "pills.svg",
                  width = "40", 
                  height = "40"
                 ),
                 footer = "Vitamin D3 supplementation"
                ),
                align = "middle"
              ),
              tagAppendAttributes(
                timelineItem(
                  title = "Item 2",
                  timelineItemMedia(
                    src = "syringe.svg", 
                    width = "40", 
                    height = "40"
                  ),
                  border = FALSE
                ),
                align = "middle"
              ),
              timelineLabel(2015, color = "orange"),
              tagAppendAttributes(
                timelineItem(
                  title = "Item 3",
                  icon = "paint-brush",
                  color = "maroon",
                  timelineItemMedia(
                    src = "medicine.svg", 
                    width = "40", 
                    height = "40"
                  )
                ),
                align = "middle"
              ),
              timelineStart(color = "gray")
            )
          )
          
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