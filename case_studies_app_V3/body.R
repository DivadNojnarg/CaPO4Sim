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
  
  # print feedback for input
  useShinyFeedback(),
  useSweetAlert(),
  
  tabItems(
    tabItem(
      tabName = "main",
      
      fluidRow(
        column(width = 6, offset = 0, style = 'padding:0px;',
               box(
                 id = "boxinfo", width = 12, solidHeader = TRUE,
                 
                 column(4, align = "left",
                        conditionalPanel(
                          condition = "input.run_php1 | input.run_hypopara | 
                                       input.run_hypoD3 | input.help",
                          introBox(
                            actionBttn(inputId = "back1", 
                                       label = "Back", 
                                       style = "stretch", 
                                       color = "primary", 
                                       size = "md", 
                                       icon = icon("step-backward")),
                            data.step = 3,
                            data.intro = help_text[3]
                          )
                        )
                 ),
                 column(4, align = "center",
                        conditionalPanel(
                          # this panel is also available in help
                          condition = "input.run_Ca_inject | input.help",
                          introBox(
                            sliderInput("tmaxCainj", 
                                        "Current Time", 
                                        min = 1, 
                                        max = 120, 
                                        value = 1, 
                                        step = 1) %>%
                              shinyInput_label_embed(
                                icon("undo") %>%
                                  actionBttn(inputId = "reset_tmaxCainj",
                                             label = "", 
                                             color = "danger", 
                                             size = "xs")),
                            data.step = 4,
                            data.intro = help_text[4]
                          )
                        ),
                        conditionalPanel(
                          condition = "input.run_PO4_inject",
                          
                          sliderInput("tmaxPO4inj", 
                                      "Current Time", 
                                      min = 1, 
                                      max = 250, 
                                      value = 1, 
                                      step = 1) %>%
                            shinyInput_label_embed(
                              icon("undo") %>%
                                actionBttn(inputId = "reset_tmaxPO4inj",
                                           label = "", 
                                           color = "danger", 
                                           size = "xs"))
                        ),
                        conditionalPanel(
                          condition = "input.run_PO4_gav",
                          
                          sliderInput("tmaxPO4gav", 
                                      "Current Time",
                                      min = 1,
                                      max = 250, 
                                      value = 1, 
                                      step = 1) %>%
                            shinyInput_label_embed(
                              icon("undo") %>%
                                actionBttn(inputId = "reset_tmaxPO4gav",
                                           label = "", 
                                           color = "danger", 
                                           size = "xs"))
                        )
                 ),
                 column(4, align = "right",
                        conditionalPanel(
                          condition = "input.run_php1 | input.run_hypopara | 
                                       input.run_hypoD3 | input.help",
                          actionBttn(inputId = "next1", 
                                     label = "Next", 
                                     style = "stretch", 
                                     color = "primary", 
                                     size = "md", 
                                     icon = icon("step-forward"))
                        )
                 ),
                 
                 br(),
                 
                 introBox(
                   div(id = "network_cap", # to insert a background image if needed
                       withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                                   size = 2, type = 8, color = "#000000")
                   ),
                   data.step = 2,
                   data.intro = help_text[2],
                   data.position = "right"
                 )
               ) 
        ),
        
        column(width = 6, offset = 0, style = 'padding:0px;',
               box(
                 id = "tabset1", width = 12, solidHeader = TRUE, height = "950px",
                 #verbatimTextOutput("test"),
                 uiOutput("info"),
                 
                 conditionalPanel(
                   condition = "input.run_php1 | input.run_hypopara | 
                   input.run_hypoD3 | input.run_Ca_inject | 
                   input.run_PO4_inject | input.run_PO4_gav | input.help",
                   
                   column(12, align = "center",
                          introBox(
                            withSpinner(plotlyOutput("plot", height = "600px"), 
                                        size = 2, type = 8, color = "#000000"),
                            data.step = 5,
                            data.intro = help_text[5]
                          )
                   ),
                   column(4, align = "left"),
                   column(4, align = "center",
                          br(), br(), br(), br(), br(),
                          introBox(
                            uiOutput("slider", class = "theme-orange"),
                            data.step = 6,
                            data.intro = help_text[6],
                            data.position = "left"
                          )
                   ),
                   column(4, align = "right")
                 )
               )
        )
      )
    ),
    
    # Demonstration Panel
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
      box(id = "PTH_movie", solidHeader = TRUE,
          column(12, align = "center",
                 HTML('<iframe width="560" height="315"
                        src="https://youtube.com/embed/TiibPBsxV0E"
                        frameborder="0" allowfullscreen></iframe>')
          )
      ),
      fluidRow()
      # div(id = "boxvideo",
      #     box(id = "boxvideo", solidHeader = TRUE,
      #         HTML('<iframe width="560" height="315"
      #              src="https://www.youtube.com/embed/AKFyJfYdJhA"
      #              frameborder="0" allowfullscreen></iframe>')
      #     )
      # )
    ),
    
    # About section Panel
    tabItem(
      tabName = "about",
      div(id = "about_us",
          HTML(paste("<img style=\"height: 100%; width: 100%; object-fit: contain\" 
                      border=\"0\" align=\"center\"  src=\"about_us.jpg\"/> "))#,
          #HTML(paste(tags$img(src = "about_us.jpg")))
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