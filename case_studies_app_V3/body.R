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
source("help.R")

body <- dashboardBody(
  
  tags$head(
    tags$style(HTML("
                    #page{
                      position: relative;
                    }
                    
                    .js-irs-0 .irs-single, 
                    .js-irs-0 .irs-bar-edge,
                    .js-irs-0 .irs-bar{
                    background: orange;
                    }
                    
                    .js-irs-1 .irs-single, 
                    .js-irs-1 .irs-bar-edge, 
                    .js-irs-1 .irs-bar{
                    background: orange;
                    }
                    
                    .js-irs-2 .irs-single, 
                    .js-irs-2 .irs-bar-edge, 
                    .js-irs-2 .irs-bar{
                    background: orange;
                    }
                    
                    .theme-orange .irs-single, 
                    .theme-orange .irs-bar-edge, 
                    .theme-orange .irs-bar{
                    background: orange;
                    }
                    
                    .network_capnone{
                      padding-left: 0; 
                      padding-right: 0;
                      height:100%;
                    }
                    
                    .network_caprat{
                      padding-left: 0; 
                      padding-right: 0;
                      background-image:url('rat_wholebody.svg');
                      background-size: cover;
                      background-repeat: no-repeat;
                      background-position: center;
                      height:100%;
                    }
                    
                    .network_caphuman{
                      padding-left: 0; 
                      padding-right: 0;
                      background-image:url('human_wholebody.svg');
                      background-size: cover;
                      background-repeat: no-repeat;
                      background-position: center;
                      height:100%;
                    }

                    #boxvideo{
                      text-align: center;
                    }

                    mark {
                      background-color: yellow;
                    }

                    .newClass{
                      min-width: 900px;
                      max-width: 900px;
                    }

                    #about_us{
                      max-width:100%;
                      max-height:100%;
                    }

                    #zoom_image{
                      border: 3px black solid;
                      border-radius: 10px;
                      box-shadow: 6px 6px 0px black;
                    }
                    "))
  ),
  
  # notification that can be switched on or off as required
  #tags$style("#shiny-notification-notifid {position: fixed; top: 25%; right: 76% ; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-notifid {position: fixed; top: 20%; 
             right: 76% ; width: 20em; opacity: 1; z-index:100;}"),
  tags$style("#shiny-notification-graph_notif {position: fixed; top: 30%; 
             left: 70% ; width: 20em; opacity: 1; z-index:100;}"),
  
  # perform some javascript events such as show/hide ...
  useShinyjs(), 
  # load the help animation library
  introjsUI(),
  # make beautiful notifications, replace showNotifications by shiny
  useToastr(),
  
  use_bs_popover(),
  use_bs_tooltip(),
  
  # include the script for Hotjar tracking
  #tags$head(includeScript("www/hotjar.js")),
  
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
                 )#,
                 # column(4, align = "left",
                 #        verbatimTextOutput("position")
                 # ),
                 # column(4, align = "center",
                 #        verbatimTextOutput("viewposition")
                 # )
                 #,
                 # column(4, align = "left",
                 #        verbatimTextOutput("counter_nav")
                 # ),
                 # column(4, align = "center",
                 #        verbatimTextOutput("id_bis")
                 # ),
                 # column(4, align = "right",
                 #        verbatimTextOutput("id")
                 # )
                 
               ) 
        ),
        
        column(width = 6, offset = 0, style = 'padding:0px;',
               box(
                 id = "tabset1", width = 12, solidHeader = TRUE,
                 
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
                   br(),
                   column(4, align = "left"),
                   column(4, align = "center",
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
      
      div(id = "boxvideo",
          box(id = "boxvideo", solidHeader = TRUE,
              HTML('<iframe width="560" height="315"
                   src="https://www.youtube.com/embed/AKFyJfYdJhA"
                   frameborder="0" allowfullscreen></iframe>')
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
    ),
    
    # Written tutorial for this app
    tabItem(
      tabName = "help",
      h1("How to use this App?")
    )
    
  )
)




