#-------------------------------------------------------------------------
#  This code contains the body of shinydashboard. It is an advanced dashboard
#  using several advanced javascript properties such as MathJax display,
#  organize each Boxes relatively to each others via jqui commands (shinyjqui), 
#  trigger some help modals when required. 
#
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------
source("help.R")
body <- dashboardBody(
  
  fluidRow(
    column(width = 6, offset = 0, style = 'padding:0px;',
           box(
             id = "boxinfo", width = 12, solidHeader = TRUE,
             
             column(4, align = "left",
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
                    
                    actionBttn(inputId = "next1", 
                               label = "Next", 
                               style = "stretch", 
                               color = "primary", 
                               size = "md", 
                               icon = icon("step-forward"))
             ),
             
             br(),
             
             introBox(
               div(id = "network_cap", # to insert a background image if needed
                   withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                               size = 2, type = 8, color = "#000000")
               ),
               data.step = 2,
               data.intro = help_text[2]
             )#,
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
             ### Steady-state simulations ###
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
                      uiOutput("slider", class = "theme-orange")
               ),
               column(4, align = "right")
             )
           )
    )
  )
)