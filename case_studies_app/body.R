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

body <- dashboardBody(
  
  fluidRow(
    column(width = 6, offset = 0, style = 'padding:0px;',
           box(
             id = "boxinfo", width = 12, solidHeader = TRUE,
             
             column(4, align = "left",
                    
                    actionBttn(inputId = "back1", 
                               label = "Back", 
                               style = "stretch", 
                               color = "primary", 
                               size = "md", 
                               icon = icon("step-backward"))
             ),
             column(4, align = "center",
                    conditionalPanel(
                      condition = "input.run_Ca_inject",
                      
                      sliderInput("tmax", 
                                  "Current Time", 
                                  min = 1, 
                                  max = 120, 
                                  value = 1, 
                                  step = 1) %>%
                        shinyInput_label_embed(
                          icon("info") %>%
                            bs_embed_tooltip(title = "Click on this button to control the time of simulation. 
                                             (only for Ca-EGTA infusion, PO4 iv and PO4 gavage). 
                                             Thus you can visualize the change in fluxes at each time point on the diagramm."))
                    ),
                    conditionalPanel(
                      condition = "input.run_PO4_inject",
                      
                      sliderInput("tmaxbis", 
                                  "Current Time", 
                                  min = 1, 
                                  max = 250, 
                                  value = 1, 
                                  step = 1) %>%
                        shinyInput_label_embed(
                          icon("info") %>%
                            bs_embed_tooltip(title = "Click on this button to control the time of simulation. 
                                             (only for Ca-EGTA infusion, PO4 iv and PO4 gavage). 
                                             Thus you can visualize the change in fluxes at each time point on the diagramm."))
                    ),
                    conditionalPanel(
                      condition = "input.run_PO4_gav",
                      
                      sliderInput("tmaxtris", 
                                  "Current Time",
                                  min = 1,
                                  max = 250, 
                                  value = 1, 
                                  step = 1) %>%
                        shinyInput_label_embed(
                          icon("info") %>%
                            bs_embed_tooltip(title = "Click on this button to control the time of simulation. 
                                             (only for Ca-EGTA infusion, PO4 iv and PO4 gavage). 
                                             Thus you can visualize the change in fluxes at each time point on the diagramm."))
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
             
             div(id = "network_cap", # to insert a background image if needed
                 withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                             size = 2, type = 6, color = "#000000")
             ),
             column(6, align = "center",
                    verbatimTextOutput("counter_nav")
             )
             #column(6, align ="center",
             #verbatimTextOutput("id_bis")
             #)
             
           ) 
    ),
    
    column(width = 6, offset = 0, style = 'padding:0px;',
           box(
             id = "tabset1", width = 12, solidHeader = TRUE,
             
             ### Steady-state simulations ###
             conditionalPanel(
               condition = "input.run_php1",
               
               column(12, align = "center",
                      
                      withSpinner(plotlyOutput("php1_plot", height = "400px"), 
                                  size = 2, type = 6, color = "#000000")
                      
               )
               
             ),
             conditionalPanel(
               condition = "input.run_hypoD3",
               
               column(12, align = "center",
                      
                      withSpinner(plotlyOutput("hypoD3_plot", height = "400px"), 
                                  size = 2, type = 6, color = "#000000")
                      
               )
               
             ),
             conditionalPanel(
               condition = "input.run_hypopara",
               
               column(12, align = "center",
                      
                      withSpinner(plotlyOutput("hypopara_plot", height = "400px"), 
                                  size = 2, type = 6, color = "#000000")
                      
               )
               
             ),
             ### dynamic simulations ###
             conditionalPanel(
               condition = "input.run_Ca_inject",
               
               column(12, align = "center",
                      
                      withSpinner(plotlyOutput("Ca_iv_plot", height = "400px"), 
                                  size = 2, type = 6, color = "#000000")
                      
               )
               
             ),
             conditionalPanel(
               condition = "input.run_PO4_inject",
               
               column(12, align = "center",
                      
                      withSpinner(plotlyOutput("PO4_iv_plot", height = "400px"), 
                                  size = 2, type = 6, color = "#000000")
                      
               )
               
             ),
             conditionalPanel(
               condition = "input.run_PO4_gav",
               
               column(12, align = "center",
                      
                      withSpinner(plotlyOutput("PO4_gav_plot", height = "400px"), 
                                  size = 2, type = 6, color = "#000000")
                      
               )
               
             )
             
           )
    )
  )
)