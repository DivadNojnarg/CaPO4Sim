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
  
  #navbarPage("Menu", collapsible = TRUE,
  #tabPanel("Bench", # multiple dashboard navigation
  #jumbotron("Hi Shiny!", "text to show", button = FALSE),
  
  #jqui_sortabled( # all the 3 fluidrow of the main panel can be moved relatively 
  #div(
  fluidRow(
    #jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
    #div(
    column(width = 8,
           tabBox(
             id = "boxinfo",
             title = tagList(shiny::icon("map-o"), "Interactive Map of CaPO4 Homeostasis"), 
             height = "1050px", width = NULL,
             
             #For column-based layouts, use NULL for the width; the width is set by the column that contains the box
             tabPanel(title = tagList(shiny::icon("map-marker"), "CaPO4 Homeostasis"),
                      
                      column(6, align = "left",
                             
                             actionBttn(inputId = "back1", 
                                        label = "Back", 
                                        style = "stretch", 
                                        color = "primary", 
                                        size = "md", 
                                        icon = icon("step-backward"))
                      ),
                      column(6, align = "right",
                             
                             actionBttn(inputId = "next1", 
                                        label = "Next", 
                                        style = "stretch", 
                                        color = "primary", 
                                        size = "md", 
                                        icon = icon("step-forward"))
                      ),
                      
                      br(),
                      
                      div(id = "boxdiagram", # to insert a background image if needed
                          conditionalPanel(
                            condition = "(input.goQuizphp1 && output.counter_quiz_php1) || (input.goQuizhypopara && output.counter_quiz_hypopara) || (input.goQuizhypoD3 && output.counter_quiz_hypoD3) || input.Ca_inject || input.PO4_inject || input.PO4_gav", # see server code to use outputOptions...
                            withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                                        size = 2, type = 6, color = "#000000")
                            
                          )
                      ),
                      
                      #,
                      #column(6, align ="center",
                      #verbatimTextOutput("id")
                      #),
                      #column(6, align ="center",
                      #verbatimTextOutput("id_bis")
                      #)
                      
                      receiveSweetAlert(messageId = "successSw"),
                      receiveSweetAlert(messageId ="successQuizphp1"),
                      receiveSweetAlert(messageId ="successQuizhypopara"),
                      receiveSweetAlert(messageId ="successQuizhypoD3")
                      
             )
           ) 
    ),
    
    column(width = 4,
           # tabBox( # For the moment Timeline cannot work with visNetwork
           #   
           #   id = "tabset2",
           #   title = tagList(shiny::icon("thumb-tack"), "TimeLine"), height = "150px", width = NULL,
           #   
           #   conditionalPanel(
           #     condition = "input.Ca_inject",
           #     tabPanel(title = "",
           #              # timeline graph event
           #              timevisOutput("timeline")
           #     )
           #   )
           #   
           # ),
           jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
             div(
               tabBox(
                 id = "tabset1",
                 title = tagList(shiny::icon("microchip"), "Results"), 
                 height = "500px", width = NULL,
                 
                 tabPanel(title = tagList(shiny::icon("ambulance"), "Pathological Simulations"),
                          ### steady-state simulations ###
                          conditionalPanel(
                            condition = "input.run_php1 && input.goQuizphp1",
                            
                            column(12, align = "center",
                                   
                                   withSpinner(plotlyOutput("php1_plot", height = "400px"), 
                                               size = 2, type = 6, color = "#000000")
                                   
                            )
                            
                          ),
                          conditionalPanel(
                            condition = "input.run_hypoD3 && input.goQuizhypoD3",
                            
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
                            condition = "input.Ca_inject",
                            
                            column(12, align = "center",
                                   
                                   withSpinner(plotlyOutput("Ca_iv_plot", height = "400px"), 
                                               size = 2, type = 6, color = "#000000")
                                   
                            )
                            
                          ),
                          conditionalPanel(
                            condition = "input.PO4_inject",
                            
                            column(12, align = "center",
                                   
                                   withSpinner(plotlyOutput("PO4_iv_plot", height = "400px"), 
                                               size = 2, type = 6, color = "#000000")
                                   
                            )
                            
                          ),
                          conditionalPanel(
                            condition = "input.PO4_gav",
                            
                            column(12, align = "center",
                                   
                                   withSpinner(plotlyOutput("PO4_gav_plot", height = "400px"), 
                                               size = 2, type = 6, color = "#000000")
                                   
                            )
                            
                          )
                          
                 )
               ),
               div(id = "boxinput", # values to be reset if needed
                   tabBox(
                     # classic id does not work with tabBox to reset values inside ...
                     title = tagList(shiny::icon("gear"), "Control Center"), width = NULL,
                     height = "300px",
                     
                     tabPanel(title = tagList(shiny::icon("cogs"), "Pathological cases"), 
                              
                              
                              #h3(shiny::icon("cogs"), "Simulations"),
                              
                              #tags$h3("Simulations"),
                              fluidRow(
                                column(6, align = "center",
                                       
                                       h6("Steady state simulations"),
                                       
                                       # Modal for primary hyperparathyroidism
                                       
                                       modal_php1 <-
                                         bs_modal(
                                           id = "modal_php1",
                                           title = "About Primary Hyperparathyroidism",
                                           body = withMathJax("This is a simulation of primary hyperparathyroidism in the rat. The coefficient of PTH 
                                                                      production will be increased several times its base case value (until 300 times). Then 
                                                                      \\([Ca^{2+}]_p\\), \\([PO_4]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and \\([FGF]_p\\), as well as 
                                                                      calcium and phosphate fluxes are calculated at equilibrium and normalized compared to their 
                                                                      initial equilibrium value (namely when PTH production is normal). What is displayed are all 
                                                                      these concentations and fluxes as a function of the normalized PTH synthesis rate (and not as a 
                                                                      function of time). A rat model of primary hyperparathyroidism already exists in the rat and can 
                                                                      be found here: https://www.ncbi.nlm.nih.gov/pubmed/3591940"),
                                           size = "medium"
                                         ),
                                       
                                       awesomeCheckbox("run_php1", 
                                                       "PHP1", 
                                                       value = FALSE, 
                                                       status = "primary", 
                                                       width = NULL)%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_attach_modal(id_modal = "modal_php1")),
                                       
                                       # Modal for hypoparathyroidism
                                       
                                       modal_hypopara <-
                                         bs_modal(
                                           id = "modal_hypopara",
                                           title = "About hypoparathyroidism",
                                           body = withMathJax("This is a simulation of hypoparathyroidism in the rat. The coefficient of PTH production will 
                                                                       be decreased several times its base case value (until 0 times). Then \\([Ca^{2+}]_p\\), 
                                                                       \\([PO_4]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and 
                                                                       phosphate fluxes are calculated at equilibrium and normalized compared to their initial 
                                                                       equilibrium value (namely when PTH production is normal). What is displayed are all these 
                                                                       concentations and fluxes as a function of the normalized PTH synthesis rate (and not as a 
                                                                       function of time). On the x-axis, the value 1 represents the base-case, while 0.5 corresponds 
                                                                       to a division by a factor 2 of the PTH production rate and 0 represents a total inhition of 
                                                                       PTH synthesis."),
                                           img(src= "bone.png"), # include image in modal
                                           size = "medium"
                                         ),
                                       
                                       awesomeCheckbox("run_hypopara", 
                                                       "Hypoparathyroidism", 
                                                       value = FALSE, 
                                                       status = "primary", 
                                                       width = NULL)%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_attach_modal(id_modal = "modal_hypopara")),
                                       
                                       # Modal for vitamin D3 deficiency
                                       
                                       
                                       modal_hypoD3 <-
                                         bs_modal(
                                           id = "modal_hypoD3",
                                           title = "About vitamin D3 deficiency",
                                           body = withMathJax("This is a simulation of vitamin D\\(_3\\) deficiency in the rat. The concentration of 
                                                                      inactive vitamin D (25(OH)D) will be decreased several times its base case value (until 0 
                                                                      times). Then \\([Ca^{2+}]_p\\), \\([PO_4]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and 
                                                                      \\([FGF]_p\\), as well as calcium and phosphate fluxes are calculated at equilibrium and 
                                                                      normalized compared to their initial equilibrium value (namely when PTH production is normal). 
                                                                      What is displayed are all these concentations and fluxes as a function of the normalized 
                                                                      25(OH)D concentration (and not as a function of time). On the x-axis, the value 1 represents 
                                                                      the base-case, while 0.5 corresponds to a division by a factor 2 of the 25(OH)D concentration 
                                                                      and 0 represents the absence of 25(OH)D."),
                                           size = "medium"
                                         ),
                                       
                                       awesomeCheckbox("run_hypoD3", 
                                                       "D3 deficiency", 
                                                       value = FALSE, 
                                                       status = "primary", 
                                                       width = NULL)%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_attach_modal(id_modal = "modal_hypoD3"))
                                       
                                ),
                                column(6, align = "center",
                                       
                                       h6("Dynamic simulations"),
                                       
                                       # Modal for Ca iv injection
                                       
                                       modal_Ca_inject <-
                                         bs_modal(
                                           id = "modal_Ca_inject",
                                           title = "About Calcium infusion and EGTA infusion",
                                           body = withMathJax("This is a simulation of induced hypercalcemia followed by acute hypocalcemia in 250g male 
                                                                      Wistar rats (https://www.ncbi.nlm.nih.gov/pubmed/24801007). Hypercalcemia is induced by 
                                                                      continuously injecting a solution containing calcium during 60 min, and followed by EGTA 
                                                                      infusion (30 mM, rate: 3mL/min) during 60 min so that ionized plasma calcium concentration is 
                                                                      decreased by 0.3 mM. On the y-axis are represented normalized \\([Ca^{2+}]_p\\) and 
                                                                      \\([PTH]_p\\) as a function of time. Experimental observations are shown as black dots"),
                                           size = "medium"
                                         ),
                                       
                                       awesomeCheckbox("Ca_inject", 
                                                       "Ca IV injection", 
                                                       value = FALSE, 
                                                       status = "primary", 
                                                       width = NULL)%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_attach_modal(id_modal = "modal_Ca_inject")),
                                       
                                       # Modal for PO4 iv injection
                                       
                                       modal_PO4_inject <-
                                         bs_modal(
                                           id = "modal_PO4_inject",
                                           title = "About PO4 infusion",
                                           body = withMathJax("This is a simulation of intravenouse phosphate load in 250g-350g male wistar rats 
                                                                      (https://www.ncbi.nlm.nih.gov/pubmed/28246304). A solution containing 0.5 mmol \\(Na_2HPO_4\\) 
                                                                      and \\(NaH_2PO_4\\) was infused during 2-3 minutes to rats. On the y-axis are represented 
                                                                      \\([PO_4]_{tot}\\) (total plasma concentration of phosphate), normalized \\([Ca^{2+}]_p\\) and 
                                                                      \\([PTH]_p\\) as a function of time (250 minutes). Experimental observations are shown as black 
                                                                      dots."),
                                           size = "medium"
                                         ),
                                       
                                       awesomeCheckbox("PO4_inject", 
                                                       "PO4 IV injection", 
                                                       value = FALSE, 
                                                       status = "primary",
                                                       width = NULL)%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_attach_modal(id_modal = "modal_PO4_inject")),
                                       
                                       # Modal for PO4 iv injection
                                       
                                       modal_PO4_gavage <-
                                         bs_modal(
                                           id = "modal_PO4_gavage",
                                           title = "About PO4 gavage",
                                           body = withMathJax("This is a simulation of phosphate gavage in 250g-350g male wistar rats 
                                                                     (https://www.ncbi.nlm.nih.gov/pubmed/28246304). A solution containing 0.5 mmol \\(Na_2HPO_4\\) 
                                                                      and \\(NaH_2PO_4\\) was used. On the y-axis are represented \\([PO_4]_{tot}\\) (total plasma 
                                                                      concentration of phosphate), normalized \\([Ca^{2+}]_p\\) and \\([PTH]_p\\) as a function of 
                                                                      time (250 minutes). Experimental observations are shown as black dots."),
                                           size = "medium"
                                         ),
                                       
                                       awesomeCheckbox("PO4_gav", 
                                                       "PO4 gavage", 
                                                       value = FALSE,
                                                       status = "primary", 
                                                       width = NULL)%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_attach_modal(id_modal = "modal_PO4_gavage"))
                                       
                                )
                              ),
                              fluidRow(
                                column(6, align = "center",
                                       
                                       progressBar(id = "progress_bar", 
                                                   value = 0, 
                                                   status = "primary", 
                                                   display_pct = TRUE,
                                                   striped = TRUE, 
                                                   title = "Progress")
                                ),
                                column(6, align = "center",
                                       
                                       conditionalPanel(
                                         condition = "input.Ca_inject",
                                         
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
                                         condition = "input.PO4_inject",
                                         
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
                                         condition = "input.PO4_gav",
                                         
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
                                       
                                )
                              )
                     )
                   )
               )
             )
           )
           
    )
    #)
    #)
  )
  #)
  #)
  
  #),
  #tabPanel("Bench2") #  multiple dashboard navigation
  #)
)