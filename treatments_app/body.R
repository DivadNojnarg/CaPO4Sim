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
  
  jqui_sortabled( # all the 3 fluidrow of the main panel can be moved relatively 
    div(
      fluidRow(
        jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
          div(
            column(width = 6,
                   tabBox(
                     id = "boxinfo",
                     title = tagList(shiny::icon("map-o"), "Interactive Map"), height = "1000px", width = NULL,
                     #For column-based layouts, use NULL for the width; the width is set by the column that contains the box
                     tabPanel(title = tagList(shiny::icon("map-marker"), "CaPO4 Homeostasis"),
                              
                              div(id = "network_cap",
                                  withSpinner(visNetworkOutput("network_Ca", height = "900px"), size = 2, type = 6, color = "#000000")
                              )#,
                              # column(6, align ="center",
                              #        verbatimTextOutput("id")
                              # ),
                              # column(6, align ="center",
                              #        verbatimTextOutput("id_bis")
                              # )#,
                              #downloadButton("downloadData", "Download Data")
                              
                     )
                   ) 
            ),
            
            column(width = 6,
                   jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
                     div(
                       tabBox(
                         id = "tabset1",
                         title = tagList(shiny::icon("microchip"), "Results"), height = "600px", width = NULL,
                         
                         tabPanel(title = tagList(shiny::icon("line-chart"), "Time Plot"),
                                  
                                  column(8, align = "center",
                                         
                                         withSpinner(plotlyOutput("hover_graph"), size = 2, type = 6, color = "#000000")
                                  ),
                                  column(4, align = "center",
                                         
                                         withSpinner(plotlyOutput("hover_graph_bis"), size = 2, type = 6, color = "#000000")
                                  ),
                                  
                                  br(),
                                  numericInput("tmax","Value of tmax:", 500, min = 0, max = NA, width = "50%")
                                  
                         )
                       ),
                       
                       div(id = "boxinput", # values to be reset if needed
                           tabBox(
                             # classic id does not work with tabBox to reset values inside ...
                             title = tagList(shiny::icon("gear"), "Control Center"), width = NULL,
                             height = "350px",
                             tabPanel(title = tagList(shiny::icon("sliders"), "Parameters"), style = "overflow-x: scroll",
                                      
                                      column(6, align = "center",
                                             multiInput(
                                               inputId = "disease_selected", label = "Select a disease :",
                                               choices = c("primary-hyperparathyroidism", "hypoparathyroidism", "vitamin D3 deficiency", "pseudohypoparathyroidism"),
                                               selected = "", width = "100%"
                                             )
                                      ),
                                      column(6, align = "center",
                                             multiInput(
                                               inputId = "treatment_selected", label = "Select a treatment :",
                                               choices = c("parathyroid surgery","vitamin D3 iv injection","Ca supplementation","Ca iv injection",
                                                           "PO4 supplementation","PO4 iv injection","cinacalcet"),
                                               selected = "", width = "100%"
                                             )   
                                      ),
                                      verbatimTextOutput("multiple")
                                      
                             )
                           )
                       )
                     )
                   )
                   
            )
          )
        )
      )
    )
  )
  
  #),
  #tabPanel("Bench2") #  multiple dashboard navigation
  #)
)