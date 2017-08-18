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
                                  withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                                              size = 2, type = 6, color = "#000000")
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
                                         
                                         withSpinner(plotlyOutput("plot_node"), size = 2, type = 6, color = "#000000")
                                  ),
                                  column(4, align = "center",
                                         
                                         withSpinner(plotlyOutput("plot_edge"), size = 2, type = 6, color = "#000000")
                                  ),
                                  
                                  tableOutput("event_table")
                         )
                         
                       ),
                       
                       div(id = "boxinput", # values to be reset if needed
                           tabBox(
                             # classic id does not work with tabBox to reset values inside ...
                             title = tagList(shiny::icon("question-circle"), "Questions"), width = NULL,
                             height = "350px",
                             tabPanel(title = tagList(shiny::icon("question-circle-o"), "Q1") #, style = "overflow-x: scroll"
                                      
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