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
  
  # network CSS
  tags$head(
    tags$style(HTML("
                    
                    .network_cap{
                    padding-left: 0; 
                    padding-right: 0;
                    background-image:url('rat_wholebody.svg');
                    background-size: cover;
                    background-repeat: no-repeat;
                    background-position: center;
                    height:100%;
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
                      margin-left: auto;
                      margin-right: auto;
                      max-width:70%;
                      max-height:70%;
                    }
                    
                    #zoom_image{
                      border: 3px black solid;
                      border-radius: 10px;
                      box-shadow: 6px 6px 0px black;
                    }

                    #dropdown_treatment {
                      overflow-y: scroll; 
                      max-height: 500px;
                      overflow-x: hidden;
                    }

                    #tab { 
                      display:inline-block; 
                      margin-left: 100px; 
                    }
                    
                    "))
  ),
  
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
  
  tabItems(
    tabItem(
      tabName = "main",
      
      fluidRow(
        column(width = 6, offset = 0, style = 'padding:0px;',
               box(
                 id = "boxinfo", width = 12, solidHeader = TRUE,
                 
                 introBox(
                   div(id = "network_cap",
                       
                       withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                                   size = 2, 
                                   type = 8, 
                                   color = "#000000")
                   ),
                   data.step = 2,
                   data.intro = help_text[2]
                 )
               ) 
        ),
        
        column(width = 6, offset = 0, style = 'padding:0px;',
               
               box(
                 id = "tabset1",
                 width = 12,
                 collapsible = TRUE,
                 solidHeader = TRUE,
                 
                 column(6, align = "center", offset = 0, style = 'padding:0px;',
                        
                        introBox(
                          withSpinner(plotlyOutput("plot_node", height = "300px"), 
                                      size = 2, 
                                      type = 8, 
                                      color = "#000000"),
                          data.step = 3,
                          data.intro = help_text[3]
                        )
                        
                 ),
                 column(6, align = "center", offset = 0, style = 'padding:0px;',
                        introBox(
                          withSpinner(plotlyOutput("plot_edge", height = "300px"), 
                                      size = 2, 
                                      type = 8, 
                                      color = "#000000"),
                          data.step = 4,
                          data.intro = help_text[4]
                        )
                 ),
                 tableOutput("event_table")
               ),
               
               div(id = "boxinput", # values to be reset if needed
                   box(
                     # classic id does not work with tabBox to reset values inside ...
                     title = tagList(shiny::icon("question-circle"), "Questions"), width = 12,
                     "Put the vignette text here "
                     
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
