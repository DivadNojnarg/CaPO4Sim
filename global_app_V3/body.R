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
                    #networkPTH{
                    background-image:url('parathyroid_gland_zoom.png');
                    background-size: auto 100%;
                    background-repeat: no-repeat;
                    background-position: center center;
                    }
                    
                    #networkkidney_zoom2{
                    background-image:url('kidney_zoom2.png');
                    background-size: auto 100%;
                    background-repeat: no-repeat;
                    background-position: center center;
                    }
                    
                    #networkkidney_PT{
                    background-image:url('kidney_PT_zoom.png');
                    background-size: auto 100%;
                    background-repeat: no-repeat;
                    background-position: center center;
                    }
                    
                    #networkkidney_PT_PO4{
                    background-image:url('kidney_PT_PO4_zoom.png');
                    background-size: auto 100%;
                    background-repeat: no-repeat;
                    background-position: center center;
                    }
                    
                    #networkkidney_TAL{
                    background-image:url('kidney_TAL_zoom.png');
                    background-size: auto 100%;
                    background-repeat: no-repeat;
                    background-position: center center;
                    }
                    
                    #networkkidney_DCT{
                    background-image:url('kidney_DCT_zoom.png');
                    background-size: auto 100%;
                    background-repeat: no-repeat;
                    background-position: center center;
                    }
                    
                    #networkintestine{
                    background-image:url('intestine_zoom.png');
                    background-size: auto 100%;
                    background-repeat: no-repeat;
                    background-position: center center;
                    }
                    
                    #networkbone{
                    background-image:url('bone_zoom.png');
                    background-size: auto 100%;
                    background-repeat: no-repeat;
                    background-position: center center;
                    }
                    
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

                    #tab { 
                      display:inline-block; 
                      margin-left: 400px; 
                    }

                    "))
  ),
  
  # include the script for Hotjar tracking
  tags$head(includeScript("www/hotjar.js")),
  # include the script needed to find the web browser
  tags$head(includeScript("www/find_navigator.js")),
  
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
                 )
               ),
               
               div(id = "boxinput", # values to be reset if needed
                   box(
                     width = 12,
                     #title = header_box_network,
                     solidHeader = TRUE,
                     
                     #column(8, align = "left", offset = 0, style = 'padding:0px;',
                     
                     conditionalPanel(
                       condition = "input.current_node_bis_id == null ||
                                    input.current_node_bis_id == 'null'",
                       HTML(paste("Please", "<mark><font color=\"#FF0000\"><b>", 
                                  "double-click", "</b></font></mark>", 
                                  "on a node to display its, <b>detailed content</b> 
                                  (only available for parathyroid glands,
                                  bones, kidneys and intestine)."), sep = " ")
                     ),
                     
                     introBox(
                       # the PTH zoom graph
                       # this panel is also shown when help button is clicked
                       conditionalPanel(
                         condition = "input.current_node_bis_id == 11 ||
                                         input.help",
                         
                         div(id = "networkPTH",
                             visNetworkOutput("network_PTH", height = "400px")
                         )
                         
                       ),
                       data.step = 5,
                       data.intro = help_text[5]
                     ),
                     
                     # Kidney zoom 1
                     conditionalPanel(
                       condition = "input.current_node_bis_id == 6",
                       
                       div(id = "networkkidney_zoom2",
                           visNetworkOutput("network_kidney_zoom2", height = "400px")
                       )
                       
                     ),
                     
                     # Proximal tubule zoom Ca
                     conditionalPanel(
                       condition = "input.current_node_tris_id == 1 &&
                                    input.current_node_bis_id != 'null' &&
                                    input.current_node_tris_id != 'null' &&
                                    input.network_Ca_choice.indexOf('Ca') > -1 ",
                       
                       div(id = "networkkidney_PT",
                           visNetworkOutput("network_kidney_PT", height = "400px")
                       )
                       
                     ),
                     
                     # Proximal tubule zoom PO4
                     conditionalPanel(
                       condition = "input.current_node_tris_id == 1 &&
                                    input.current_node_bis_id != 'null' &&
                                    input.current_node_tris_id != 'null' &&
                                    input.network_Ca_choice.indexOf('PO4') > -1 ",
                       
                       div(id = "networkkidney_PT_PO4",
                           visNetworkOutput("network_kidney_PT_PO4", height = "400px")
                       )
                       
                     ),
                     
                     # TAL zoom
                     conditionalPanel(
                       condition = "input.current_node_tris_id == 2 &&
                                    input.current_node_bis_id != 'null' &&
                                    input.current_node_tris_id != 'null'",
                       
                       div(id = "networkkidney_TAL",
                           visNetworkOutput("network_kidney_TAL", height = "400px")
                       )
                       
                     ),
                     
                     # DCT zoom
                     conditionalPanel(
                       condition = "input.current_node_tris_id == 3 &&
                                    input.current_node_bis_id != 'null' &&
                                    input.current_node_tris_id != 'null'",
                       
                       div(id = "networkkidney_DCT",
                           visNetworkOutput("network_kidney_DCT", height = "400px")
                       )
                       
                     ),
                     
                     # Intestine zoom
                     conditionalPanel(
                       condition = "input.current_node_bis_id == 1 &&
                                    input.current_node_bis_id != 'null'",
                       
                       div(id = "networkintestine",
                           visNetworkOutput("network_intestine", height = "400px")
                       )
                       
                     ),
                     
                     # bone zoom
                     conditionalPanel(
                       condition = "input.current_node_bis_id == 4 &&
                                    input.current_node_bis_id != 'null'",
                       
                       div(id = "networkbone",
                           visNetworkOutput("network_bone", height = "400px")
                       )
                     )
                     #)
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
