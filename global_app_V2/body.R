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
    column(width = 6, offset = 0, style='padding:0px;',
           box(
             id = "boxinfo",
             #title = tagList(shiny::icon("map-o"), "Interactive Map"), 
             height = "1000px", width = 12,
             #For column-based layouts, use NULL for the width; the width is set 
             # by the column that contains the box
             #tabPanel(title = tagList(shiny::icon("map-marker"), "CaPO4 Homeostasis"),
             
             div(id = "network_cap",
                 
                 withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                             size = 2, 
                             type = 6, 
                             color = "#000000")
             ),
             column(6, align ="center",
                    verbatimTextOutput("id")
             ),
             column(6, align ="center",
                    verbatimTextOutput("id_bis")
             ),
             column(6, align ="center",
                    verbatimTextOutput("id_tris")
             ),
             column(6, align ="center",
                    verbatimTextOutput("position")
             ),
             column(6, align ="center",
                    verbatimTextOutput("viewposition")
             ),
             column(12,
                    verbatimTextOutput("test")
             )
             #downloadButton("downloadData", "Download Data")
             
             #)
           ) 
    ),
    
    column(width = 6, offset = 0, style='padding:0px;',
           
           box(
             id = "tabset1",
             #title = tagList(shiny::icon("microchip"), "Results"), 
             height = "450px", width = 12,
             
             #tabPanel(title = tagList(shiny::icon("line-chart"), "Time Plot"),
             
             column(6, align = "center", offset = 0, style='padding:0px;',
                    
                    withSpinner(plotlyOutput("plot_node"), 
                                size = 2, 
                                type = 6, 
                                color = "#000000")
             ),
             column(6, align = "center", offset = 0, style='padding:0px;',
                    
                    withSpinner(plotlyOutput("plot_edge"), 
                                size = 2, 
                                type = 6, 
                                color = "#000000")
             )
           ),
           
           div(id = "boxinput", # values to be reset if needed
               box(
                 # classic id does not work with tabBox to reset values inside ...
                 #title = tagList(shiny::icon("gear"), "Control Center"), width = NULL,
                 height = "500px", width = 12,
                 #tabPanel(title = tagList(shiny::icon("sliders"), "Parameters"),
                 column(8, align="left", offset = 0, style='padding:0px;',
                        
                        # the zoom graph
                        conditionalPanel(
                          condition = "input.current_node_bis_id == 16",
                          div(id = "networkPTH",
                              visNetworkOutput("network_PTH", height = "400px")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.current_node_bis_id == 9",
                          div(id = "network_PTzoom",
                              imageOutput("PT")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.current_node_bis_id == 10",
                          div(id = "network_TALzoom",
                              imageOutput("TAL")
                              )
                        ),
                        conditionalPanel(
                          condition = "input.current_node_bis_id == 11",
                          div(id = "network_DCT-CNTzoom",
                              imageOutput("DCT_CNT")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.current_node_bis_id == 2",
                          div(id = "network_intestinezoom",
                              imageOutput("intestine_zoom")
                          )
                        ),conditionalPanel(
                          condition = "input.current_node_bis_id == 6",
                          div(id = "network_bonezoom",
                              imageOutput("bone_zoom")
                          )
                        )
                        
                 ),
                 
                 # Parameter sliders
                 column(4, align = "right", offset = 0, style='padding:0px;',
                        
                        conditionalPanel( # PTH parameters, help is added beside each slider with tooltip
                          
                          condition = "input.current_edge_bis_id != 'null' && 
                                                input.current_edge_bis_id == 1",
                          
                          sliderInput("k_prod_PTHg", 
                                      "PTH synthesis rate constant (μmol/min)",
                                      min = 0, 
                                      max = 100, 
                                      value = 1, 
                                      step = 1) %>%
                            shinyInput_label_embed(
                              icon("undo") %>%
                                actionBttn(inputId="resetPTHsynthesis",
                                           label="", 
                                           color="danger", 
                                           size = "xs"))
                        ),
                        
                        conditionalPanel( # PTH parameters for PTH exocytosis
                          
                          condition = "input.current_edge_bis_id != 'null' && 
                                                input.current_edge_bis_id == 3",
                          
                          sliderInput("beta_exo_PTHg", 
                                      "Maximal PTH secretion from parathyroid 
                                                        glands (1/min)", 
                                      min = 0, 
                                      max = 2, 
                                      value = 1, 
                                      step = 0.1) %>%
                            shinyInput_label_embed(
                              icon("undo") %>%
                                actionBttn(inputId="resetPTHexocytosis",
                                           label="", 
                                           color="danger", 
                                           size = "xs")),
                          
                          # generate the alert when beta_exo_PTHg may crash the solver
                          receiveSweetAlert(messageId = "failSw")
                        ),
                        
                        conditionalPanel( # PTH parameters for PTH exocytosis
                          
                          condition = "input.current_edge_bis_id != 'null' && 
                                                input.current_edge_bis_id == 4",
                          
                          sliderInput("gamma_exo_PTHg", 
                                      "Maximal inhibition of PTH 
                                                        secretion from parathyroid glands 
                                                        by calcium (1/min)", 
                                      min = 0, 
                                      max = 1, 
                                      value = 1, 
                                      step = 0.1) %>%
                            shinyInput_label_embed(
                              icon("undo") %>%
                                actionBttn(inputId="resetPTHexocytosisinhib",
                                           label="", 
                                           color="danger", 
                                           size = "xs"))
                          
                        ),
                        
                        conditionalPanel( # PTH parameters for both synthesis and exocytosis
                          
                          condition = "/1/.test(input.current_edge_bis_id) && 
                                                /4/.test(input.current_edge_bis_id)",
                          
                          fluidRow(
                            column(8, align = "center", 
                                   sliderInput("k_prod_PTHg", 
                                               "PTH synthesis rate constant (μmol/min)", 
                                               min = 0, 
                                               max = 100, 
                                               value = 1, 
                                               step = 1) %>%
                                     shinyInput_label_embed(
                                       icon("undo") %>%
                                         actionBttn(inputId="resetPTHsynthesis",
                                                    label="", 
                                                    color="danger", 
                                                    size = "xs"))
                            )),
                          
                          fluidRow(
                            column(8, align = "center",
                                   
                                   sliderInput("beta_exo_PTHg", 
                                               "Maximal PTH secretion from parathyroid 
                                                        glands (1/min)", 
                                               min = 0, 
                                               max = 2, 
                                               value = 1, 
                                               step = 0.1) %>%
                                     shinyInput_label_embed(
                                       icon("undo") %>%
                                         actionBttn(inputId="resetPTHexocytosis",
                                                    label="", 
                                                    color="danger", 
                                                    size = "xs")),
                                   
                                   # generate the alert when beta_exo_PTHg may crash the solver
                                   receiveSweetAlert(messageId = "failSw")
                                   
                            )),
                          
                          fluidRow(
                            column(8, align = "center",
                                   
                                   sliderInput("gamma_exo_PTHg", 
                                               "Maximal inhibition of PTH secretion 
                                                        from parathyroid glands by calcium (1/min)", 
                                               min = 0, 
                                               max = 1, 
                                               value = 1, 
                                               step = 0.1) %>%
                                     shinyInput_label_embed(
                                       icon("undo") %>%
                                         actionBttn(inputId="resetPTHexocytosisinhib",
                                                    label="", 
                                                    color="danger", 
                                                    size = "xs"))
                            ))
                        ),
                        
                        
                        conditionalPanel( # Vitamin D3 parameters
                          
                          condition = "input.current_node_id == 10 || 
                                                input.current_node_id == 11",
                          
                          column(12, align = "center", 
                                 sliderInput("D3_inact", 
                                             "Plasma concentration of 25(OH)D (nM)", 
                                             min = 0, 
                                             max = 100, 
                                             value = 1, 
                                             step = 1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetD3inact",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          ),
                          column(12, align = "center",            
                                 sliderInput("k_deg_D3", 
                                             "Rate constant for vitamin D3 
                                                                     degradation (1/min)", 
                                             min = 0, 
                                             max = 10, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetD3deg",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          )
                          
                        ),
                        
                        
                        conditionalPanel( # FGF parameters
                          
                          condition = "input.current_node_id == 12",
                          
                          sliderInput("k_prod_FGF", 
                                      "Minimal rate of FGF23 synthesis (fM/min)", 
                                      min = 0, 
                                      max = 10, 
                                      value = 1, 
                                      step = 0.1) %>%
                            shinyInput_label_embed(
                              icon("undo") %>%
                                actionBttn(inputId="resetFGFsynth",
                                           label="", 
                                           color="danger", 
                                           size = "xs"))
                        ),
                        
                        
                        conditionalPanel( # Intake parameters
                          
                          condition = "input.current_node_id == 1",
                          
                          column(12, align = "center", 
                                 sliderInput("I_Ca", 
                                             "Calcium intake (μmol/min)", 
                                             min = 0, 
                                             max = 10, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetCaintake",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          ),
                          column(12, align = "center", 
                                 sliderInput("I_P", 
                                             "Phosphate intake (μmol/min)", 
                                             min = 0, 
                                             max = 10, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetPintake",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          )
                          
                          
                        ),
                        
                        
                        conditionalPanel( # Fast Bone parameters
                          
                          condition = "input.current_node_id == 5",
                          
                          column(6, align = "center",           
                                 sliderInput("k_p_Ca", 
                                             "Ca transfer 
                                                      from plasma to fast bone pool (1/min)", 
                                             min = 0, 
                                             max = 2, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetkpCa",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          ),
                          column(6, align = "center",             
                                 sliderInput("k_f_Ca", 
                                             "Ca transfer 
                                                      from fast bone pool to plasma (1/min)", 
                                             min = 0, 
                                             max = 2, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetkfCa",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          ),
                          column(6, align = "center",           
                                 sliderInput("k_p_P", 
                                             "PO4 transfer from 
                                                      plasma to fast bone pool (1/min)", 
                                             min = 0, 
                                             max = 2, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetkpP",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          ),
                          column(6, align = "center",           
                                 sliderInput("k_f_P", 
                                             "PO4 transfer from 
                                                       fast bone pool to plasma (1/min)", 
                                             min = 0, 
                                             max = 2, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetkfP",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          )
                          
                          
                          
                        ),
                        
                        conditionalPanel( # Slow Bone parameters
                          
                          condition = "input.current_node_id == 6",
                          column(12, align = "center",           
                                 sliderInput("Lambda_ac_Ca", 
                                             "rate constant of Ca flux into bone (1/min)", 
                                             min = 0, 
                                             max = 10, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetacCa",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          ),
                          column(12, align = "center",            
                                 sliderInput("Lambda_res_min", 
                                             "Minimal resorption rate (μmol/min)", 
                                             min = 0, 
                                             max = 10, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetresmin",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          ),
                          column(12, align = "center",            
                                 sliderInput("delta_res_max", 
                                             "Maximal resorption rate (μmol/min)", 
                                             min = 0, 
                                             max = 10, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetresmax",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          )
                          
                          
                        ),
                        
                        conditionalPanel( # Cells parameters
                          
                          condition = "input.current_node_id == 13",
                          
                          column(12, align = "center",           
                                 sliderInput("k_pc", 
                                             "PO4 transfer from 
                                                           plasma pool to intracellular (1/min)", 
                                             min = 0, 
                                             max = 2, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetkpc",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          ),
                          column(12, align = "center",            
                                 sliderInput("k_cp", 
                                             "PO4 transfer from 
                                                           intracellular pool to plasma (1/min)", 
                                             min = 0, 
                                             max = 2, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("undo") %>%
                                       actionBttn(inputId="resetkcp",
                                                  label="", 
                                                  color="danger", 
                                                  size = "xs"))
                          )
                          
                        ),
                        
                        
                        conditionalPanel( # CaPO4 group
                          
                          condition = "input.CaPO4group",
                          
                          column(6, align = "center",
                                 sliderInput("k_fet", 
                                             "$$ k_f^{fetA} $$", 
                                             min = 0, 
                                             max = 10, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("info") %>%
                                       bs_embed_tooltip(title = "Fetuin-A binding to CaHPO4 and 
                                                                     CaH2PO4+ (1/min)"))
                          ),
                          column(6, align = "center",           
                                 sliderInput("k_c_CPP", 
                                             "$$ k_c^{CPP} $$", 
                                             min = 0, 
                                             max = 100, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("info") %>%
                                       bs_embed_tooltip(title = "CPP degradation rate constant (1/min)"))
                          ),
                          column(6, align = "center",          
                                 sliderInput("Na", 
                                             "$$ [Na^+]_p $$", 
                                             min = 0, 
                                             max = 10, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("info") %>%
                                       bs_embed_tooltip(title = "Sodium plasma concentration (mM)"))
                          ),
                          column(6, align = "center",           
                                 sliderInput("Prot_tot_p", 
                                             "$$ [Prot^{tot}]_p $$", 
                                             min = 0.25, 
                                             max = 2, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("info") %>%
                                       bs_embed_tooltip(title = "Concentration of proteins in plasma (mM)"))
                          )
                          
                        ),
                        
                        
                        conditionalPanel( # Other parameters
                          
                          condition = "input.others",
                          
                          column(12, align = "center",
                                 sliderInput("Vp", 
                                             "$$ V_p $$", 
                                             min = 0.7, 
                                             max = 1.3, 
                                             value = 1, 
                                             step = 0.1) %>% # + - 30% variation
                                   shinyInput_label_embed(
                                     icon("info") %>%
                                       bs_embed_tooltip(title = "Plasma volume (mL)"))
                          ),
                          column(12, align = "center",            
                                 sliderInput("GFR", 
                                             "$$ GFR $$", 
                                             min = 0, 
                                             max = 1.5, 
                                             value = 1, 
                                             step = 0.1) %>%
                                   shinyInput_label_embed(
                                     icon("info") %>%
                                       bs_embed_tooltip(title = "glomerular filtration rate (mL/min)"))
                          )
                          #sliderInput("tau", "Value of tau:", min = 0, max = 500, value = 240, step = 1) # integrate the delay
                          
                          #sliderInput("pH", "Value of pH:", min = 0, max = 12, value = 7.4, step = 1) # integrate effect of pH
                          
                        )
                        
                 )
               )
           )
           
    )
  )
)