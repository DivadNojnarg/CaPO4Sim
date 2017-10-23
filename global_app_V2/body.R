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
        
             introBox(
               div(id = "network_cap",
                   
                   withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                               size = 2, 
                               type = 6, 
                               color = "#000000")
               ),
               data.step = 1,
               data.intro = help_text[1]
             )#,
             #column(6, align = "center",
             #        verbatimTextOutput("browser")
             #)
             # column(6, align = "center",
             #         verbatimTextOutput("edges_id_bone")
             # ),
             #column(6, align = "center",
             #        dataTableOutput("table")
             #)
             #column(6, align ="center",
             #       verbatimTextOutput("node_tris")
             #),
             #column(6, align ="center",
             #       verbatimTextOutput("id")
             # )
             # column(6, align ="center",
             #        verbatimTextOutput("id_bis")
             # ),
             # column(6, align ="center",
             #        verbatimTextOutput("id_tris")
             # ),
             # column(6, align = "center",
             #        verbatimTextOutput("position")
             # ),
             # column(6, align = "center",
             #        verbatimTextOutput("viewposition")
             # )
             # column(12,
             #        verbatimTextOutput("test")
             # )
             #downloadButton("downloadData", "Download Data")
             
             #)
           ) 
    ),
    
    column(width = 6, offset = 0, style='padding:0px;',
           
           box(
             id = "tabset1",
             width = 12,
             collapsible = TRUE,
             solidHeader = TRUE,
             
             column(6, align = "center", offset = 0, style='padding:0px;',
                    
                    introBox(
                      withSpinner(plotlyOutput("plot_node", height = "300px"), 
                                  size = 2, 
                                  type = 6, 
                                  color = "#000000"),
                      data.step = 2,
                      data.intro = help_text[2]
                    )
                    
             ),
             column(6, align = "center", offset = 0, style='padding:0px;',
                    introBox(
                      withSpinner(plotlyOutput("plot_edge", height = "300px"), 
                                  size = 2, 
                                  type = 6, 
                                  color = "#000000"),
                      data.step = 3,
                      data.intro = help_text[3]
                    )
             )
           ),
           
           div(id = "boxinput", # values to be reset if needed
               box(
                 width = 12,
                 #title = header_box_network,
                 solidHeader = TRUE,
                 
                 column(8, align = "left", offset = 0, style = 'padding:0px;',
                        
                        conditionalPanel(
                          condition = "input.current_node_bis_id == null",
                          "Please double-click on a node to display its
                          detailed content (only available for parathyroid glands,
                          bones, kidneys and intestine)."
                        ),
                        
                        introBox(
                          # the PTH zoom graph
                          # this panel is also shown when help button is clicked
                          conditionalPanel(
                            condition = "input.current_node_bis_id == 11 ||
                                         input.notif_switch",
                            
                            div(id = "networkPTH",
                                visNetworkOutput("network_PTH", height = "400px")
                            )
                            
                          ),
                          data.step = 4,
                          data.intro = help_text[4]
                        ),
                        
                        # Kidney zoom 1
                        conditionalPanel(
                          condition = "input.current_node_bis_id == 6",
                          
                          div(id = "networkkidney_zoom2",
                              visNetworkOutput("network_kidney_zoom2", height = "400px")
                          )
                          
                        ),
                        
                        # Proximal tubule zoom
                        conditionalPanel(
                          condition = "input.current_node_tris_id == 1 &&
                                       input.current_node_bis_id != 'null' &&
                                       input.current_node_tris_id != 'null'",
                          
                          div(id = "networkkidney_PT",
                              visNetworkOutput("network_kidney_PT", height = "400px")
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
                        
                 ),
                 
                 # Parameter sliders
                 column(4, align = "right", offset = 0, style = 'padding:0px;',
                        introBox(
                          # PTH parameters, help is added beside each slider with tooltip
                          # this slider can also be seen when help button is clicked
                          conditionalPanel(
                            
                            condition = "input.current_edge_bis_id != 'null' && 
                                         input.current_edge_bis_id == 1 && 
                                         input.current_node_bis_id == 11 ||
                                         input.notif_switch",
                            
                            sliderInput("k_prod_PTHg", 
                                        "PTH synthesis rate constant (μmol/min)",
                                        min = 0, 
                                        max = 100, 
                                        value = 1,
                                        step = 1) %>%
                              shinyInput_label_embed(
                                icon("undo") %>%
                                  actionBttn(inputId = "resetPTHsynthesis",
                                             label = "", 
                                             color = "danger", 
                                             size = "xs"))
                          ),
                          
                          conditionalPanel( # PTH parameters for PTH exocytosis
                            
                            condition = "input.current_edge_bis_id != 'null' && 
                                         input.current_edge_bis_id == 3 &&
                                         input.current_node_bis_id == 11",
                            
                            sliderInput("beta_exo_PTHg", 
                                        "Maximal PTH secretion from parathyroid 
                                        glands (1/min)", 
                                        min = 0, 
                                        max = 2, 
                                        value = 1, 
                                        step = 0.1) %>%
                              shinyInput_label_embed(
                                icon("undo") %>%
                                  actionBttn(inputId = "resetPTHexocytosis",
                                             label = "", 
                                             color = "danger", 
                                             size = "xs")),
                            
                            # generate the alert when beta_exo_PTHg may crash the solver
                            useSweetAlert()
                          ),
                          
                          conditionalPanel( # PTH parameters for PTH exocytosis
                            
                            condition = "input.current_edge_bis_id != 'null' && 
                                         input.current_edge_bis_id == 4 &&
                                         input.current_node_bis_id == 11",
                            
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
                                  actionBttn(inputId = "resetPTHexocytosisinhib",
                                             label = "", 
                                             color = "danger", 
                                             size = "xs"))
                            
                          ),
                          
                          conditionalPanel( # Vitamin D3 parameters
                            
                            condition = "input.current_node_id == 13 || 
                                         input.current_node_id == 14 ||
                                         input.current_node_id == 15",
                            
                            column(12, align = "center", 
                                   sliderInput("D3_inact", 
                                               "Plasma concentration of 25(OH)D (nM)", 
                                               min = 0, 
                                               max = 100, 
                                               value = 1, 
                                               step = 1) %>%
                                     shinyInput_label_embed(
                                       icon("undo") %>%
                                         actionBttn(inputId = "resetD3inact",
                                                    label = "", 
                                                    color = "danger", 
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
                                         actionBttn(inputId = "resetD3deg",
                                                    label = "", 
                                                    color = "danger", 
                                                    size = "xs"))
                            )
                            
                          ),
                          
                          
                          conditionalPanel( # FGF parameters
                            
                            condition = "input.current_node_id == 16",
                            
                            sliderInput("k_prod_FGF", 
                                        "Minimal rate of FGF23 synthesis (fM/min)", 
                                        min = 0, 
                                        max = 10, 
                                        value = 1, 
                                        step = 0.1) %>%
                              shinyInput_label_embed(
                                icon("undo") %>%
                                  actionBttn(inputId = "resetFGFsynth",
                                             label = "", 
                                             color = "danger", 
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
                                         actionBttn(inputId = "resetCaintake",
                                                    label = "", 
                                                    color = "danger", 
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
                                         actionBttn(inputId = "resetPintake",
                                                    label = "", 
                                                    color = "danger", 
                                                    size = "xs"))
                            )
                          ),
                          
                          
                          conditionalPanel( # Fast Bone parameters
                            
                            condition = "input.current_node_id == 3",
                            
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
                                         actionBttn(inputId = "resetkpCa",
                                                    label = "", 
                                                    color = "danger", 
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
                                         actionBttn(inputId = "resetkfCa",
                                                    label = "", 
                                                    color = "danger", 
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
                                         actionBttn(inputId = "resetkpP",
                                                    label = "", 
                                                    color = "danger", 
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
                                         actionBttn(inputId = "resetkfP",
                                                    label = "", 
                                                    color = "danger", 
                                                    size = "xs"))
                            )
                          ),
                          
                          conditionalPanel( # Slow Bone parameters
                            
                            condition = "input.current_node_id == 4",
                            column(12, align = "center",           
                                   sliderInput("Lambda_ac_Ca", 
                                               "rate constant of Ca flux into bone (1/min)", 
                                               min = 0, 
                                               max = 10, 
                                               value = 1, 
                                               step = 0.1) %>%
                                     shinyInput_label_embed(
                                       icon("undo") %>%
                                         actionBttn(inputId = "resetacCa",
                                                    label = "", 
                                                    color = "danger", 
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
                                         actionBttn(inputId = "resetresmin",
                                                    label = "", 
                                                    color = "danger", 
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
                                         actionBttn(inputId = "resetresmax",
                                                    label = "", 
                                                    color = "danger", 
                                                    size = "xs"))
                            )
                            
                            
                          ),
                          
                          conditionalPanel( # Cells parameters
                            
                            condition = "input.current_node_id == 8",
                            
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
                                         actionBttn(inputId = "resetkpc",
                                                    label = "", 
                                                    color = "danger", 
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
                                         actionBttn(inputId = "resetkcp",
                                                    label = "", 
                                                    color = "danger", 
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
                            
                          ),
                          data.step = 5,
                          data.intro = help_text[5]
                        )
                 )
               )
           )
           
    )
  )
)
