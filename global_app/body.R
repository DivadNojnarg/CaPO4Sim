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
                     title = tagList(shiny::icon("map-o"), "Interactive Map"), 
                     height = "1000px", width = NULL,
                     
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
                         title = tagList(shiny::icon("microchip"), "Results"), 
                         height = "600px", width = NULL,
                         
                         tabPanel(title = tagList(shiny::icon("line-chart"), "Time Plot"),
                                  
                                  column(8, align = "center",
                                         
                                         withSpinner(plotlyOutput("plot_node"), 
                                                     size = 2, type = 6, color = "#000000")
                                  ),
                                  column(4, align = "center",
                                         
                                         withSpinner(plotlyOutput("plot_edge"), 
                                                     size = 2, type = 6, color = "#000000")
                                  ),
                                  
                                  br(),
                                  numericInput("tmax",
                                               "Value of tmax:", 
                                               value = 500, 
                                               min = 0, 
                                               max = NA)
                                  
                         )
                       ),
                       
                       div(id = "boxinput", # values to be reset if needed
                           tabBox(
                             # classic id does not work with tabBox to reset values inside ...
                             title = tagList(shiny::icon("gear"), "Control Center"), 
                             width = NULL, height = "350px",
                             
                             tabPanel(title = tagList(shiny::icon("sliders"), "Parameters"), 
                                      column(3, align="left",
                                             
                                             # awesomeCheckboxGroup(
                                             #   inputId = "parameters_choice", label = "Choose a list of parameters :", 
                                             #   choices = c("PTH" = "PTHgroup", "D3" = "D3group", "FGF" = "FGFgroup", 
                                             #               "Calcium" = "Cagroup", "Phosphate" = "PO4group", "CaPO4 complexes" = "CaPO4group",
                                             #               "Others" = "others"), 
                                             #   inline = FALSE, status = "primary"
                                             # ),
                                             
                                             awesomeCheckbox("PTHgroup", 
                                                             "PTH parameters", 
                                                             value = FALSE, 
                                                             status = "primary", 
                                                             width = NULL),
                                             
                                             awesomeCheckbox("D3group", 
                                                             "D3 parameters", 
                                                             value = FALSE, 
                                                             status = "primary", 
                                                             width = NULL),
                                             
                                             awesomeCheckbox("FGFgroup", 
                                                             "FGF parameters", 
                                                             value = FALSE, 
                                                             status = "primary", 
                                                             width = NULL),
                                             
                                             awesomeCheckbox("Cagroup", 
                                                             "Ca parameters", 
                                                             value = FALSE, 
                                                             status = "primary", 
                                                             width = NULL),
                                             
                                             awesomeCheckbox("PO4group", 
                                                             "PO4 parameters", 
                                                             value = FALSE, 
                                                             status = "primary", 
                                                             width = NULL),
                                             
                                             awesomeCheckbox("CaPO4group", 
                                                             "Complexes parameters", 
                                                             value = FALSE, 
                                                             status = "primary", 
                                                             width = NULL),
                                             
                                             awesomeCheckbox("others", 
                                                             "Others", 
                                                             value = FALSE, 
                                                             status = "primary", 
                                                             width = NULL)
                                             
                                      ),
                                      column(9, align = "center",
                                             
                                             conditionalPanel( # PTH parameters, help is added beside each slider with tooltip
                                               condition = "input.PTHgroup",
                                               
                                               jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
                                                 div(
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("k_prod_PTHg", 
                                                                      "$$ k_{prod}^{PTH_g} $$", 
                                                                      min = 0, 
                                                                      max = 100, 
                                                                      value = 1, 
                                                                      step = 1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "the rate of PTH synthesis 
                                                                                 from parathyroid glands (μmol/min)"))
                                                   ),
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("beta_exo_PTHg",
                                                                      "$$ \\beta_{exo}^{PTH_g} $$", 
                                                                      min = 0, 
                                                                      max = 2, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant for maximal PTH secretion 
                                                                                 from parathyroid glands (1/min)")),
                                                          
                                                          # generate the alert when beta_exo_PTHg may crash the solver
                                                          receiveSweetAlert(messageId = "failSw")
                                                          
                                                   ),
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("gamma_exo_PTHg", 
                                                                      "$$ \\gamma_{exo}^{PTH_g} $$", 
                                                                      min = 0, 
                                                                      max = 1, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant for maximal inhibition 
                                                                                          of PTH secretion from parathyroid glands 
                                                                                          by calcium (1/min)"))
                                                   )
                                                   
                                                 )
                                               )
                                             ),
                                             
                                             conditionalPanel( # Vitamin D3 parameters
                                               condition = "input.D3group",
                                               
                                               jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
                                                 div(
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("D3_inact", 
                                                                      "$$ [D_3^{inact}]_p $$",
                                                                      min = 0, 
                                                                      max = 100, 
                                                                      value = 1, 
                                                                      step = 1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Plasma concentration of 25(OH)D, 
                                                                                 the inactive vitamin D3 (nM)"))
                                                   ),
                                                   column(4, align = "center", 
                                                          
                                                          sliderInput("k_deg_D3", 
                                                                      "$$ k_{deg}^{D3} $$", 
                                                                      min = 0, 
                                                                      max = 10, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant for vitamin D3 degradation (1/min)"))
                                                   )
                                                   
                                                 )
                                               )
                                             ),
                                             
                                             
                                             conditionalPanel( # FGF parameters
                                               condition = "input.FGFgroup",
                                               
                                               sliderInput("k_prod_FGF", 
                                                           "$$ k_{prod}^{FGF} $$", 
                                                           min = 0, 
                                                           max = 10, 
                                                           value = 1, 
                                                           step = 0.1) %>%
                                                 shinyInput_label_embed(
                                                   icon("info") %>%
                                                     bs_embed_tooltip(title = "Minimal rate of FGF23 synthesis (fM/min)"))
                                               
                                             ),
                                             
                                             
                                             conditionalPanel( # Calcium parameters
                                               condition = "input.Cagroup",
                                               
                                               jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
                                                 div(
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("I_Ca", 
                                                                      "$$ I_{Ca} $$", 
                                                                      min = 0, 
                                                                      max = 10, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Calcium intake (μmol/min)"))
                                                   ),
                                                   column(4, align = "center",     
                                                          
                                                          sliderInput("Lambda_ac_Ca", 
                                                                      "$$ \\gamma_{ac}^{Ca} $$", 
                                                                      min = 0, 
                                                                      max = 10, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "rate constant of Ca flux into bone (1/min)"))
                                                   ),
                                                   column(4, align = "center", 
                                                          
                                                          sliderInput("Lambda_res_min", 
                                                                      "$$ \\Gamma_{res}^{min} $$", 
                                                                      min = 0, 
                                                                      max = 10, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Minimal resorption rate (μmol/min)"))
                                                   ),
                                                   column(4, align = "center",   
                                                          
                                                          sliderInput("delta_res_max", 
                                                                      "$$ \\delta_{res}^{max} $$", 
                                                                      min = 0, 
                                                                      max = 10, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Maximal resorption rate (μmol/min)"))
                                                   ),
                                                   column(4, align = "center",   
                                                          
                                                          sliderInput("k_p_Ca", 
                                                                      "$$ k_{pf}^{Ca} $$", 
                                                                      min = 0, 
                                                                      max = 2, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant of calcium transfer 
                                                                                 from plasma to fast bone pool (1/min)"))
                                                   ),
                                                   column(4, align = "center", 
                                                          
                                                          sliderInput("k_f_Ca", 
                                                                      "$$ k_{fp}^{Ca} $$", 
                                                                      min = 0, 
                                                                      max = 2, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant of calcium transfer 
                                                                                 from fast bone pool to plasma (1/min)"))
                                                   )
                                                 )
                                               )
                                               
                                             ),
                                             
                                             
                                             conditionalPanel( # phosphate parameters
                                               condition = "input.PO4group",
                                               
                                               jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
                                                 div(
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("I_P", 
                                                                      "$$ I_{PO_4} $$", 
                                                                      min = 0, 
                                                                      max = 10, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Phosphate intake (μmol/min)"))
                                                   ),
                                                   column(4, align = "center", 
                                                          
                                                          sliderInput("k_pc", 
                                                                      "$$ k_{pc} $$", 
                                                                      min = 0, 
                                                                      max = 2, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant of PO4 transfer 
                                                                                 from plasma pool to intracellular (1/min)"))
                                                   ),
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("k_cp", 
                                                                      "$$ k_{cp} $$", 
                                                                      min = 0, 
                                                                      max = 2, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant of PO4 transfer 
                                                                                 from intracellular pool to plasma (1/min)"))
                                                   ),
                                                   column(4, align = "center", 
                                                          
                                                          sliderInput("k_p_P", 
                                                                      "$$ k_{pf}^{PO_4} $$", 
                                                                      min = 0, 
                                                                      max = 2, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant of PO4 transfer 
                                                                                 from plasma to fast bone pool (1/min)"))
                                                   ),
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("k_f_P", 
                                                                      "$$ k_{fp}^{PO_4} $$", 
                                                                      min = 0, 
                                                                      max = 2, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Rate constant of PO4 transfer 
                                                                                 from fast bone pool to plasma (1/min)"))
                                                   )
                                                 )
                                               )
                                               
                                             ),
                                             
                                             
                                             conditionalPanel( # CaPO4 group
                                               condition = "input.CaPO4group",
                                               
                                               jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
                                                 div(
                                                   column(4, align = "center",
                                                          
                                                          sliderInput("k_fet", 
                                                                      "$$ k_f^{fetA} $$", 
                                                                      min = 0, 
                                                                      max = 10, 
                                                                      value = 1, 
                                                                      step = 0.1) %>%
                                                            shinyInput_label_embed(
                                                              icon("info") %>%
                                                                bs_embed_tooltip(title = "Fetuin-A binding to CaHPO4 and CaH2PO4+ (1/min)"))
                                                   ),
                                                   column(4, align = "center",  
                                                          
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
                                                   column(4, align = "center",  
                                                          
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
                                                   column(4, align = "center", 
                                                          
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
                                                 )
                                               )
                                             ),
                                             
                                             
                                             conditionalPanel( # Other parameters
                                               condition = "input.others",
                                               
                                               jqui_sortabled( # the too tabBoxes for picture and results can be changed. However, the graph cannot be moved anymore
                                                 div(
                                                   column(4, align = "center",
                                                          
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
                                                   column(4, align = "center",     
                                                          
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