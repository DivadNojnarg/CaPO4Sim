#-------------------------------------------------------------------------
#  This code contains the sidebar of shinydashboard. 
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------
source("help.R")

sidebar <- dashboardSidebar(
  width = 300,
  
  introBox(
    
    sidebarMenu(
      
      id = "sidebar_main",
      
      #menuSegment("MAIN NAVIGATION"),
      
      menuItem("About", tabName = "about", icon = icon("info-circle")), 
      menuItem("Help", tabName = "help", icon = icon("question-circle"),
               actionBttn("help", label = "Help", icon = NULL, style = "unite",
                          color = "danger", size = "lg", block = FALSE, no_outline = TRUE)),
      menuItem("Demo", tabName = "demo", icon = icon("youtube-play")),
      menuItem("App", tabName = "main", icon = icon("home"), selected = TRUE),
      menuItem("Settings", tabName = "settings", icon = icon("sliders"),
               
               # show background or not
               awesomeCheckboxGroup(inputId = "background_choice",
                                    label = "Background",
                                    choices = c("rat","human"), inline = TRUE,
                                    status = "primary"),
               
               # select the network
               awesomeCheckboxGroup(inputId = "network_Ca_choice",
                                    label = "Choose your network", 
                                    selected = "Ca",
                                    choices = c("Ca","PO4"), inline = TRUE,
                                    status = "primary"),
               
               # selector for hormonal regulation
               materialSwitch(inputId = "network_hormonal_choice", 
                              label = "Show regulations", 
                              status = "success",
                              value = FALSE),
               
               # maximum time of integration
               numericInput("tmax",
                            "Maximum simulated time:", 
                            value = 500, 
                            min = 0, 
                            max = NA,
                            width = "100%"),
               
               # navigate to a given time
               sliderInput("t_now",
                           "Time after simulation:", 
                           value = 500, 
                           min = 1, 
                           max = 500,
                           width = "90%") %>%
                 shinyInput_label_embed(
                   icon("undo") %>%
                     actionBttn(inputId = "reset_t_now",
                                label = "", 
                                color = "danger", 
                                size = "xs"))
      ),
      menuItem("Diseases", tabName = "diseases", icon = icon("heartbeat"),
               
               prettyCheckboxGroup(inputId = "disease_selected",
                                   label = "Select a disease:", thick = TRUE,
                                   choices = c("Hyperparathyroidism I" = "primary-hyperparathyroidism", 
                                               "Hypoparathyroidism" = "hypoparathyroidism", 
                                               "vitamin D3 deficiency" = "vitamin D3 deficiency"),
                                   selected = "",
                                   animation = "pulse", status = "info",
                                   width = "100%")
               
      ),
      menuItem("Treatments", tabName = "treatments", icon = icon("user-md"),
               div(id = "dropdown_treatment",
                   
                   
                   prettyCheckboxGroup(inputId = "treatment_selected",
                                       label = "Select a treatment:", thick = TRUE,
                                       choices = c("parathyroid surgery",
                                                   "vitamin D3 iv injection",
                                                   "Ca supplementation",
                                                   "Ca iv injection",
                                                   "PO4 supplementation",
                                                   "PO4 iv injection",
                                                   "cinacalcet"),
                                       selected = "",
                                       animation = "pulse", status = "info",
                                       width = "100%"),
                   
                   
                   conditionalPanel( # for Ca iv injection
                     # special JS condition to test if an element is in a list
                     condition = "/Ca iv injection/.test(input.treatment_selected)", 
                     
                     sliderInput("Ca_inject", 
                                 "$$ k_{inject}^{Ca} $$", 
                                 min = 0, 
                                 max = 0.002, 
                                 value = 0.001, 
                                 step = 0.0001) %>%
                       shinyInput_label_embed(
                         icon("info") %>%
                           bs_embed_tooltip(title = "Rate of injection of calcium in plasma (μmol/min)")),
                     
                     numericInput("t_start_Cainject",
                                  "Time when begins the Ca iv injection:", 
                                  value = 0, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     numericInput("t_stop_Cainject",
                                  "Time,when stops the Ca iv injection:", 
                                  value = 100, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     column(12, align = "center",
                            
                            actionBttn(inputId = "add_newCaiv", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "primary", 
                                       icon = icon("plus"))
                     ),
                     br(),
                     br(),
                     column(12, align = "center",
                            
                            actionBttn(inputId = "delete_oldCaiv", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "danger", 
                                       icon = icon("minus"))
                     ),
                     column(12, align = "center",
                            
                            numericInput("delete_Caiv_id",
                                         "Event to remove:", 
                                         value = 1, 
                                         min = 1, 
                                         max = NA, 
                                         width = "100%")
                     )
                   ),
                   
                   conditionalPanel( # for Ca food
                     condition = "/Ca supplementation/.test(input.treatment_selected)", 
                     
                     sliderInput("Ca_food", 
                                 "Ca intake", 
                                 value = 0.0022,
                                 min = 0, 
                                 max = 0.008, 
                                 step = 0.0001) %>%
                       shinyInput_label_embed(
                         icon("info") %>%
                           bs_embed_tooltip(title = "Calcium intake (μmol/min)")),
                     
                     numericInput("t_start_Caintake",
                                  "Time when begins the Ca supplementation:", 
                                  value = 0, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     numericInput("t_stop_Caintake",
                                  "Time when stops the Ca supplementation:", 
                                  value = 100, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     column(12, align = "center",
                            
                            actionBttn(inputId = "add_newCaintake", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "primary", 
                                       icon = icon("plus"))
                     ),
                     br(),
                     br(),
                     column(12, align = "center",
                            
                            actionBttn(inputId = "delete_oldCaintake", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "danger", 
                                       icon = icon("minus"))
                     ),
                     column(12, align = "center",
                            
                            numericInput("delete_oldCaintake_id",
                                         "Event to remove:", 
                                         value = 1, 
                                         min = 1, 
                                         max = NA, 
                                         width = "100%")
                     )
                   ),
                   
                   conditionalPanel( # for D3 iv injection
                     condition = "/vitamin D3 iv injection/.test(input.treatment_selected)", 
                     
                     sliderInput("D3_inject", 
                                 "D3 injection", 
                                 value = 0.001,
                                 min = 0, 
                                 max = 0.1, 
                                 step = 0.001) %>%
                       shinyInput_label_embed(
                         icon("info") %>%
                           bs_embed_tooltip(title = "D3 injection (pmol/min)")),
                     
                     numericInput("t_start_D3inject",
                                  "Time when begins the D3 iv injection:", 
                                  value = 0, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     numericInput("t_stop_D3inject",
                                  "Time when stops the D3 iv injection:", 
                                  value = 100, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     column(12, align = "center",
                            
                            actionBttn(inputId = "add_newD3iv", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "primary", 
                                       icon = icon("plus"))
                     ),
                     br(),
                     br(),
                     column(12, align = "center",
                            
                            actionBttn(inputId = "delete_oldD3iv", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "danger", 
                                       icon = icon("minus"))
                     ),
                     column(12, align = "center",
                            
                            numericInput("delete_oldD3iv_id",
                                         "Event to remove:", 
                                         value = 1, 
                                         min = 1, 
                                         max = NA, 
                                         width = "100%")
                     )
                   ),
                   
                   conditionalPanel( # for PO4 iv injection
                     condition = "/PO4 iv injection/.test(input.treatment_selected)", 
                     
                     sliderInput("P_inject", 
                                 "PO4 injection", 
                                 value = 0.001, 
                                 min = 0, 
                                 max = 0.01, 
                                 step = 0.0001) %>%
                       shinyInput_label_embed(
                         icon("info") %>%
                           bs_embed_tooltip(title = "PO4 injection (μmol/min)")),
                     
                     numericInput("t_start_Pinject",
                                  "Time when begins the PO4 iv injection:", 
                                  value = 0, 
                                  min = 0, 
                                  max = NA,
                                  width = "100%"),
                     
                     numericInput("t_stop_Pinject",
                                  "Time when stops the PO4 iv injection:", 
                                  value = 100, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     column(12, align = "center",
                            
                            actionBttn(inputId = "add_newPiv", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "primary", 
                                       icon = icon("plus"))
                     ),
                     br(),
                     br(),
                     column(12, align = "center",
                            
                            actionBttn(inputId = "delete_oldPiv", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "danger", 
                                       icon = icon("minus"))
                     ),
                     column(12, align = "center",
                            
                            numericInput("delete_oldPiv_id",
                                         "Event to remove:", 
                                         value = 1, 
                                         min = 1, 
                                         max = NA, 
                                         width = "100%")
                     )
                   ),
                   
                   conditionalPanel( # for PO4 supplementation
                     condition = "/PO4 supplementation/.test(input.treatment_selected)", 
                     
                     sliderInput("P_food", 
                                 "PO4 intake", 
                                 value = 1.55e-003,
                                 min = 0, 
                                 max = 0.01, 
                                 step = 0.0001) %>%
                       shinyInput_label_embed(
                         icon("info") %>%
                           bs_embed_tooltip(title = "Phosphate intake (μmol/min)")),
                     
                     numericInput("t_start_Pintake",
                                  "Time when begins the Ca supplementation:", 
                                  value = 0, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     numericInput("t_stop_Pintake",
                                  "Time when stops the Ca supplementation:", 
                                  value = 100, 
                                  min = 0, 
                                  max = NA, 
                                  width = "100%"),
                     
                     column(12, align = "center",
                            
                            actionBttn(inputId = "add_newPintake", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "primary", 
                                       icon = icon("plus"))
                     ),
                     br(),
                     br(),
                     column(12, align = "center",
                            
                            actionBttn(inputId = "delete_oldPintake", 
                                       label = NULL, 
                                       style = "material-circle", 
                                       color = "danger", 
                                       icon = icon("minus"))
                     ),
                     column(12, align = "center",
                            
                            numericInput("delete_oldPintake_id",
                                         "Event to remove:", 
                                         value = 1, 
                                         min = 1, 
                                         max = NA, 
                                         width = "100%")
                     )
                   )
               )
      )
    ),
    data.step = 1,
    data.intro = help_text[1]
  )
)

