#-------------------------------------------------------------------------
#  This code contains the header of shinydashboard. It is modified compared
#  to classic header. Indeed, some buttons to save, load, reset, download are
#  inserted in the header bar. Moreover, users can change the global theme
#  clicking on the theme selector.
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------

header <- dashboardHeader(
  title="Calcium Phosphate Homeostasis: treatment App", titleWidth = 500,
  
  # play button to launch simulation
  
  tags$li(
    title = "",
    class = "dropdown",
    actionBttn(inputId = "play", 
               label = "Run", 
               style = "fill", 
               color = "primary", 
               icon = icon("play"))
    
  ),
  
  # tmax value and other options
  
  tags$li(
    title = "",
    class = "dropdown",
    dropdownButton(icon = icon("gear"), width = "150px", circle = FALSE, 
                   status = "danger", label = "Options",
                   
                   numericInput("tmax",
                                "Value of tmax:", 
                                value = 500, 
                                min = 0, 
                                max = NA, 
                                width = "50%")
                   
    )
  ),
  
  # Disease selector
  
  tags$li(
    title = "",
    class = "dropdown",
    dropdownButton(icon = icon("stethoscope"), width = "350px", circle = FALSE, 
                   status = "primary", label = "Disease",
                   
                   multiInput(
                     inputId = "disease_selected", 
                     label = "Select a disease :",
                     choices = c("primary-hyperparathyroidism", "hypoparathyroidism", 
                                 "vitamin D3 deficiency", "pseudohypoparathyroidism"),
                     selected = "", 
                     width = "100%")
                   
    )
  ),
  
  # Treatment selector
  
  tags$li(
    title = "",
    class = "dropdown",
    dropdownButton(icon = icon("medkit"), width = "310px", circle = FALSE, 
                   status = "primary", label = "Treatment",
                   
                   div(id = "dropdown_treatment",
                     multiInput(
                       inputId = "treatment_selected", 
                       label = "Select a treatment :",
                       choices = c("parathyroid surgery","vitamin D3 iv injection",
                                   "Ca supplementation","Ca iv injection",
                                   "PO4 supplementation","PO4 iv injection","cinacalcet"),
                       selected = "", 
                       width = "100%"),
                     
                     conditionalPanel( # for Ca iv injection
                       condition = "/Ca iv injection/.test(input.treatment_selected)", # special JS condition to test if an element is in a list
                       
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
                                    width = "50%"),
                       
                       numericInput("t_stop_Cainject",
                                    "Time,when stops the Ca iv injection:", 
                                    value = 100, 
                                    min = 0, 
                                    max = NA, 
                                    width = "50%"),
                       
                       column(6, align = "center",
                              
                              actionBttn(inputId = "add_newCaiv", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "primary", 
                                         icon = icon("plus"))
                       ),
                       column(6, align = "center",
                              
                              actionBttn(inputId = "delete_oldCaiv", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "danger", 
                                         icon = icon("minus")),
                              
                              numericInput("delete_Caiv_id",
                                           "Event to remove?", 
                                           value = 1, 
                                           min = 1, 
                                           max = NA, 
                                           width = "50%")
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
                                    width = "50%"),
                       
                       numericInput("t_stop_Caintake",
                                    "Time when stops the Ca supplementation:", 
                                    value = 100, 
                                    min = 0, 
                                    max = NA, 
                                    width = "50%"),
                       
                       column(6, align = "center",
                              
                              actionBttn(inputId = "add_newCaintake", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "primary", 
                                         icon = icon("plus"))
                       ),
                       column(6, align = "center",
                              
                              actionBttn(inputId = "delete_oldCaintake", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "danger", 
                                         icon = icon("minus")),
                              
                              numericInput("delete_Caintake_id",
                                           "Event to remove?", 
                                           value = 1, 
                                           min = 1, 
                                           max = NA, 
                                           width = "50%")
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
                                    width = "50%"),
                       
                       numericInput("t_stop_D3inject",
                                    "Time when stops the D3 iv injection:", 
                                    value = 100, 
                                    min = 0, 
                                    max = NA, 
                                    width = "50%"),
                       
                       column(6, align = "center",
                              
                              actionBttn(inputId = "add_newD3iv", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "primary", 
                                         icon = icon("plus"))
                       ),
                       column(6, align = "center",
                              
                              actionBttn(inputId = "delete_oldD3iv", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "danger", 
                                         icon = icon("minus")),
                              
                              numericInput("delete_D3iv_id",
                                           "Event to remove?", 
                                           value = 1, 
                                           min = 1, 
                                           max = NA, 
                                           width = "50%")
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
                                    width = "50%"),
                       
                       numericInput("t_stop_Pinject",
                                    "Time when stops the PO4 iv injection:", 
                                    value = 100, 
                                    min = 0, 
                                    max = NA, 
                                    width = "50%"),
                       
                       column(6, align = "center",
                              
                              actionBttn(inputId = "add_newPiv", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "primary", 
                                         icon = icon("plus"))
                       ),
                       column(6, align = "center",
                              
                              actionBttn(inputId = "delete_oldPiv", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "danger", 
                                         icon = icon("minus")),
                              
                              numericInput("delete_Piv_id",
                                           "Event to remove?", 
                                           value = 1, 
                                           min = 1, 
                                           max = NA, 
                                           width = "50%")
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
                                    width = "50%"),
                       
                       numericInput("t_stop_Pintake",
                                    "Time when stops the Ca supplementation:", 
                                    value = 100, 
                                    min = 0, 
                                    max = NA, 
                                    width = "50%"),
                       
                       column(6, align = "center",
                              
                              actionBttn(inputId = "add_newPintake", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "primary", 
                                         icon = icon("plus"))
                       ),
                       column(6, align = "center",
                              
                              actionBttn(inputId = "delete_oldPintake", 
                                         label = NULL, 
                                         style = "material-circle", 
                                         color = "danger", 
                                         icon = icon("minus")),
                              numericInput("delete_Pintake_id",
                                           "Event to remove?", 
                                           value = 1, 
                                           min = 1, 
                                           max = NA, 
                                           width = "50%")
                       )
                       
                     )
                   )
                   
    )
  ),
  
  # Share the state of the App with other users and reset the application
  
  tags$li(
    title = "",
    class = "dropdown",
    
    actionButton(inputId = "bookmark", 
                 label = "Share", 
                 icon = shiny::icon("link", lib = "glyphicon"), 
                 class="btn btn-primary"),
    
    actionButton(class="fa fa-trash fa-5x", 
                 inputId="resetAll",
                 label=" Reset", 
                 class="btn btn-danger")
  ),
  
  # Help menu
  
  dropdownMenu(type = "notifications", 
               badgeStatus = "warning",
               notificationItem(icon = icon("users"), 
                                status = "info",
                                "5 new members joined today"),
               
               notificationItem(icon = icon("warning"), 
                                status = "danger",
                                "Resource usage near limit."),
               
               notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
                                status = "success", 
                                "25 sales made"),
               
               notificationItem(icon = icon("user", lib = "glyphicon"),
                                status = "danger", 
                                "You changed your username")
  )
  
)