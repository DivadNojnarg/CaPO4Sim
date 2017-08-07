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
  
  # tmax value
  
  tags$li(
    title = "",
    class = "dropdown",
    
    numericInput("tmax","Value of tmax:", 500, min = 0, max = NA, width = "50%")
    
  ),
  
  # Disease selector
  
  tags$li(
    title = "",
    class = "dropdown",
    dropdownButton(icon = icon("download"), width = "350px", circle = FALSE, status = "primary", label = "Disease",
                   
                   multiInput(
                     inputId = "disease_selected", label = "Select a disease :",
                     choices = c("primary-hyperparathyroidism", "hypoparathyroidism", "vitamin D3 deficiency", "pseudohypoparathyroidism"),
                     selected = "", width = "100%"
                   )
                   
    )
  ),
  
  # Treatment selector
  
  tags$li(
    title = "",
    class = "dropdown",
    dropdownButton(icon = icon("download"), width = "300px", circle = FALSE, status = "primary", label = "Treatment",
                   
                   multiInput(
                     inputId = "treatment_selected", label = "Select a treatment :",
                     choices = c("parathyroid surgery","vitamin D3 iv injection","Ca supplementation","Ca iv injection",
                                 "PO4 supplementation","PO4 iv injection","cinacalcet"),
                     selected = "", width = "100%"
                   ),
                   
                   uiOutput("Ca_iv_control"),
                   uiOutput("Ca_food_control"),
                   uiOutput("D3_iv_control"),
                   uiOutput("P_iv_control"),
                   uiOutput("P_food_control")
                   
    )
  ),
  
  # Share the state of the App with other users and reset the application
  
  tags$li(
    title = "",
    class = "dropdown",
    actionButton(inputId = "bookmark", label = "Share", 
                 icon = shiny::icon("link", lib = "glyphicon"), 
                 class="btn btn-primary"),
    actionButton(class="fa fa-trash fa-5x", inputId="resetAll",
                 label=" Reset", class="btn btn-danger")
  )
  
)