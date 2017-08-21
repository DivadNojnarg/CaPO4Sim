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
  title="Calcium Phosphate Homeostasis in the Rat: Free exploration", titleWidth = 700,
  
  # tmax value and other options
  
  tags$li(
    title = "",
    class = "dropdown",
    dropdownButton(icon = icon("gear"), width = "150px", circle = FALSE, status = "danger", label = "Options",
                   
                   numericInput("tmax","Value of tmax:", 500, min = 0, max = NA, width = "50%"),
                   # enable notifications or not since this can be boring
                   switchInput(inputId = "notif_switch", label = "Help?",
                               onStatus = "success", offStatus = "danger", value = FALSE, size = "mini")
                   
    )
  ),
  
  # Share the state of the App with other users
  tags$li(
    title = "",
    class = "dropdown",
    actionButton(inputId = "bookmark", label = "Share", 
                 icon = shiny::icon("link", lib = "glyphicon"), 
                 class="btn btn-primary")
    ),
  
  # Menu to save, load, reset and close the application
  tags$li(
    title = "",
    class = "dropdown",
    actionButton(class="fa fa-trash fa-5x", inputId="resetAll",
                 label=" Reset", class="btn btn-danger")
  ),
  
  # Task Menu
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 60, color = "orange",
                        "Design"
               ),
               taskItem(value = 80, color = "green",
                        "Features"
               )
  )
  
)