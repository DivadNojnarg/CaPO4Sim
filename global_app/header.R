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
  title="Calcium Phosphate Homeostasis in the Rat", titleWidth = 500,
  
  # enable notifications or not since this can be boring
  tags$li(
    title = "",
    class = "dropdown",
    switchInput(inputId = "notif_switch", label = "Help?",
                onStatus = "success", offStatus = "danger", value = FALSE, size = "mini")
  ),
  
  # Make a report
  tags$li(
    title = "",
    class = "dropdown",
    dropdownButton(icon = icon("download"), width = "100px", circle = FALSE, status = "primary", label = "Report",
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport')
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
    actionButton(class="fa fa-floppy-o fa-5x", inputId="save",
                 label=" Save", class="btn btn-primary"),
    actionButton(class="fa fa-refresh fa-5x", inputId="load",
                 label=" Load", class="btn btn-primary"),
    actionButton(class="fa fa-trash fa-5x", inputId="resetAll",
                 label=" Reset", class="btn btn-danger"),
    actionButton(class="fa fa-close fa-5x", inputId="close",
                 label=" Close", class="btn btn-danger")
  ),
  
  # Task Menu
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 60, color = "orange",
                        "Design"
               ),
               taskItem(value = 80, color = "green",
                        "Features"
               )
  ),
  
  tags$li(
    title = "",
    class = "dropdown",
    dropdownButton(
      # Put here a series of shiny widget for example such as theme selector, ...
      shinythemes::themeSelector(),
      circle = FALSE, status = "primary", icon = icon("gear"), width = "100px",
      tooltip = tooltipOptions(title = "Click to see inputs !")
    )
  )
  
)