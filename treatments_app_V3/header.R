#-------------------------------------------------------------------------
#  This code contains the header of shinydashboard. It is modified compared
#  to classic header. Indeed, some buttons to save, load, reset, download are
#  inserted in the header bar. Moreover, users can change the global theme
#  clicking on the theme selector.
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

header <- dashboardHeader(
  title = HTML(
    paste0(
      '<span class = "logo-lg">CaPO4 Teaching Tool</span>',
      '<img src= "online-learning.svg">'
    )
  ),
  
  titleWidth = 300,
  
  tags$li(
    title = "",
    class = "dropdown",
    actionBttn(
      inputId = "run",
      size = "lg",
      label = "Run",
      style = "fill",
      color = "primary",
      icon = icon("play")
    )
    
  ),
  tags$li(
    title = "",
    class = "dropdown",
    actionBttn(
      inputId = "resetAll",
      size = "lg",
      label = " Reset",
      style = "fill",
      color = "danger",
      icon = icon("trash")
    )
  ),
  
  tags$li(
    title = "",
    class = "dropdown",
    actionBttn(
      "help",
      label = "Help",
      icon = NULL,
      style = "fill",
      color = "danger",
      size = "lg",
      block = FALSE,
      no_outline = TRUE
    )
  )
  
  #dropdownMenuOutput("parameter_changed")
  
  # dropdownMenu(
  #   tags$li(
  #     HTML(paste0(
  #       '<section class="contact" id="contact">
  #           <div class="container">
  #               <div class="row center-xs">
  #                 <div class="col-xs-12"><h1>Contact us!</h1></div>
  #               </div>
  #             <form action="https://formspree.io/david.granjon@uzh.ch" method="POST">
  #               <div class="row center-xs">
  #                 <div class="col-xs-12 col-sm-6"><input type="text" name="name" placeholder="Name"></div>
  #                 <div class="col-xs-12 col-sm-6"><input type="email" name="email" placeholder="Email"></div>
  #                 <div class="col-xs-12"><input type="text" name="_subject" placeholder="Subject"></div>
  #                 <div class="col-xs-12"><textarea name="message" placeholder="Message"></textarea></div>
  #                 <input type="text" name="_gotcha" style="display:none">
  #                 <input type="hidden" name="_next" value="/">
  #                 <div class="col-xs-6"><input type="submit" value="Send"></div>
  #               </div>
  #             </form>
  #           </div>
  #         </section>'
  #       ))
  #   )
  # )
)