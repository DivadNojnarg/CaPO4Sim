
# tmax value and other options

header_box_network <- div(
  
  id = "options",
  tagList(
    
    dropdownButton(icon = icon("gear"), 
                   width = "150px", 
                   circle = FALSE, 
                   status = "danger",
                   up = TRUE,
                   label = "",
                   
                   # enable notifications or not since this can be boring
                   actionBttn(inputId = "notif_switch", 
                              label = "Help?",
                              color = "danger", 
                              size = "xs"),
                   
                   br(),
                   
                   # selector for Ca an/or PO4 homeostasis
                   introBox(
                     awesomeCheckboxGroup(inputId = "network_Ca_choice", 
                                          label = "Choose your network", 
                                          choices = c("Ca","PO4"), 
                                          selected = c("Ca","PO4"), 
                                          inline = TRUE, 
                                          status = "primary"),
                     data.step = 6,
                     data.intro = help_text[6]
                   ),
                   
                   br(),
                   
                   # selector for hormonal regulation
                   introBox(
                     switchInput(inputId = "network_hormonal_choice", 
                                 label = "Hide hormones?", 
                                 value = FALSE,
                                 onStatus = "success",
                                 offStatus = "danger",
                                 size = "mini"),
                     data.step = 7,
                     data.intro = help_text[7]
                   ),

                   br(),
                   
                   # tmax: maximum time of integration
                   introBox(
                     numericInput("tmax",
                                  "Value of tmax:", 
                                  value = 500, 
                                  min = 0, 
                                  max = NA),
                     data.step = 8,
                     data.intro = help_text[8]
                   )
                   
    )
  )
)