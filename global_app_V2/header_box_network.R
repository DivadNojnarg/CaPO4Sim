
# tmax value and other options
source("help.R")

header_box_network <- div(
  
  id = "options",
  tagList(
    
    dropdownButton(icon = icon("gear"), 
                   width = "150px", 
                   circle = FALSE, 
                   status = "danger",
                   up = TRUE,
                   label = "",
                   
                   br(),
                   
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
                                 label = "Show regulations?", 
                                 value = FALSE,
                                 onStatus = "success",
                                 offStatus = "danger",
                                 size = "mini"),
                     data.step = 7,
                     data.intro = help_text[7]
                   ),

                   br(),
                   
                   # selector for background display
                   
                   materialSwitch(inputId = "background_switch", 
                                  label = "Show background", value = TRUE),
                   
                   br(),
                   
                   # tmax: maximum time of integration
                   introBox(
                     numericInput("tmax",
                                  "Maximum simulated time:", 
                                  value = 500, 
                                  min = 0, 
                                  max = NA),
                     data.step = 8,
                     data.intro = help_text[8]
                   ),
                   
                   br(),
                   
                   # current time of interest for arrow lighting
                   introBox(
                     sliderInput("t_now",
                                  "Time after simulation:", 
                                  value = 1, 
                                  min = 1, 
                                  max = 500)%>%
                       shinyInput_label_embed(
                         icon("undo") %>%
                           actionBttn(inputId="reset_t_now",
                                      label="", 
                                      color="danger", 
                                      size = "xs")),
                     data.step = 9,
                     data.intro = help_text[9]
                   )
                   
    )
  )
)