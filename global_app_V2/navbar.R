source("help.R")
header_box_network <- introBox(
  fluidPage(
    smNavBar("testMenu", "", full.width = TRUE, fixed = FALSE, 
             # enable notifications or not since this can be boring
             actionBttn(inputId = "help", 
                        label = "Help?",
                        color = "danger", 
                        size = "lg",
                        style = "simple"),
             # show a demonstration
             actionBttn(inputId = "ShowDemo", 
                        label = "Demo",
                        color = "warning", 
                        size = "lg",
                        style = "simple",
                        icon = icon("youtube-play")),
             # select the network
             awesomeCheckboxGroup(inputId = "network_Ca_choice", 
                                  label = "Choose your network", 
                                  choices = c("Ca","PO4"), 
                                  selected = "Ca", 
                                  inline = TRUE, 
                                  status = "primary"),
             # selector for hormonal regulation
             switchInput(inputId = "network_hormonal_choice", 
                         label = "Show regulations", 
                         value = FALSE,
                         onStatus = "success",
                         offStatus = "danger",
                         size = "mini"),
             # show background or not 
             switchInput(inputId = "background_switch", 
                         label = "Show background", 
                         value = TRUE,
                         onStatus = "success",
                         offStatus = "danger",
                         size = "mini"),
             # numeric options
             smNavDropdown("Integration",
                           # maximum time of integration
                           numericInput("tmax",
                                        "Maximum simulated time:", 
                                        value = 500, 
                                        min = 0, 
                                        max = NA,
                                        width = "100%"),
                           smDivider(),
                           # navigate to a given time
                           sliderInput("t_now",
                                       "Time after simulation:", 
                                       value = 1, 
                                       min = 1, 
                                       max = 500,
                                       width = "90%") %>%
                             shinyInput_label_embed(
                               icon("undo") %>%
                                 actionBttn(inputId = "reset_t_now",
                                            label = "", 
                                            color = "danger", 
                                            size = "xs"))
             )
    )
  ),
  data.step = 6,
  data.intro = help_text[6]
)