## global.R ##

library(shiny)
require(visNetwork)
library(shinyBS)
library(dplyr)
library(plotly)
library(deSolve)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjqui)
library(bsplus)
library(sweetalertR)
library(shinytoastr)
library(shinyMenus)
library(stringr)
library(purrr)
library(rintrojs)

# Load the template components

source("header.R")
source("sidebar.R")
source("body.R")

# JS code for closing shiny app with a button

#jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Load server components and functions

source("calcium_phosphate_Caiv.R")
source("calcium_phosphate_PO4iv.R")
source("calcium_phosphate_PO4gav.R")
source("cap_fixed_parameters.R")
source("calc_change.R")
source("notifications.R")
source("help.R")
source("navbar.R")
source("network.R")

path_to_images <- "/Users/macdavidgranjon/Documents/WebApp_CaP_homeostasis/case_studies_app/www"
#path_to_images <- "/srv/shiny-server/capApp/case_studies_app/www"


state <- c("PTH_g" = 1288.19, "PTH_p" = 0.0687, "D3_p" = 564.2664, 
           "FGF_p" = 16.78112, "Ca_p" = 1.2061, # initial conditions
           "Ca_f" = 1.8363, "Ca_b" = 250, "PO4_p" = 1.4784, 
           "PO4_f" = 0.7922, "PO4_b" = 90, "PO4_c" = 2.7719,
           "CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, 
           "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, 
           "CaH2PO4_f" = 0.0031, "CaProt_p" = 1.4518,
           "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, 
           "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0)


# Notification function
# Takes counter_nav diagram as well as the simulation event as arguments
# and also the switch to allow notifications or not.
generate_notification <- function(simulation, counter, allowed) {
  idx <- counter
  # only take the part after the "_"
  message <- str_split(simulation, pattern = "_")[[1]][2]
  # print only if notifications are allowed
  if (allowed == TRUE) {
  showNotification(id = "notifid",
                  # need to eval and deparse so as to paste message
                  eval(parse(text = paste("notification_list$", message, "[idx+1]", sep = ""))),
                  type = "message",
                  duration = 9999)
    
  # toastr is interesting but need to be improved!  
  # toastr_info(message = eval(parse(text = paste("notification_list$", message, "[idx+1]", sep = ""))),
  #             title = "",
  #             closeButton = TRUE,
  #             preventDuplicates = TRUE,
  #             position = "top-left",
  #             timeOut = 0,
  #             showMethod = "fadeIn",
  #             hideMethod = "fadeOut"
  # )  
    
  } else {
   removeNotification(id = "notifid")
  }
}


# Extract the current runing simulation among php1, hypoD3, hypopara
# Ca_inject, PO4_inject and PO4 gavage. Takes input as argument
# returns the names of the current simulation (string).
extract_running_sim <- function(input) {
  # extract all simulations
  sim <- str_extract(names(input), pattern = "^run_\\w+")
  # remove NAs
  sim <- sim[!is.na(sim)]
  # converting each string to the corresponding object
  sim_obj <- lapply(sim, function(x) {eval(parse(text = paste("input$", x)))})
  # which simulation is set to true? (php1, hypopara, ...)
  sim_idx <- which(sim_obj == TRUE)
  current_simulation <- sim[sim_idx]
  return(list(current_simulation, sim))
}



