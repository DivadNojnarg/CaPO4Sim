#------------------------------------------------------------------------- 
#  This codes loads all packages needed by the application.
#  Moreover, it contains all mandatory UI and server elements. 
#  Some useful functions are defined here so as to lighten
#  the server code
#
#-------------------------------------------------------------------------

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
source("animations.R")
source("all_plots.R")

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



# Lighting events for php1, hypopara and hypoD3
# As fluxes are not calculated by the model
# we have to make something different of what
# was done in the global app
# 


# extract the proper table of animations
extract_animation <- function(input) {
  # returns php1, hypopara, hypoD3
  current_sim <- extract_running_sim(input)[[1]]
  current_animation <- paste("animation", str_extract(current_sim, 
                                               "_\\w+"), sep = "")
  return(current_animation)
}


# highlight arrows for steady state events
arrow_lighting <- function(edges, simulation, counter, input, session) {
  # store the current animation
  current_anim <- eval(parse(text = extract_animation(input)))
  
  # if the counter is 1 or higher
  if (counter > 0) {
    # selected arrows
    sel <- unlist(current_anim[[counter]])
    #set new edge properties
    if (counter != 6) edges$color.color[sel] <- "yellow" # perturbation
    
    # edge size might depend on the event
    if (simulation == "run_php1") {
      ifelse(sum(is.element(c(1:4, 6), counter)) == 1,
             edges$width[sel] <- 8,
             edges$width[sel] <- 3)
      if (counter == 6) {
        edges$color.color[sel] <- c(rep("red", 4), rep("green", 7))
      }
    } else if (simulation == "run_hypopara") {
      ifelse(sum(is.element(c(1:4), counter)) == 1,
             edges$width[sel] <- 3,
             edges$width[sel] <- 8)
      if (counter == 6) {
        edges$color.color[sel] <- c(rep("green", 4), rep("red", 7))
      }
    } else if (simulation == "run_hypoD3") {
      ifelse(sum(is.element(c(1, 3, 4, 5), counter)) == 1,
             edges$width[sel] <- 3,
             edges$width[sel] <- 8)
      if (counter == 6) {
        edges$color.color[sel] <- "red"
      }
    }
    
  } else {
    # no selection when the counter equals 0
    sel <- NULL
  }
  
  # update the network
  visNetworkProxy("network_Ca", session) %>%
    visUpdateEdges(edges = edges)
}



# highlitght arrows for dynamic events
arrow_lighting_live <- function(out, edges, session) {
  calc_change_t <- calc_change(out)
  calc_change_t$X <- NULL # remove column X
  
  # calculate the difference between live fluxes and base-case values
  index <- c(1,10,11,6,4,5,8,9,2,3,12) # index of arrows in the graph (which are fluxes and not regulations)
  calc_change_t <- rbind(calc_change_t, index)
  
  flux_changed_index <- which(calc_change_t[1,] != 0) # calculate which element in the sum table is different of 0 and store the index
  arrow_index <- as.numeric(t(calc_change_t[2,flux_changed_index])) # convert to arrow index in the interactive diagramm
  
  if (!is.null(flux_changed_index)) {
    for (i in (1:ncol(calc_change_t))) {
      arrow_index_i <- arrow_index[i] # change edge color according to an increase or decrease of the flux
      ifelse(calc_change_t[[i]][1] > 0,
             edges$color.color[arrow_index_i] <- "green",
             edges$color.color[arrow_index_i] <- "red")
    }
    
  }
  
  # need to add elements related to PO4 and Ca events
  # if (input$run_PO4_gav) { # PO4 gavage
  # 
  #   edges_Ca$color.color[1] <- "yellow" # perturbation
  #   edges_Ca$width[1] <- 8
  # 
  # }
  # if (input$run_Ca_inject) {
  #   if (input$tmax < 60) { # Ca infusion
  # 
  #     edges_Ca$color.color[c(20,24,25,33)] <- "yellow" # perturbation
  #     edges_Ca$width[c(20,24,25,33)] <- 8
  # 
  #   } else {# EGTA infusion
  # 
  #     edges_Ca$color.color[c(20,24,25,35)] <- "yellow" # perturbation
  #     edges_Ca$width[c(20,24,25)] <- 2
  #     edges_Ca$width[35] <- 8
  # 
  #   }
  # }
  # if (input$run_PO4_inject) { # PO4 injection
  #   edges_Ca$color.color[c(26,27,28)] <- "yellow" # perturbation
  #   edges_Ca$width[c(26,27,28)] <- 8
  # 
  #   if (input$tmaxbis <= 3) {
  # 
  #     edges_Ca$color.color[34] <- "yellow" # perturbation
  #     edges_Ca$width[34] <- 8
  # 
  #   }
  # 
  # }
  
  visNetworkProxy("network_Ca") %>%
    visUpdateEdges(edges = edges)
}



# Function to reset sliders input to their original values
# Takes a reset_table, network and edges as arguments
# reset_table contains the state of reset button (0 if
# not used) as well as the related sliders_id

sliders_reset <- function(button_states, input) {
  
  # stock the previous state of buttons in
  # reactiveValues so as to compare with
  # the current state
  button_states$values <- append(button_states$values, 
                                 list(c(input$reset_tmaxCainj[1],
                                        input$reset_tmaxPO4inj[1],
                                        input$reset_tmaxPO4gav[1])))         
  
  # associate each reset button to its related slider
  reset_vector <- c("tmaxCainj",
                    "tmaxPO4inj",
                    "tmaxPO4gav")
  
  # store the temp state of buttons
  states <- button_states$values
  last_state <- states[[length(states)]]
  
  # select which reset buttons are selected
  if (length(states) <= 1) {
    # compare the current state with 0
    reset_target <- which(unlist(states) != 0)
  } else {
    # compare the current state with the previous one
    penultimate_state <- states[[length(states) - 1]]
    reset_target <- which(penultimate_state != last_state)
  }
  
  # reset the corresponding target(s) in the table
  shinyjs::reset(reset_vector[reset_target])
  
}