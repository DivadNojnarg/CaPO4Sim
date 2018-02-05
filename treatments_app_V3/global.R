#------------------------------------------------------------------------- 
#  This codes loads all packages needed by the application.
#  Moreover, it contains all mandatory UI and server elements. 
#
#-------------------------------------------------------------------------

# load packages
library(shiny)
library(plotly)
library(deSolve)
require(visNetwork)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(shinyjqui)
library(bsplus)
library(sweetalertR)
library(rintrojs)
library(purrr)
library(stringr)
library(shinyFeedback)
library(shinyWidgets)
library(ygdashboard)

# Load the template components of UI
source("header.R")
source("sidebar.R")
source("body.R")

#------------------------------------------------------------------------- 
#  
#  
#  Load server elements and useful functions
#  
#
#-------------------------------------------------------------------------

# Load usefull scripts
source("utils.R")
source("dashboardControlbar.R")
source("cap_fixed_parameters.R")
source("calcium_phosphate_core.R") # core model
source("calc_change.R")
source("networks.R")
source("model_utils.R")

# Load state values based on files previously created for each case (php1, hypopara, hypoD3)
state_php1 <- read.csv("/Users/macdavidgranjon/Documents/WebApp_CaP_homeostasis/treatments_app_V3/init_php1.csv", # for local config
                       stringsAsFactors = FALSE)
# state_php1 <- read.csv("/srv/shiny-server/capApp/treatments_app/init_php1.csv", # use in the server
#                        stringsAsFactors = FALSE)
state_php1 <- unlist(state_php1[,-1]) # need unlist to convert the dataframe in vector as required for the state variable

state_hypopara <- read.csv("/Users/macdavidgranjon/Documents/WebApp_CaP_homeostasis/treatments_app_V3/init_hypopara.csv", # for local config
                           stringsAsFactors = FALSE)
# state_hypopara <- read.csv("/srv/shiny-server/capApp/treatments_app/init_hypopara.csv", # use in the server
#                            stringsAsFactors = FALSE)
state_hypopara <- unlist(state_hypopara[,-1]) # need unlist to convert the dataframe in vector as required for the state variable


state_hypoD3 <- read.csv("/Users/macdavidgranjon/Documents/WebApp_CaP_homeostasis/treatments_app_V3/init_hypoD3.csv", # for local config
                         stringsAsFactors = FALSE)
# state_hypoD3 <- read.csv("/srv/shiny-server/capApp/treatments_app/init_hypoD3.csv", # use in the server
#                          stringsAsFactors = FALSE)
state_hypoD3 <- unlist(state_hypoD3[,-1]) # need unlist to convert the dataframe in vector as required for the state variable


# Time extractor function for event handling
t_start_Cainject <- NULL
t_stop_Cainject <- NULL
t_start_Caintake <- NULL
t_stop_Caintake <- NULL
t_start_D3inject <- NULL
t_stop_D3inject <- NULL
t_start_Pinject <- NULL
t_stop_Pinject <- NULL
t_start_Pintake <- NULL
t_stop_Pintake <- NULL

# to recover to_start and t_stop time when reading the event_table
time_extractor <- function(event_table) { 
  for (i in 1:nrow(event_table$df)) {
    if (is.element("Ca_iv", event_table$df[i,"event"])) {
      t_start_Cainject <- c(t_start_Cainject, event_table$df[i,"start_time"])
      t_stop_Cainject <- c(t_stop_Cainject, event_table$df[i,"stop_time"])
    } else if (is.element("Ca_gavage", event_table$df[i,"event"])) {
      t_start_Caintake <- c(t_start_Caintake, event_table$df[i,"start_time"])
      t_stop_Caintake <- c(t_stop_Caintake, event_table$df[i,"stop_time"])
    } else if (is.element("D3_iv", event_table$df[i,"event"])) {
      t_start_D3inject <- c(t_start_D3inject, event_table$df[i,"start_time"])
      t_stop_D3inject <- c(t_stop_D3inject, event_table$df[i,"stop_time"])
    } else if (is.element("P_iv", event_table$df[i,"event"])) {
      t_start_Pinject <- c(t_start_Pinject, event_table$df[i,"start_time"])
      t_stop_Pinject <- c(t_stop_Pinject, event_table$df[i,"stop_time"])
    } else if (is.element("P_gavage", event_table$df[i,"event"])) {
      t_start_Pintake <- c(t_start_Pintake, event_table$df[i,"start_time"])
      t_stop_Pintake <- c(t_stop_Pintake, event_table$df[i,"stop_time"])
    }
  }
  # return what is really important
  return(list("t_start_Cainject" = t_start_Cainject, "t_stop_Cainject" = t_stop_Cainject, 
              "t_start_Caintake" = t_start_Caintake, "t_stop_Caintake" = t_stop_Caintake,
              "t_start_D3inject" = t_start_D3inject, "t_stop_D3inject" = t_stop_D3inject,
              "t_start_Pinject" = t_start_Pinject, "t_stop_Pinject" = t_stop_Pinject,
              "t_start_Pintake" = t_start_Pintake, "t_stop_Pintake" = t_stop_Pintake))
}

# compile the C code containing equations
#system("R CMD SHLIB compiled_core.c")
#dyn.load(paste("compiled_core", .Platform$dynlib.ext, sep = ""))

#------------------------------------------------------------------------- 
#  
#  
#  Other elements: bookmarking, config,...
#  
#
#-------------------------------------------------------------------------

# Bookmarking
#enableBookmarking(store = "server") # save to the disk