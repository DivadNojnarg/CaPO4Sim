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
source("generate_patient_info.R")
source("app_css.R")
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
source("generate_slider_events.R")
source("generate_dynamicFooter.R")

# Load state values based on files previously created for each case (php1, hypopara, hypoD3)
state_php1 <- read.csv(paste0(getwd(), "/www/init_php1.csv"),
                       stringsAsFactors = FALSE)
# need unlist to convert the dataframe in vector as required for the state variable
state_php1 <- unlist(state_php1[,-1]) 

state_hypopara <- read.csv(paste0(getwd(),"/www/init_hypopara.csv"), 
                           stringsAsFactors = FALSE)
# need unlist to convert the dataframe in vector as required for the state variable
state_hypopara <- unlist(state_hypopara[,-1]) 


state_hypoD3 <- read.csv(paste0(getwd(),"/www/init_hypoD3.csv"), 
                         stringsAsFactors = FALSE)
# need unlist to convert the dataframe in vector as required for the state variable
state_hypoD3 <- unlist(state_hypoD3[,-1]) 



# below is needed to handle disease and treatments events
disease_choices <- c(
  "primary-hyperparathyroidism", 
  "hypoparathyroidism", 
  "vitamin D3 deficiency"
)
treatment_choices <- c(
  "parathyroid surgery",
  "D3_inject",
  "Ca_food",
  "Ca_inject",
  "P_food",
  "P_inject",
  "cinacalcet"
)

t_start <- NULL
t_stop <- NULL


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