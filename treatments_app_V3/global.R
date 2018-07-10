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
library(shinydashboard)
library(shinydashboardPlus)

# Load the template components of UI
source("patient_generator.R")
source("patient_selector.R")
source("app_css.R")
source("help.R")
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


# load patient files
patient_datas <- patient_selector()

# Load state values based on files previously created for each case (php1, hypopara, hypoD3)
patient_state_0 <- patient_datas$initial_conditions

# patient disease
patient_disease <- patient_datas$disease_id

# initial conditions
state <- c("PTH_g" = 1288.19, "PTH_p" = 0.0687, 
           "D3_p" = 564.2664, "FGF_p" = 16.78112, 
           "Ca_p" = 1.2061,"Ca_f" = 1.8363, "Ca_b" = 250, 
           "PO4_p" = 1.4784, "PO4_f" = 0.7922, "PO4_b" = 90, 
           "PO4_c" = 2.7719,"CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, 
           "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, 
           "CaProt_p" = 1.4518, "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, 
           "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0)

# below is needed to handle treatments events
treatment_choices <- c(
  "PTX",
  "D3_inject",
  "Ca_food",
  "Ca_inject",
  "P_food",
  "P_inject",
  "cinacalcet"
)

# initialization of event parameters
t_start <- NULL
t_stop <- NULL
Ca_inject <- NULL
Ca_food <- NULL
P_inject <- NULL
P_food <- NULL
D3_inject <- NULL


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