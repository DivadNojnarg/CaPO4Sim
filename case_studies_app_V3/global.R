# *------------------------------------------------------------------
# | PROGRAM NAME: global.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This codes loads all packages needed by the application             
# |           Moreover, it contains all mandatory UI and server elements
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  packages
# |  PART 2:  UI elements
# |  PART 3: server components
# |  PART 4:  server datas (state vector)
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

# 1) load all packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(visNetwork)
library(shinyBS)
library(dplyr)
library(plotly)
library(deSolve)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjqui)
library(bsplus)
library(sweetalertR)
library(shinytoastr)
library(stringr)
library(purrr)
library(rintrojs)
library(shinyFeedback)
library(magrittr)
library(DT)

# 2) Load the template components
source("help.R")
source("app_css.R")
source("utils.R")
source("dashboardUser.R")
source("header.R")
source("sidebar.R")
source("network_box.R")
source("graph_box.R")
source("body.R")

# 3) Load server components and functions
source("model_utils.R")
source("dashboardControlbar.R")
source("calcium_phosphate_Caiv.R")
source("calcium_phosphate_PO4iv.R")
source("calcium_phosphate_PO4gav.R")
source("cap_fixed_parameters.R")
source("calc_change.R")
source("notifications.R")
source("graph_notifications.R")
source("network.R")
source("getting_started.R")
source("animations.R")
source("modals.R")
source("generate_network_knobs.R")
source("generate_userInfo.R")
source("generate_glossary.R")
source("generate_dynamicFooter.R")
# all plot requires generate_slidersteady and extract_running_sim
# to run properly. Therefore, it is loaded in the end
source("all_plots.R")

# 4) load the initial state of the model. Only needed for dynamic
#    simulation that is, Ca/EGTA injection, PO4 injection and
#    PO4 gavage
state <- c(
  "PTH_g" = 1288.19, 
  "PTH_p" = 0.0687, 
  "D3_p" = 564.2664, 
  "FGF_p" = 16.78112, 
  "Ca_p" = 1.2061,
  "Ca_f" = 1.8363, 
  "Ca_b" = 250, 
  "PO4_p" = 1.4784, 
  "PO4_f" = 0.7922, 
  "PO4_b" = 90, 
  "PO4_c" = 2.7719,
  "CaHPO4_p" = 0.1059, 
  "CaH2PO4_p" = 0.0038, 
  "CPP_p" = 0.0109, 
  "CaHPO4_f" = 0.0864, 
  "CaH2PO4_f" = 0.0031, 
  "CaProt_p" = 1.4518,
  "NaPO4_p" = 0.9135, 
  "Ca_tot" = 2.4914, 
  "PO4_tot" = 2.8354, 
  "EGTA_p" = 0, 
  "CaEGTA_p" = 0
)
