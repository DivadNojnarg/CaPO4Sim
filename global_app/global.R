## global.R ##

#loading packages
library(shiny)
library(plotly)
library(deSolve)
require(visNetwork)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjqui)
library(bsplus)
library(sweetalertR)

# Load the template components of UI

source("header.R")
source("sidebar.R")
source("body.R")

# JS code for closing shiny app with a button

#jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Load server elements and usefull functions

source("cap_fixed_parameters.R")
source("calcium_phosphate_core.R") # core model
source("calc_change.R")
source("box_close.R")

state <- c("PTH_g" = 1288.19, "PTH_p" = 0.0687, "D3_p" = 564.2664, 
           "FGF_p" = 16.78112, "Ca_p" = 1.2061, # initial conditions
           "Ca_f" = 1.8363, "Ca_b" = 250, "PO4_p" = 1.4784, 
           "PO4_f" = 0.7922, "PO4_b" = 90, "PO4_c" = 2.7719,
           "CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, "CPP_p" = 0.0109, 
           "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, "CaProt_p" = 1.4518, 
           "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, "PO4_tot" = 2.8354, 
           "EGTA_p" = 0, "CaEGTA_p" = 0)

#source("chatBox_tools.R")

# Bookmarking

enableBookmarking(store = "url")

#- create_dynamicChatMessage()
#allChatMessage <- list() # keep hist of all chat message