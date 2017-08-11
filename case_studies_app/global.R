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

# Load the template components

source("header.R")
source("sidebar.R")
source("body.R")

# JS code for closing shiny app with a button

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Load server components and functions

source("calcium_phosphate_Caiv.R")
source("calcium_phosphate_PO4iv.R")
source("calcium_phosphate_PO4gav.R")
source("cap_fixed_parameters.R")
source("calc_change.R")

path_to_images <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/www"
#path_to_images <- "/srv/shiny-server/capApp/case_studies_app/www"


state <- c("PTH_g" = 1288.19, "PTH_p" = 0.0687, "D3_p" = 564.2664, "FGF_p" = 16.78112, "Ca_p" = 1.2061,
           "Ca_f" = 1.8363, "Ca_b" = 250, "PO4_p" = 1.4784, "PO4_f" = 0.7922, "PO4_b" = 90, "PO4_c" = 2.7719,
           "CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, "CaProt_p" = 1.4518,
           "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0)

# Bookmarking

enableBookmarking(store = "url")