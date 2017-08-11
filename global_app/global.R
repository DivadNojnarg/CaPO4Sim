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

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Load server elements and usefull functions

source("cap_fixed_parameters.R")
source("calcium_phosphate_core.R") # core model
source("calc_change.R")
source("box_close.R")
#source("chatBox_tools.R")

# Bookmarking

enableBookmarking(store = "url")

#- create_dynamicChatMessage()
#allChatMessage <- list() # keep hist of all chat message