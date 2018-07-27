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
library(shinyjs)
library(shinycssloaders)
library(shinyjqui)
library(bsplus)
library(purrr)
library(stringr)
library(shinyFeedback)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)

# Load the template components of UI
source("patient_generator.R")
source("patient_selector.R")
source("getting_started.R")
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
source("dashboardControlbar.R")
source("cap_fixed_parameters.R")
source("calcium_phosphate_core.R") # core model
source("calc_change.R")
source("networks.R")
source("model_utils.R")
source("generate_slider_events.R")
source("generate_dynamicFooter.R")

# set the current time zone to Zurich (for shiny server)
Sys.setenv(TZ = "Europe/Zurich")

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