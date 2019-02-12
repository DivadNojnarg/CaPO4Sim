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
library(shinyEffects)
library(visNetwork)
library(plotly)
library(deSolve)
library(shinyjs)
library(shinyjqui)
library(shinycssloaders)
library(shinyWidgets)
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
#source("utils.R")
source("header.R")
source("sidebar.R")
source("body.R")

# 3) Load server components and functions
source("dashboardControlbar.R")
source("notifications.R")
source("graph_notifications.R")
source("animations.R")
source("modals.R")
source("generate_dynamicFooter.R")
# all plot requires generate_slidersteady and extract_running_sim
# to run properly. Therefore, it is loaded in the end
source("all_plots.R")
