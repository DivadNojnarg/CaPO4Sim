## global.R ##

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

# Load the template components

source("header.R")
source("sidebar.R")
source("body.R")

# JS code for closing shiny app with a button

jscode <- "shinyjs.closeWindow = function() { window.close(); }"


# Load the relevant code for server part 

source("cap_fixed_parameters.R")
source("calcium_phosphate_core.R") # core model
source("calc_change.R")
source("box_close.R")

# Load state values based on files previously created for each case (php1, hypopara, hypoD3)
state_php1 <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/treatments_app/init_php1.csv", # for local config
                       stringsAsFactors = FALSE)
#state_php1 <- read.csv("/srv/shiny-server/capApp/treatments_app/init_php1.csv", # use in the server
#stringsAsFactors = FALSE)
state_php1 <- unlist(state_php1[,-1]) # need unlist to convert the dataframe in vector as required for the state variable

state_hypopara <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/treatments_app/init_hypopara.csv", # for local config
                           stringsAsFactors = FALSE)
#state_hypopara <- read.csv("/srv/shiny-server/capApp/treatments_app/init_hypopara.csv", # use in the server
#stringsAsFactors = FALSE)
state_hypopara <- unlist(state_hypopara[,-1]) # need unlist to convert the dataframe in vector as required for the state variable


state_hypoD3 <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/treatments_app/init_hypoD3.csv", # for local config
                         stringsAsFactors = FALSE)
#state_hypoD3 <- read.csv("/srv/shiny-server/capApp/treatments_app/init_hypoD3.csv", # use in the server
#stringsAsFactors = FALSE)
state_hypoD3 <- unlist(state_hypoD3[,-1]) # need unlist to convert the dataframe in vector as required for the state variable



# enable bookmarking in url

enableBookmarking(store = "url")

