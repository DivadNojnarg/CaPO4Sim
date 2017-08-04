#-------------------------------------------------------------------------
#  This UI code contains the global UI of the application. It calls
#  header, body and sidebar (which is NULL in this case) and load all 
#  javascript libraries such as shinyJS, extendShinyjs, MathJax... as well
#  as the theme by default which is cerulean (can be changed with theme selector)
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(flexdashboard)
require(visNetwork)
library(shinyWidgets)
library(shinyjqui)
library(bsplus)
library(sweetalertR)
library(shinyLP)
#library(timevis)

# Load the template components

source("header.R")
source("sidebar.R")
source("body.R")
source("helpers.R")

# JS code for closing shiny app with a button

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Define UI 
shinyUI(fluidPage(

  # 4 danger notification that can be switched on or off as required
  tags$style("#shiny-notification-menu_notif {position: fixed; top: 1%; right: 50% ; width: 40em; opacity: 1;}"),
  tags$style("#shiny-notification-graph_notif {position: fixed; top: 30%; right: 25% ; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-control_notif {position: fixed; top: 70%; right: 15% ; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-diagram_notif {position: fixed; top: 20%; left: 15% ; width: 20em; opacity: 1;}"),
  
  # notifications for php1
  tags$style("#shiny-notification-php11_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"), 
  tags$style("#shiny-notification-php12_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-php13_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-php14_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-php15_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  
  tags$style("#shiny-notification-php1_xaxis_notif {position: fixed; top: 46%; right: 17%; width: 20em; opacity: 1;}"),
  
  # notifivations for hypopara
  tags$style("#shiny-notification-hypopara1_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-hypopara2_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-hypopara3_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-hypopara4_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  
  tags$style("#shiny-notification-hypopara_xaxis_notif {position: fixed; top: 46%; right: 17%; width: 20em; opacity: 1;}"),
  
  # notifications for hypoD3
  tags$style("#shiny-notification-hypoD31_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-hypoD32_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-hypoD33_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-hypoD34_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-hypoD35_notif {position: fixed; top: 75%; left: 10%; width: 20em; opacity: 1;}"),
  
  
  tags$style("#shiny-notification-hypoD3_xaxis_notif {position: fixed; top: 46%; right: 17%; width: 20em; opacity: 1;}"),
  
  #
  tags$style("#shiny-notification-tmax_notif {position: fixed; top: 77%; right: 5%; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-backnext_notif {position: fixed; top: 90%; left: 15%; width: 20em; opacity: 1;}"),
  
  #tags$style("#boxdiagram {background-image: url('background_main.png');}"), # insert a background image for the box
  
  useShinyjs(), # perform some javascript events such as show/hide ...
  includeJqueryUI(), # to move graphs and resize them
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  withMathJax(), # write mathematics
  
  # Application theme
  theme = shinytheme("yeti"),
  
  # include a dashboard
  dashboardPage(skin = "black", header, sidebar, body)
  
))