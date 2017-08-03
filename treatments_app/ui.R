#-------------------------------------------------------------------------
#  This UI code contains the global UI of the application. It calls
#  header, body and sidebar (which is NULL in this case) and load all 
#  javascript libraries such as shinyJS, extendShinyjs, MathJax... as well
#  as the theme by default which is cerulean (can be changed with theme selector)
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#
#  bsplus only works with R > 3.3, so pay attention to update R before installing
#  other packages. On shiny-server, always install R packages by running R in the
#  shiny folder. Put the app in src/shiny-server/myApp and access via: 
#  server_ip:3838/myApp
#
#-------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
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

# Load the template components

source("header.R")
source("sidebar.R")
source("body.R")
source("helpers.R")

# JS code for closing shiny app with a button

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Define UI 
shinyUI(fluidPage(
  
  # tags$head(
  #   tags$style(HTML("
  #                   #network_cap{
  #                     background-image:url('bone.png');
  #                   }
  # 
  #                   "))
  # ),
  
  # tags$head(tags$script(src="ng-knob.js")),
  # tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular.min.js")),
  # tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.10/d3.min.js")),
  
  includeJqueryUI(), # to move graphs and resize them
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  withMathJax(), 
  
  # Application theme
  theme = shinytheme("yeti"),
  #shinythemes::themeSelector(),
  #theme = "bootswatch-journal.css",
  
  # include a dashboard
  dashboardPage(skin = "black", header, sidebar, body)
  
))