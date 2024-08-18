#-------------------------------------------------------------------------
#  This codes loads all packages needed by the application.
#  Moreover, it contains all mandatory UI and server elements.
#
#-------------------------------------------------------------------------

# load packages
library(shiny)
library(plotly)
library(deSolve)
library(visNetwork)
library(shinyjs)
library(shinycssloaders)
library(shinyjqui)
library(bsplus)
library(purrr)
library(shinyWidgets)
library(shinyEffects)

library(stringr)
library(shinyFeedback)
library(bs4Dash)
library(dplyr)
library(waiter)

library(CaPO4Sim)

# Load the template components of UI
source("patient_selector.R")
source("getting_started.R")
source("generate_questions.R")
source("navbar.R")
source("sidebar.R")
source("body.R")
source("footer.R")

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

# set the current time zone to Zurich (for shiny server)
Sys.setenv(TZ = "Europe/Zurich")

# compile the C code containing equations
so_name <- paste("compiled_core", .Platform$dynlib.ext, sep = "")
system("R CMD SHLIB compiled_core.c")
dyn.load(so_name)


#-------------------------------------------------------------------------
#
#
#  Other elements: bookmarking, config,...
#
#
#-------------------------------------------------------------------------
users_logs <- "www/users_data"
if (!dir.exists(users_logs)) {
  dir.create(users_logs)
}

onStop(function() {
  if (.Platform$OS.type == "unix") {
    file.remove(so_name)
    file.remove(gsub("so", "o", so_name))
  } else if (.Platform$OS.type == "windows") {
   file.remove(so_name)
  }
})

# Bookmarking
#enableBookmarking(store = "server") # save to the disk
