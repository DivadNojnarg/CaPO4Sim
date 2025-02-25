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
library(shinyMobile)
library(dplyr)
library(V8)

library(CaPO4Sim)

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

f7Dialog <- function(
  id = NULL,
  title = NULL,
  text,
  type = c("alert", "confirm", "prompt", "login"),
  session = shiny::getDefaultReactiveDomain()
) {
  type <- match.arg(type)
  if (is.null(id) && type %in% c("confirm", "prompt", "login")) {
    stop("Missing id.")
  }
  text <- if (any(class(text) %in% c("shiny.tag", "shiny.tag.list"))) {
    as.character(force(text))
  } else {
    text
  }
  message <- dropNulls(list(id = id, title = title, text = text, type = type))
  session$sendCustomMessage(
    type = "dialog",
    message = jsonlite::toJSON(message, auto_unbox = TRUE, json_verbatim = TRUE)
  )
}

# Load the template components of UI
source("patient_selector.R")
source("getting_started.R")
source("generate_questions.R")
source("footer.R")
source("body.R")
source("rightPanel.R")
source("navbar.R")

#-------------------------------------------------------------------------
#
#
#  Load server elements and useful functions
#
#
#-------------------------------------------------------------------------

# Load usefull scripts
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

addResourcePath("www", "www")
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
