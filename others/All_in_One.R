# Put here the code of an App where you have a problem so that people
# can run it easily. Do not forget to load all the libraries...


library(shiny)
library(deSolve)
library(rootSolve)
library(phaseR)
library(shinyAce)
library(shinycssloaders)

if (interactive()) {
  options(device.ask.default = FALSE)
  
  app <- shinyApp(
    ui = bootstrapPage(
      
      # write UI code
      
      ),
    
    server = function(input, output, session) {
      
      shinyEnv <- environment()
      
      # write server code
      
    }
    )
  runApp(app)
  }