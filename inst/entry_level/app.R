# 1) load all packages
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

library(shinydashboard)
library(shinydashboardPlus)
library(rintrojs)
library(magrittr)
library(DT)

library(CardioRenalSim)

# 2) Load the template components
source("help.R")
source("header.R")
source("sidebar.R")
source("body.R")
source("dashboardControlbar.R")
source("footer.R")

# 3) Load server components and functions
source("notifications.R")
source("graph_notifications.R")
source("animations.R")
source("modals.R")
source("network_modals.R")


# 4) App
shinyApp(
  ui = dashboardPagePlus(
    skin = "black",
    title = "Cardio-Renal Teaching App",
    collapse_sidebar = TRUE,
    enable_preloader = TRUE,
    header,
    sidebar,
    body,
    footer,
    rightsidebar
  ),
  server = function(input, output, session) {

    # determine whether we are on mobile or not
    # relies on the find-navigator.js file in www/js
    # a simple Shiny.onInputChange
    isMobile <- reactive(input$isMobile)

    # Disable the private section: workaround so that shinyWidgets works
    # in the right sidebar
    observe(shinyjs::hide("prettystuff"))

    # some useful modules
    help <- callModule(module = helpCaPO4, id = "help_section")
    callModule(module = video, id = "CaPO4_movies")
    callModule(module = fullScreen, id = "fullScreenTrigger")
    callModule(module = skinSelect, id = "skin")
    callModule(module = glossaryCaPO4, id = "lexicus")
    diseases <- callModule(module = diseaseSelect, id = "diseases")
    networkOptions <- callModule(module = networkOptions, id = "network_options", mobile = isMobile)

    # network module
    network_utils <- callModule(
      module = networkCaPO4,
      id = "network",
      isMobile = isMobile,
      components = networkOptions$components,
      organs = networkOptions$organs,
      regulations = networkOptions$regulations,
      background = networkOptions$background,
      diseases = diseases,
      organs_nodes_size = networkOptions$organs_nodes_size,
      hormones_nodes_size = networkOptions$hormones_nodes_size,
      organs_edges_size = networkOptions$organs_edges_size,
      hormones_edges_size = networkOptions$hormones_edges_size,
      help = help
    )

    # modals module
    callModule(
      module = infos,
      id = "infos",
      diseases = diseases,
      animation_counter = network_utils$counter,
      regulations = networkOptions$regulations
    )

    # plot module
    slider_disease <- callModule(module = plotBox, id = "graphs", diseases = diseases, help = help, isMobile = isMobile)

    # userInfo module
    callModule(module = userInfo, id = "rat", diseases = diseases, sliderDisease = slider_disease, help = help)
  }
)
