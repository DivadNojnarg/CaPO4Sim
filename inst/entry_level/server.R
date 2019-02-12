shinyServer(function(input, output, session) {

  # determine whether we are on mobile or not
  # relies on the find-navigator.js file in www/js
  # a simple Shiny.onInputChange
  isMobile <- reactive(input$isMobile)

  # Disable the private section: workaround so that shinyWidgets works
  # in the right sidebar
  observe(shinyjs::hide("prettystuff"))

  # call all modules
  help <- callModule(module = helpCaPO4, id = "help_section")
  callModule(module = video, id = "CaPO4_movies")
  callModule(module = skinSelect, id = "skin")
  callModule(module = glossaryCaPO4, id = "glossary")
  callModule(module = userInfo, id = "rat")
  diseases <- callModule(module = diseaseSelect, id = "diseases")
  networkOptions <- callModule(module = networkOptions, id = "network_options", mobile = isMobile)
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
  callModule(
    module = infos,
    id = "infos",
    diseases = diseases,
    animation_counter = network_utils$counter,
    regulations = networkOptions$regulations
  )
  #callModule(module = plotBox, id = "graphs")

  ## test disease inputs
  #observe(print(diseases$php1()))
  #observe(print(which(lapply(seq_along(diseases), isTRUE) == TRUE)))
  #observe(print(networkOptions$regulations()))

  #-------------------------------------------------------------------------
  #
  #  5) Useful functions: show/hide/reset/...
  #
  #-------------------------------------------------------------------------


  # Need to find a way to integrate the userMenu card to the help section
  #observe({
  #  shinyjs::toggleClass(
  #    id = "user",
  #    class = "user-menu open",
  #    condition = input$help
  #  )
  #})


  # disable the human background
  #observe({
  #  shinyjs::disable(selector = "#background_choice input[value='human']")
  #})

  # Custom footer
  #output$dynamicFooter <- renderFooter({
  #  generate_dynamicFooter()
  #})

})
