# *------------------------------------------------------------------
# | PROGRAM NAME: server.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the global UI of the application.
# |           It calls header, body, both sidebars and the footer
# *-----------------------------------------------------------------
# | DATA USED:  state (global.R)
# |             parameters.R
# |
# |
# |*------------------------------------------------------------------
# | CONTENTS:
# |  PART 1: CaPO4 network generation (highlight, blink, nodes, edges, ...)
# |  PART 2: Ploting part
# |  PART 3: Educational content (modals, notifications, glossary, userInfo)
# |  PART 4: Useful tasks
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)
# |
# |
# *------------------------------------------------------------------

shinyServer(function(input, output, session) {

  # determine whether we are on mobile or not
  # relies on the find-navigator.js file in www/js
  # a simple Shiny.onInputChange
  isMobile <- reactive(input$isMobile)

  # Disable the private section: workaround so that shinyWidgets works
  # in the right sidebar
  observe(shinyjs::hide("prettystuff"))

  # call all modules
  #callModule(module = video, id = "CaPO4_movies")
  callModule(module = skinSelect, id = "skin")
  callModule(module = glossaryCaPO4, id = "glossary")
  callModule(module = userInfo, id = "rat")
  diseases <- callModule(module = diseaseSelect, id = "diseases")
  networkOptions <- callModule(module = networkOptions, id = "network_options", mobile = isMobile)
  #network_utils <- callModule(module = networkCaPO4, id = "network_CaPO4")
  #callModule(module = info, id = "infos")
  #callModule(module = plotBox, id = "graphs_CaPO4")

  ## test disease inputs
  #observe(print(diseases$php1()))
  ## test network options
  #observe(print(networkOptions$background()))

  #observe(print(input$current_node_id))
  ## test network utils
  #observe(print(network_utils$counter()))


  #-------------------------------------------------------------------------
  #
  #  5) Useful functions: show/hide/reset/...
  #
  #-------------------------------------------------------------------------



  # help animation with introjs
  # options are provided to control the size of the help
  # Do not forget to wrap the event content in I('my_function')
  # otherwise it will fail
  observeEvent(input$help,{
    introjs(
      session,
      options = list(
        "nextLabel" = "Next step!",
        "prevLabel" = "Did you forget something?",
        "tooltipClass" = "newClass",
        #"highlightClass" = "newClass",
        "showProgress" = TRUE,
        "showBullets" = FALSE),
      events = list(
        # reset the session to hide sliders and back/next buttons
        "oncomplete" = I('history.go(0)'),
        "onbeforchange" = I("function(steps) {
                            Shiny.onInputChange('current_step', data-stepnumber);
                            ;}")
        # "onbeforechange" = I('
        #     if (targetElement.getAttribute("data-step")==="2") {
        #         $(".newClass").css("max-width", "800px").css("min-width","800px");
        #     } else {
        #         $(".newClass").css("max-width", "500px").css("min-width","500px");
        #     }')
      )
    )
  })



  #  Toggle the sidebar when a user press the help button
  observe({
    shinyjs::toggleClass(
      id = "right_sidebar",
      class = "control-sidebar-open",
      condition = input$help
    )
  })

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
