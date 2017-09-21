#-------------------------------------------------------------------------
#  This application is a R-Shiny implementation of a calcium and phosphate 
#  homeostasis model. It aims at being used by medical students but also
#  researchers. See https://divadnojnarg.github.io for more informations
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  #------------------------------------------------------------------------- 
  #  Store times, state and parameters in reactive values that can
  #  react to user inputs
  #  
  #-------------------------------------------------------------------------
  
  # Basic reactive expressions needed by the solver
  times <- reactive({ seq(0,input$tmax, by = 1) }) # maybe 0:input$tmax is faster?
  
  t_target <- reactive({ input$t_now })
  
  # Parameters: multiply the real parameter value by the user input. 
  # By default, user input = 1 so that parameters are well defined
  parameters <- reactive({ 
    
    c("k_prod_PTHg" = 4.192*input$k_prod_PTHg, 
      "beta_exo_PTHg" = 5.9e-002*input$beta_exo_PTHg,
      "gamma_exo_PTHg" = 5.8e-002*input$gamma_exo_PTHg, 
      "D3_inact" = 2.5e-005*input$D3_inact, 
      "k_deg_D3" = 1e-003*input$k_deg_D3,
      "k_prod_FGF" = 6.902e-011*input$k_prod_FGF, 
      "I_Ca" = 2.2e-003*input$I_Ca, 
      "Lambda_ac_Ca" = 5.5e-004*input$Lambda_ac_Ca,
      "Lambda_ac_P" = 2.75e-004*input$Lambda_ac_Ca, 
      "Lambda_res_min" = 1e-004*input$Lambda_res_min, 
      "delta_res_max" = 6e-004*input$delta_res_max,
      "k_p_Ca" = 0.44*input$k_p_Ca, 
      "k_f_Ca" = 2.34e-003*input$k_f_Ca, 
      "I_P" = 1.55e-003*input$I_P, 
      "k_pc" = 0.1875*input$k_pc,
      "k_cp" = 1e-003*input$k_cp, 
      "k_p_P" = 13.5*input$k_p_P, 
      "k_f_P" = 0.25165*input$k_f_P, 
      "k_fet" = 0.3*input$k_fet,
      "k_c_CPP" = 3*input$k_c_CPP, 
      "Na" = 142*input$Na, 
      "Prot_tot_p" = 0.6*input$Prot_tot_p, 
      "Vp" = 0.01*input$Vp,
      "GFR" = 2e-003*input$GFR) 
    
  })
  
  # make a vector of input$parameters, fixed_parameters and calculated parameters
  parameters_bis <- reactive({ c(parameters(), parameters_fixed) })
  
  #------------------------------------------------------------------------- 
  #  
  #  Integrate equations using deSolve package to generate table
  #  out is a reactive intermediate component that is called by
  #  to make plots or other stuffs
  #
  #-------------------------------------------------------------------------
  
  out <- reactive({
    
    parameters_bis <- parameters_bis()
    times <- times()
    
    as.data.frame(ode(y = state, 
                      times = times, 
                      func = calcium_phosphate_core, 
                      parms = parameters_bis))
    
  })
  
  #output$table <- renderDataTable( out(), options = list(pageLength = 10) )
  
  #------------------------------------------------------------------------- 
  #  
  #  The network part: make interactive diagramms of Ca and PO4 homeostasis
  #  as well as regulation by hormones such as PTH, vitamin D3 and FGF23
  #  
  #-------------------------------------------------------------------------
  
  # Generate the CaP Graph network
  
  nodes_Ca <- reactive({ generate_nodes_Ca(input) })
  edges_Ca <- reactive({ generate_edges_Ca(input) })
  
  # Generate the output of the Ca graph to be used in body
  
  output$network_Ca <- renderVisNetwork({
    
    nodes_Ca <- nodes_Ca()
    edges_Ca <- edges_Ca()
    
    generate_network(nodes = nodes_Ca, edges = edges_Ca, 
                     css_export = css_export) %>%
      # simple click event to allow graph ploting
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}") %>% 
      # add the doubleclick function to handle zoom views
      visEvents(doubleClick = "function(nodes) {
                Shiny.onInputChange('current_node_bis_id', nodes.nodes);
                }") %>%  
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', edges.edges);
                ;}") %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "once", afterDrawing = "function() {
                this.moveTo({ position: {x: 0, y:-13.43},
                offset: {x: 0, y:0} })}") %>% 
      visEvents(type = "once", startStabilizing = "function() {
                this.moveTo({scale:2})}") # to set the initial zoom (1 by default)
    
  })
  
  output$id <- renderPrint({ input$current_edge_id })
  output$id_bis <- renderPrint({ input$current_node_id })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  The zoom network part: make zoom network of CaPO4 homeostasis
  #  PTHg network
  #  
  #
  #-------------------------------------------------------------------------
  
  nodes_PTHg <- reactive({ generate_nodes_PTHg() })
  edges_PTHg <- reactive({ generate_edges_PTHg() })
  
  output$network_PTH <- renderVisNetwork({
    
    nodes_PTHg <- nodes_PTHg()
    edges_PTHg <- edges_PTHg()
    
    generate_network(nodes = nodes_PTHg, edges = edges_PTHg, 
                     css_export = css_export_zoom) %>%
      visEvents(selectEdge = "function(edges) {
                 Shiny.onInputChange('current_edge_bis_id', edges.edges);
                 ;}") %>%
      # set value to NULL to prevent sliders from being displayed
      visEvents(deselectEdge = "function(edges) {
         Shiny.onInputChange('current_edge_bis_id', 'null');
                 ;}") %>% 
      # very important: change the whole graph position after drawing
      visEvents(type = "once", afterDrawing = "function() {
                this.moveTo({ position: {x: 2.5, y:-2.5},
                offset: {x: 0, y:0} })}")
    
  })
  
  output$id_tris <- renderPrint({ input$current_edge_bis_id })
  
  
  
  # node coordinates
  # useful when developing to return x and y position
  output$position <- renderPrint( vals$coords ) 
  vals <- reactiveValues(coords=NULL, viewposition = NULL)
  observe({
    invalidateLater(1000)
    visNetworkProxy("network_Ca") %>% visGetPositions()
    vals$coords <- if (!is.null(input$network_Ca_positions)) 
      do.call(rbind, input$network_Ca_positions)
  })
  
  # view position (of the camera)
  # useful to set a proper view
  output$viewposition <- renderPrint({ vals$viewposition })
  observe({
    invalidateLater(1000)
    visNetworkProxy("network_Ca") %>% visGetViewPosition()
    vals$viewposition <- if (!is.null(input$network_Ca_viewPosition))
      do.call(rbind, input$network_Ca_viewPosition)
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Render image of the experimental graphs
  #
  #-------------------------------------------------------------------------
  
  output$TAL <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www', 'kidney_TAL.jpg'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$PT <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www', 'kidney_PT.jpg'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$DCT_CNT <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www', 'kidney_DCT-CNT.jpg'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$intestine_zoom <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www', 'intestine_zoom.jpg'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$bone_zoom <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./www', 'bone_zoom.jpg'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  #------------------------------------------------------------------------- 
  #  
  #  The graph part: calls out(), parameters_bis()
  #  Interactive graph as a result of click on the diagram
  #
  #-------------------------------------------------------------------------
  
  
  # Generate a graph when node is clicked. The graph corresponds to the node clicked
  
  output$plot_node <- renderPlotly({
    
    validate(need(input$current_node_id, 'Select one node on the graph!'))
    validate(need(input$current_node_id != 0, 'Select one node on the graph!'))
    
    out <- out()
    parameters_bis <- parameters_bis()
    
    plot_node(node = input$current_node_id , out, parameters_bis)
    
  })
  
  output$plot_edge <- renderPlotly({
    
    validate(need(input$current_edge_id, 'Select one edge on the graph!'))
    validate(need(input$current_edge_id != 0, 'Select one edge on the graph!'))
    
    out <- out()
    
    # call the plot_edge() function defined in global.R
    plot_edge(edge = input$current_edge_id , out)
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Events in the network, triggered by changing input,...
  #  
  #
  #-------------------------------------------------------------------------
  
  # The following commented lines are mandatory
  # if a new flux is added so as to generate
  # the new table needed for computations
  
  # calc_change_table <- reactive({
  # 
  #   as.data.frame(calc_change(out())) # calculate the base case value of change and let it isolate
  # 
  # })
  # 
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("data-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(calc_change_table(), file)
  #   }
  # )
  
  events <- reactive(c(input$Lambda_ac_Ca,
              input$k_p_Ca,input$k_p_P,
              input$k_f_Ca, input$k_f_P,
              input$Lambda_res_min,
              input$delta_res_max,
              input$GFR, input$k_pc,
              input$k_cp, input$k_prod_PTHg,
              input$D3_inact, input$k_deg_D3,
              input$k_prod_FGF))
  
  events_PTH <- reactive(c(input$k_prod_PTHg,
                  input$beta_exo_PTHg,
                  input$gamma_exo_PTHg))
  
  observeEvent(c(events(), events_PTH()),{ 

    out <- out()
    edges_Ca <- edges_Ca()
    edges_PTHg <- edges_PTHg()
    
    # for Ca/PO4 fluxes, call the flux_lighting() function
    flux_lighting(edges_Ca, 
                  network = "network_Ca", 
                  out, 
                  events = events(), 
                  t_target())
    # for the PTH network
    flux_lighting(edges_PTHg, 
                  network = "network_PTH", 
                  out, 
                  events = events_PTH(), 
                  t_target())
    
  })
  
  observe({
    updateSliderInput(session, 
                      inputId = "t_now",
                      value = input$tmax, 
                      max = input$tmax, 
                      step = 1)
  })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Notification events to explain the user how to play with the app
  #
  #-------------------------------------------------------------------------
  
  # help animation with introjs
  
  observeEvent(input$notif_switch,{
    introjs(session)
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Handle dangerous parameter values by the user
  #  
  #-------------------------------------------------------------------------
  
  observeEvent(input$beta_exo_PTHg,{ # critical value for PTHg
    
    if (input$beta_exo_PTHg < 0.9) {
      
      sendSweetAlert(messageId = "failSw", 
                     title = "Ooops ...", 
                     text = "Invalid parameter value", 
                     type = "error")
      reset("beta_exo_PTHg") # value is reset
      
    }
    
  })
  
  # prevent the user to put infinite value in the max time of integration
  
  observeEvent(input$tmax,{ # critical value for tmax
    
    if (input$tmax > 30000) {
      
      sendSweetAlert(messageId = "failSw", 
                     title = "Ooops ...", 
                     text = "Invalid parameter value: the maximum 
                            time of simulation is too high!", 
                     type = "error")
      reset("tmax") # value is reset
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Prevent user from selecting multiple boxes of parameters
  #  
  #-------------------------------------------------------------------------
  
  # prevent user from unselecting all graph components
  
  observeEvent(input$network_Ca_choice, {
    
    if (is.element("PO4", input$network_Ca_choice) && 
        !is.element("Ca", input$network_Ca_choice)) {
      disable(selector = "#network_Ca_choice input[value='PO4']")
    } else {
      enable(selector = "#network_Ca_choice input[value='PO4']")
    }
    if (is.element("Ca", input$network_Ca_choice) && 
        !is.element("PO4", input$network_Ca_choice)) {
      disable(selector = "#network_Ca_choice input[value='Ca']")
    } else {
      enable(selector = "#network_Ca_choice input[value='Ca']")
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Useful tasks such as save, reset, load ...
  #  
  #-------------------------------------------------------------------------
  
  # reset parameters individually
  
  button_states <- reactiveValues(values = list())
  
  observeEvent(c(input$resetPTHsynthesis,
                 input$resetPTHexocytosis,
                 input$resetPTHexocytosisinhib,
                 input$resetD3inact,
                 input$resetD3deg,
                 input$resetFGFsynth,
                 input$resetCaintake,
                 input$resetPintake,
                 input$resetkpCa,
                 input$resetkfCa,
                 input$resetkpP,
                 input$resetkfP,
                 input$resetacCa,
                 input$resetresmin,
                 input$resetresmax,
                 input$resetkpc,
                 input$resetkcp,
                 input$reset_t_now),{
                   
                   # call the function to reset the given slider
                   sliders_reset(button_states, input)
                
                 })
  
  # display or do not display the network background
  observeEvent(input$background_switch,{
    
    toggleClass(id = "network_cap", class = "network_cap",
                condition = input$background_switch)
    
  })
  
})