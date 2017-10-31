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
  #  
  #  Integrate equations using deSolve package to generate table.
  #  out is a reactive intermediate component that is called by
  #  to make plots or other stuffs
  #
  #-------------------------------------------------------------------------
  
  out <- reactive({
    
    if (input$run_Ca_inject == "TRUE") { # IV Ca injection followed by EGTA infusion
      times <- seq(0,input$tmaxCainj,by = 1)
      out <- as.data.frame(ode(y = state, 
                               times = times, 
                               func = calcium_phosphate_Caiv, 
                               parms = parameters))
    } else if (input$run_PO4_inject == "TRUE") { # PO4 injection 
      times <- seq(0,input$tmaxPO4inj,by = 1) 
      out <- as.data.frame(ode(y = state, 
                               times = times, 
                               func = calcium_phosphate_PO4iv, 
                               parms = parameters))
    } else if (input$run_PO4_gav == "TRUE") { # PO4 gavage
      times <- seq(0,input$tmaxPO4gav,by = 1)
      out <- as.data.frame(ode(y = state, 
                               times = times, 
                               func = calcium_phosphate_PO4gav, 
                               parms = parameters))
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  The network part: make interactive diagramms of Ca and PO4 homeostasis
  #  as well as regulation by hormones such as PTH, vitamin D3 and FGF23
  #  
  #
  #-------------------------------------------------------------------------
  
  # Generate the CaP Graph network
  
  nodes_Ca <- reactive({generate_nodes_Ca(input)})
  edges_Ca <- reactive({generate_edges_Ca(input)})
  
  # Generate the output of the Ca graph to be used in body
  
  output$network_Ca <- renderVisNetwork({
    
    nodes_Ca <- nodes_Ca()
    edges_Ca <- edges_Ca()
    
    generate_network(nodes = nodes_Ca, edges = edges_Ca, usephysics = TRUE) %>%
      # simple click event to allow graph ploting
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}") %>% 
      # unselect node event
      visEvents(deselectNode = "function(nodes) {
                Shiny.onInputChange('current_node_bis_id', 'null');
                ;}") %>%
      # add the doubleclick function to handle zoom views
      visEvents(doubleClick = "function(nodes) {
                Shiny.onInputChange('current_node_bis_id', nodes.nodes);
                }") %>%  
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', edges.edges);
                ;}") %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:0, y:-13.43},
                offset: {x: 0, y:0} })}") %>% 
      # very important: allow to detect the web browser used by client
      # use before drawing the network. Works with find_navigator.js
      visEvents(type = "on", beforeDrawing = "function() {
                Shiny.onInputChange('browser', navigator.sayswho);
                ;}") %>%
      visEvents(type = "once", startStabilizing = "function() {
                this.moveTo({scale:2})}") # to set the initial zoom (1 by default)
    
    })
  
  output$id <- renderPrint({
    input$current_edge_id
  })

  output$id_bis <- renderPrint({
    input$current_node_id
  })
  
  #-------------------------------------------------------------------------
  #
  #  Navigation counter
  #
  #-------------------------------------------------------------------------

  # create a navigation counter to trigger sequential graph animation
  counter_nav <- reactiveValues(diagram = 0) 

  # counter decrease
  observeEvent(input$back1,{
    if (counter_nav$diagram == 0) {
    } else {counter_nav$diagram <- counter_nav$diagram - 1}
  })

  # counter incrementation
  observeEvent(input$next1,{
      counter_nav$diagram <- counter_nav$diagram + 1
  })

  # reset the counter if higher than 5
  observeEvent(input$next1,{
    if (counter_nav$diagram > 6) {
      counter_nav$diagram <- 0
    }
  })
  
  # reset also if another simulation is choosen
  observeEvent(eval(parse(text = paste("input$", extract_running_sim(input)[[2]], sep = ""))),{
    counter_nav$diagram <- 0
    edges_Ca <- edges_Ca()
    edges_Ca$color <- "black"
    edges_Ca$witdh <- 4
    visNetworkProxy("network_Ca", session) %>%  # then reset the graph
      visUpdateEdges(edges = edges_Ca)
  })
  
  output$counter_nav <- renderPrint({counter_nav$diagram}) 
  
  #------------------------------------------------------------------------- 
  #  
  #  Animations of arrows when event occurs (php1, hypopara, hypoD3)
  #  
  #-------------------------------------------------------------------------
  
  observeEvent(input$next1 | input$back1 ,{
    
    edges_Ca <- edges_Ca()
    current_sim <- extract_running_sim(input)[[1]]
    dynamic_sim <- !(input$run_Ca_inject | input$run_PO4_inject |
                     input$run_PO4_gav)
    # only if a simulation is selected 
    # dynamics simulations are excluded since calculations
    # are performed live contrary to steady-state simulations
    if (!is_empty(current_sim) &&  dynamic_sim) {
      if (eval(parse(text = paste("input$", current_sim, sep = "")))) {
        arrow_lighting(edges = edges_Ca,
                       simulation = current_sim,
                       counter = counter_nav$diagram,
                       input,
                       session)
      } 
    }
    
  })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Events for the CaPO4 Homeostasis diagramm whenever a flux change
  #  
  #-------------------------------------------------------------------------
  
  # Change arrow color relatively to the value of fluxes for Ca injection/PO4 injection as well as PO4 gavage
  
  observe({

    out <- out()
    edges_Ca <- edges_Ca()

    arrow_lighting_live(out, edges = edges_Ca, session)
    
    # print(list(extract_running_sim(input)[[2]], 
    #            extract_running_sim(input)[[1]],
    #            extract_animation(input)))

  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Make interactive plot by loading tables of diseases
  #  be careful when put on webserver to change the path of table to avoid
  #  reading errors (see all_plot.R script)
  #-------------------------------------------------------------------------
  
  # draw each of the 6 plots as a function of the selected simulation
  output$plot <- renderPlotly({
  
    # extract only the name of the simulation
    # and not "run_simulation", as given by extract_running_sim()
    current_sim <- extract_running_sim(input)[[1]] %>%
      str_extract("_\\w+") %>%
      str_replace("_", "")
    
    # avoid that plotly returns an error when current_sim is empty
    req(current_sim)
    
    if (!is_empty(current_sim) | input$help) {
      
      if (current_sim == "Ca_inject" | current_sim == "PO4_inject" | 
          current_sim == "PO4_gav") {
        eval(parse(text = paste("make_plot_", current_sim, "(input)", sep = "")))    
        
      } else {
        eval(parse(text = paste("make_plot_", current_sim, "()", sep = "")))
      }
  
    }
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Update the time navigation bar for Ca/EGTA inject, PO4 inject and 
  #  PO4 gavage by clicking on back or next buttons
  #  
  #-------------------------------------------------------------------------
  
  observeEvent(input$back1,{# if the user clicks on back
    
    sliderlist <- list(
      inputId = c("tmaxCainj","tmaxPO4inj","tmaxPO4gav"),
      value = c(input$tmaxCainj - 10 , input$tmaxbisPO4inj - 10, 
                input$tmaxtrisPO4gav - 10),
      max = c(120, 250, 250))
    
    pmap(sliderlist, 
         updateSliderInput, 
         session = session, 
         label = "Current Time", 
         min = 1, 
         step = 1)
    
  })
  
  observeEvent(input$next1,{# if the user clicks on next
    
    sliderlist <- list(
      inputId = c("tmaxCainj","tmaxPO4inj","tmaxPO4gav"),
      value = c(input$tmaxCainj + 10 , input$tmaxbisPO4inj + 10, 
                input$tmaxtrisPO4gav + 10))
    
    pmap(sliderlist, 
         updateSliderInput, 
         session = session, 
         label = "Current Time", 
         min = 1, 
         step = 1)
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Prevent user from selecting multiple boxes using shinyjs functions
  #  
  #-------------------------------------------------------------------------
  
  observeEvent(eval(parse(text = paste("input$", extract_running_sim(input)[[2]], sep = ""))), {
    
    # extract the list of simulations and the current one as well as its index
    # to properly select boxes to enable/disable
    sim_list <- extract_running_sim(input)[[2]]
    current_simulation <- extract_running_sim(input)[[1]]
    index <- which(sim_list == current_simulation)

    # if one simulation run, disable all boxes that are not related to that one
    if (!is_empty(current_simulation)) {
      temp <- eval(parse(text = paste("input$", current_simulation, sep = "")))
      if (temp == "TRUE") {
        map(sim_list[-index], disable)
      } 
    } else {# if no simulation runs, all boxes are available
      map(sim_list, enable)
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Notification events to explain the user how to play with the app
  #
  #-------------------------------------------------------------------------
  
  # help animation with introjs
  
  observeEvent(input$help,{
    introjs(session)
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Notification events for PHP1, hypoD3 and hypopara
  #
  #-------------------------------------------------------------------------
  
  
  observeEvent(c(input$run_php1, input$run_hypopara, input$run_hypoD3, 
                 counter_nav$diagram, input$notif2_switch),{
                   
                   current_simulation <- extract_running_sim(input)[[1]]
                   req(current_simulation)
                   
                   if (input$run_php1 == "TRUE" | input$run_hypopara == "TRUE" | 
                       input$run_hypoD3 == "TRUE") {

                     generate_notification(counter = counter_nav$diagram, 
                                           simulation = current_simulation,
                                           allowed = input$notif2_switch)
                   }
                 })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Useful tasks such as save, reset, load ...
  #  
  #-------------------------------------------------------------------------
  
  # display or not display the network background
  observe({
    
    if (!is_empty(input$background_choice)) {
      if (input$background_choice == "rat") {
        addClass(id = "network_cap", class = "network_caprat")
        removeClass(id = "network_cap", class = "network_caphuman")
      } else {
        removeClass(id = "network_cap", class = "network_caprat")
        addClass(id = "network_cap", class = "network_caphuman")
      }
    } else {
      addClass(id = "network_cap", class = "network_capnone")
      removeClass(id = "network_cap", class = "network_caphuman")
      removeClass(id = "network_cap", class = "network_caprat")
    }
    #print(input$background_choice)
    
  })
  
  # prevent user from selecting multiple background
  observe({
    
    if (is.element("rat", input$background_choice) &&
        !is.element("human", input$background_choice)) {
      disable(selector = "#background_choice input[value='human']")
    } else {
      enable(selector = "#background_choice input[value='human']")
    }
    if (is.element("human", input$background_choice) && 
        !is.element("rat", input$background_choice)) {
      disable(selector = "#background_choice input[value='rat']")
    } else {
      enable(selector = "#background_choice input[value='rat']")
    }
    
  })
  
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
  
  # reset parameters individually
  button_states <- reactiveValues(values = list())
  
  observeEvent(c(input$reset_tmaxCainj,
                 input$reset_tmaxPO4inj,
                 input$reset_tmaxPO4gav),{
                   
                   # call the function to reset the given slider
                   sliders_reset(button_states, input)
                   
                 })
  
})