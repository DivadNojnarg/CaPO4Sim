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

  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Make interactive plot by loading tables of diseases
  #  be careful when put on webserver to change the path of table to avoid
  #  reading errors
  #-------------------------------------------------------------------------
  
  # php1
  output$php1_plot <- renderPlotly({
      
      input$run_php1
      
      make_plot_php1()
    
  })
  
  # Vitamin D3 deficiency
  output$hypoD3_plot <- renderPlotly({
      
      input$run_hypoD3
      
      make_plot_hypoD3()
    
  })
  
  # Hypoparathyroidism
  output$hypopara_plot <- renderPlotly({
    
      input$run_hypopara

      make_plot_hypopara()
      
    
  })
  
  # Ca iv injection
  
  path_to_Ca_iv <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/iv_Ca.csv"
  #path_to_Ca_iv <- "/srv/shiny-server/capApp/case_studies_app/www/iv_Ca.csv"
  Ca_iv_table <- read.csv(path_to_Ca_iv)
  
  output$Ca_iv_plot <- renderPlotly({
    
    input$run_Ca_inject
    
    injectevents <- data.frame(times = c(0, 60, 65, 70, 80, 90, 100, 110, 120), 
                               Ca_val = 1/1.35*c(1.35, 1.45, 1.30, 1.20, 1.15, 1.10, 1.10, 1.00, 1.05),
                               PTH_val = 1/65*c(65, 10, 190, 260, 300, 260, 240, 290, 310), 
                               err_Ca = 1/1.35*2*c(0.02,0.04,0.04,0.06,0.04,0.06,0.06,0.07, 0.04),
                               err_PTH = 1/65*c(20, 0, 70, 100, 70, 70, 50, 70, 110))
    
    xvar <- list(title = "time (min)", range = c(0, max(Ca_iv_table[,1]) + 10))
    yvar1 <- list(title = "Normalized [Ca2+]p", range = c(0,2))
    yvar2 <- list(title = "Normalized [PTH]p", range = c(0,10))
    
    p1 <- plot_ly(Ca_iv_table, x = Ca_iv_table[,1], 
                  y = Ca_iv_table[,"Ca_p"]/Ca_iv_table[1,"Ca_p"], 
                  type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_markers(x = injectevents$times, 
                  y = injectevents$Ca_val, mode = 'markers', 
                  symbols = "o", marker = list(size = 10, color = 'black'),
                  error_y = list(array = injectevents$err_Ca, color = 'black'), 
                  line = list(color = 'white')) %>%
      add_lines(x = Ca_iv_table[,1], 
                y = Ca_iv_table[,"Ca_p"]/Ca_iv_table[1,"Ca_p"], 
                type = "scatter", mode = "lines", 
                line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_lines(x = input$tmaxCainj, y = c(0,2), 
                line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      #add_annotations(x = input$tmax, y = 0, text = "Current Time") %>%
      layout(xaxis = xvar, yaxis = yvar1)
    
    p2 <- plot_ly(data = Ca_iv_table, x = Ca_iv_table[,1], 
                  y = Ca_iv_table[,"PTH_p"]/Ca_iv_table[1,"PTH_p"], 
                  type = "scatter", mode = "lines",
                  line = list(color = 'black', width = 2)) %>%
      add_trace(x = injectevents$times, 
                y = injectevents$PTH_val, mode = 'markers', symbols = "o", 
                marker = list(size = 10, color = 'black'),
                error_y = list(array = injectevents$err_PTH, color = 'black'), 
                line = list(color = 'white')) %>%
      add_lines(x = Ca_iv_table[,1], 
                y = Ca_iv_table[,"PTH_p"]/Ca_iv_table[1,"PTH_p"], 
                type = "scatter", mode = "lines",
                line = list(color = 'black', width = 2)) %>%
      add_lines(x = input$tmaxCainj, y = c(0,10), 
                line = list(size = 6, color = 'orange', 
                            dashed = "dashdot")) %>%
      #add_annotations(x = input$tmax, y = 0, text = "Current Time") %>%
      layout(xaxis = xvar, yaxis = yvar2)
    
    p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE, nrows = 1, margin = 0.05)
    
    hide_legend(p)
    
  })
  
  # PO4 iv injection
  
  path_to_PO4_iv <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/iv_PO4.csv"
  #path_to_PO4_iv <- "/srv/shiny-server/capApp/case_studies_app/www/iv_PO4.csv"
  PO4_iv_table <- read.csv(path_to_PO4_iv)
  
  output$PO4_iv_plot  <- renderPlotly({
    
    input$run_PO4_inject
    
    injectevents <- data.frame(times = c(0,10,25,40,55,70,130,190,250), 
                               PO4_val = c(2.86, 5.84, 5.59, 4.69, 4.24, 3.83, 3.08, 3.27, 3.26),
                               Ca_val = 1/2.4*c(2.4,1.92,1.97,1.93,1.86,1.76,1.89,2.03,2.02),
                               PTH_val = c(1,8.72, 7.80, 7, 7.64, 9.34, 9.74, 7.12, 6), 
                               err_PO4 = c(0.3, 0.33, 0.33, 0.26, 0.24, 0.13, 0.15, 0.21, 0.39),
                               err_Ca = c(0.05, 0.04, 0.07, 0.04, 0.03, 0.04, 0.09, 0.04, 0.09),
                               err_PTH = c(0.53, 1.40, 1.34, 0.88, 0.97, 0.93, 1.47, 1.42, 0.87))
    
    xvar <- list(title = "time (min)", range = c(0, max(PO4_iv_table[,1])))
    yvar1 <- list(title = "[PO4]p (mM)", range = c(0,8))
    yvar2 <- list(title = "Normalized [Ca2+]p", range = c(0,2))
    yvar3 <- list(title = "Normalized [PTH]p", range = c(0,20))
    
    p1 <- plot_ly(PO4_iv_table, x = PO4_iv_table[,1], 
                  y = PO4_iv_table[,"PO4_tot"], type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
      add_markers(x = injectevents$times, 
                  y = injectevents$PO4_val, mode = 'markers', symbols = "o", 
                  marker = list(size = 10, color = 'black'),
                  error_y = list(array = injectevents$err_PO4, color = 'black'), 
                  line = list(color = 'white')) %>%
      add_lines(x = PO4_iv_table[,1], 
                y = PO4_iv_table[,"PO4_tot"], type = "scatter", mode = "lines", 
                line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
      add_lines(x = input$tmaxPO4inj, y = c(0,8), 
                line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = NULL, yaxis = yvar1)
    
    p2 <- plot_ly(PO4_iv_table, x = PO4_iv_table[,1], 
                  y = PO4_iv_table[,"Ca_tot"]/PO4_iv_table[1,"Ca_tot"], 
                  type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_markers(x = injectevents$times, 
                  y = injectevents$Ca_val, mode = 'markers', symbols = "o", 
                  marker = list(size = 10, color = 'black'),
                  error_y = list(array = injectevents$err_Ca, color = 'black'), 
                  line = list(color = 'white')) %>%
      add_lines(x = PO4_iv_table[,1], 
                y = PO4_iv_table[,"Ca_tot"]/PO4_iv_table[1,"Ca_tot"], 
                type = "scatter", mode = "lines", 
                line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_lines(x = input$tmaxPO4inj, y = c(0,2), 
                line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      
      layout(xaxis = xvar, yaxis = yvar2)
    
    p3 <- plot_ly(data = PO4_iv_table, x = PO4_iv_table[,1], 
                  y = PO4_iv_table[,"PTH_p"]/PO4_iv_table[1,"PTH_p"],
                  type = "scatter", mode = "lines",
                  line = list(color = 'black', width = 2)) %>%
      add_trace(x = injectevents$times, 
                y = injectevents$PTH_val, mode = 'markers', symbols = "o", 
                marker = list(size = 10, color = 'black'),
                error_y = list(array = injectevents$err_PTH, color = '#000000'), 
                line = list(color = 'white')) %>%
      add_lines(x = PO4_iv_table[,1], 
                y = PO4_iv_table[,"PTH_p"]/PO4_iv_table[1,"PTH_p"], 
                type = "scatter", mode = "lines", 
                line = list(color = 'black', width = 2)) %>%
      add_lines(x = input$tmaxPO4inj, y = c(0,20), 
                line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = xvar, yaxis = yvar3)
    
    p <- subplot(p1, p2, p3, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.05)
    
    hide_legend(p)
    
  })
  
  # PO4 gavage
  
  path_to_PO4_gav <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/gav_PO4.csv"
  #path_to_PO4_gav <- "/srv/shiny-server/capApp/case_studies_app/www/gav_PO4.csv"
  PO4_gav_table <- read.csv(path_to_PO4_gav)
  
  output$PO4_gav_plot  <- renderPlotly({
    
    gavevents <- data.frame(times = c(0,10,25,40,55,70,130,190,250), 
                            PO4_val = c(2.87, 2.91, 3.30, 3.20, 3.10, 3.23, 2.97, 2.72, 2.89),
                            Ca_val = 1/2.08*c(2.08,1.80,1.99,1.96,1.91,1.83,1.75,1.74,1.79),
                            PTH_val = c(1, 1.60, 2.17, 2.14, 2.12, 1.77, 1.95, 1.69, 1.80), 
                            err_PO4 = c(0.14, 0.1, 0.07, 0.2, 0.1, 0.09, 0.13, 0.1, 0.19),
                            err_Ca = c(0.03, 0.07, 0.03, 0.07, 0.04, 0.06, 0.03, 0.03, 0.04),
                            err_PTH = c(0.16, 0.34, 0.40, 0.20, 0.29, 0.12, 0.19, 0.19, 0.14))
    
    xvar <- list(title = "time (min)", range = c(0, max(PO4_gav_table[,1])))
    yvar1 <- list(title = "[PO4]p (mM)", range = c(0,8))
    yvar2 <- list(title = "Normalized [Ca2+]p", range = c(0,2))
    yvar3 <- list(title = "Normalized [PTH]p", range = c(0,20))
    
    p1 <- plot_ly(PO4_gav_table, x = PO4_gav_table[,1], 
                  y = PO4_gav_table[,"PO4_tot"], type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
      add_markers(x = gavevents$times, 
                  y = gavevents$PO4_val, mode = 'markers', symbols = "o", 
                  marker = list(size = 10, color = 'black'),
                  error_y = list(array = gavevents$err_PO4, color = 'black'), 
                  line = list(color = 'white')) %>%
      add_lines(x = PO4_gav_table[,1], y = PO4_gav_table[,"PO4_tot"], 
                type = "scatter", mode = "lines", 
                line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
      add_lines(x = input$tmaxPO4gav, y = c(0,8), 
                line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = NULL, yaxis = yvar1)
    
    p2 <- plot_ly(PO4_gav_table, x = PO4_gav_table[,1], 
                  y = PO4_gav_table[,"Ca_tot"]/PO4_gav_table[1,"Ca_tot"], 
                  type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_markers(x = gavevents$times, 
                  y = gavevents$Ca_val, mode = 'markers', symbols = "o", 
                  marker = list(size = 10, color = 'black'),
                  error_y = list(array = gavevents$err_Ca, color = 'black'), 
                  line = list(color = 'white')) %>%
      add_lines(x = PO4_gav_table[,1], 
                y = PO4_gav_table[,"Ca_tot"]/PO4_gav_table[1,"Ca_tot"], 
                type = "scatter", mode = "lines", 
                line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_lines(x = input$tmaxPO4gav, y = c(0,2), 
                line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = xvar, yaxis = yvar2)
    
    p3 <- plot_ly(data = PO4_gav_table, x = PO4_gav_table[,1], 
                  y = PO4_gav_table[,"PTH_p"]/PO4_gav_table[1,"PTH_p"], type = "scatter", mode = "lines",
                  line = list(color = 'black', width = 2)) %>%
      add_trace(x = gavevents$times, 
                y = gavevents$PTH_val, mode = 'markers', symbols = "o", 
                marker = list(size = 10, color = 'black'),
                error_y = list(array = gavevents$err_PTH, color = 'black'), 
                line = list(color = 'white')) %>%
      add_lines(x = PO4_gav_table[,1], 
                y = PO4_gav_table[,"PTH_p"]/PO4_gav_table[1,"PTH_p"], 
                type = "scatter", mode = "lines", 
                line = list(color = 'black', width = 2)) %>%
      add_lines(x = input$tmaxPO4gav, y = c(0,20), 
                line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = xvar, yaxis = yvar3)
    
    p <- subplot(p1, p2, p3, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.05)
    
    hide_legend(p)
    
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
                   
                   if (input$run_php1 == "TRUE" | input$run_hypopara == "TRUE" | 
                       input$run_hypoD3 == "TRUE") {

                     current_simulation <- extract_running_sim(input)[[1]]
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