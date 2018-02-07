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
  
  times <- reactive({ seq(0,input$tmax, by = 1) })
  
  # Generate special state for php1, hypoD3, hypopara # Only do when the original files are unavailable or corrupted
  
  # observeEvent(input$disease_selected,{
  # 
  #   out <- out()
  # 
  #   if(input$disease_selected == "primary-hyperparathyroidism"){
  # 
  #     write.csv(x = c(out[nrow(out),2:23]), file = "init_php1.csv")
  # 
  #   }
  #   else if (input$disease_selected == "hypoparathyroidism"){
  # 
  #     write.csv(x = c(out[nrow(out),2:23]), file = "init_hypopara.csv")
  # 
  #   }
  #   else if (input$disease_selected == "vitamin D3 deficiency"){
  # 
  #     write.csv(x = c(out[nrow(out),2:23]), file = "init_hypoD3.csv")
  # 
  #   }
  # 
  # })
  
  # observeEvent(input$disease_selected,{
  # 
  #   out <- out()
  # 
  #   if (input$disease_selected == "vitamin D3 deficiency"){
  # 
  #     write.csv(x = c(out[nrow(out),2:23]), file = "init_hypoD3.csv")
  # 
  #   }
  # 
  # })
  
  
  state <- reactive({ 
    if (!is.null(input$disease_selected)) {
      if (input$disease_selected == "primary-hyperparathyroidism") {
        state_php1
      } else if (input$disease_selected == "hypoparathyroidism") {
        state_hypopara
      } else if (input$disease_selected == "vitamin D3 deficiency") {
        state_hypoD3
      }
    } else {# default initial state
      c("PTH_g" = 1288.19, "PTH_p" = 0.0687, "D3_p" = 564.2664, 
        "FGF_p" = 16.78112, "Ca_p" = 1.2061, "Ca_f" = 1.8363, 
        "Ca_b" = 250, "PO4_p" = 1.4784, "PO4_f" = 0.7922, 
        "PO4_b" = 90, "PO4_c" = 2.7719, "CaHPO4_p" = 0.1059, 
        "CaH2PO4_p" = 0.0038, "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, 
        "CaH2PO4_f" = 0.0031, "CaProt_p" = 1.4518,"NaPO4_p" = 0.9135, 
        "Ca_tot" = 2.4914, "PO4_tot" = 2.8354, "EGTA_p" = 0, 
        "CaEGTA_p" = 0) 
    }
  })
  
  # Set events parameters in reactiveValues so as to modify them later
  
  event_table <- reactiveValues(df = data.frame(event = NULL, 
                                                rate = NULL, 
                                                start_time = NULL,
                                                stop_time = NULL, 
                                                status = NULL))
  
  # cumulate events when add button is selected
  
  observeEvent(input$add_newCaiv,{ 
    #fill the event table
    df_Caiv <- data.frame(
      event = "Ca_iv", 
      rate = paste(input$Ca_inject, "(µmol/min)"), 
      start_time = input$t_start_Cainject,
      stop_time = input$t_stop_Cainject, 
      status = ifelse(is.element("Ca iv injection", input$treatment_selected), 
                      "active", "inactive")
    ) # does not work
    event_table$df <- rbind(event_table$df, df_Caiv)
  })
  
  observeEvent(input$add_newCaintake,{ 
    df_Caintake <- data.frame(
      event = "Ca_gavage", 
      rate = paste(input$Ca_food, "(µmol/min)"), 
      start_time = input$t_start_Caintake,
      stop_time = input$t_stop_Caintake,
      status = ifelse(is.element("Ca supplementation", input$treatment_selected), 
                      "active", "inactive")
    )
    event_table$df <- rbind(event_table$df, df_Caintake)
  })
  
  observeEvent(input$add_newD3iv,{ 
    df_D3iv <- data.frame(
      event = "D3_iv", 
      rate = paste(input$D3_inject, "(pmol/min)"), 
      start_time = input$t_start_D3inject,
      stop_time = input$t_stop_D3inject,
      status = ifelse(is.element("vitamin D3 iv injection", input$treatment_selected), 
                      "active", "inactive")
    )
    event_table$df <- rbind(event_table$df, df_D3iv)
  })
  
  observeEvent(input$add_newPiv,{ 
    df_Piv <- data.frame(
      event = "P_iv", 
      rate = paste(input$P_inject, "(µmol/min)"), 
      start_time = input$t_start_Pinject,
      stop_time = input$t_stop_Pinject,
      status = ifelse(is.element("PO4 iv injection", input$treatment_selected), 
                      "active", "inactive")
    )
    event_table$df <- rbind(event_table$df, df_Piv)
  })
  
  observeEvent(input$add_newPintake,{ 
    df_Pintake <- data.frame(
      event = "P_gavage", 
      rate = paste(input$P_food, "(µmol/min)"), 
      start_time = input$t_start_Pintake,
      stop_time = input$t_stop_Pintake,
      status = ifelse(is.element("PO4 supplementation", input$treatment_selected), 
                      "active", "inactive")
    )
    event_table$df <- rbind(event_table$df, df_Pintake)
  })
  
  
  # delete a given event when delete button is selected
  observeEvent(input$delete_oldCaiv,{ 
    # if the index of element to delete does not belong to the data frame
    if (input$delete_Caiv_id > nrow(event_table$df)) { 
      showNotification("Please delete an event which is in the list!",
                       type = "error", closeButton = TRUE)
    } else {# if it is an element of the data frame
      # if there is still only one line in the data frame
      if (nrow(event_table$df) >= 1) { 
        # test if the event name corresponds to Ca_iv or not
        if (is.element("Ca_iv", event_table$df[input$delete_Caiv_id,1])) { 
          # delete the corresponding row in the event table
          event_table$df <- event_table$df[-input$delete_Caiv_id,] 
        } else {# cannot suppress a D3_iv or P_iv with the Ca_iv button (security)
          showNotification("Cannot delete element different from Ca_iv 
                             injection with this button. Please use the 
                             delete button related to the event you would 
                             like to remove!",
                           type = "error", closeButton = TRUE)
        }
      }
    }
  })
  
  observeEvent(input$delete_oldCaintake,{
    if (input$delete_Caintake_id > nrow(event_table$df)) {
      showNotification("Please delete an event which is in the list!",
                       type = "error", closeButton = TRUE)
    } else{
      if (nrow(event_table$df) >= 1) {
        if (is.element("Ca_gavage", event_table$df[input$delete_Caintake_id,1])) {
          event_table$df <- event_table$df[-input$delete_Caintake_id,]
        } else {
          showNotification("Cannot delete element different from Ca_iv 
                             injection with this button. Please use the 
                             delete button related to the event you would 
                             like to remove!",
                           type = "error", closeButton = TRUE)
        }
      }
    }
  })
  
  observeEvent(input$delete_oldD3iv,{
    if (input$delete_D3iv_id > nrow(event_table$df)) {
      showNotification("Please delete an event which is in the list!",
                       type = "error", closeButton = TRUE)
    } else{
      if (nrow(event_table$df) >= 1) {
        if (is.element("D3_iv", event_table$df[input$delete_D3iv_id,1])) {
          event_table$df <- event_table$df[-input$delete_D3iv_id,]
        } else {
          showNotification("Cannot delete element different from Ca_iv 
                           injection with this button. Please use the delete 
                           button related to the event you would like to remove!",
                           type = "error", closeButton = TRUE)
        }
      }
    }
  })
  
  observeEvent(input$delete_oldPiv,{
    if (input$delete_Piv_id > nrow(event_table$df)) {
      showNotification("Please delete an event which is in the list!",
                       type = "error", closeButton = TRUE)
    } else {
      if (nrow(event_table$df) >= 1) {
        if (is.element("P_iv", event_table$df[input$delete_Piv_id,1])) {
          event_table$df <- event_table$df[-input$delete_Piv_id,]
        } else {
          showNotification("Cannot delete element different from Ca_iv 
                           injection with this button. Please use the delete 
                           button related to the event you would like to remove!",
                           type = "error", closeButton = TRUE)
        }
      }
    }
  })
  
  observeEvent(input$delete_oldPintake,{
    if (input$delete_Pintake_id > nrow(event_table$df)) {
      showNotification("Please delete an event which is in the list!",
                       type = "error", closeButton = TRUE)
    } else {
      if (nrow(event_table$df) >= 1) {
        if (is.element("P_gavage", event_table$df[input$delete_Pintake_id,1])) {
          event_table$df <- event_table$df[-input$delete_Pintake_id,]
        } else {
          showNotification("Cannot delete element different from Ca_iv injection with this button. 
                            Please use the delete button related to the event you would like to remove!",
                           type = "error", closeButton = TRUE)
        }
      }
    }
  })
  
  # storing parameters event from the data frame to a reactive list
  parameters_event <- reactive({time_extractor(event_table)})
  
  # Create parameters sets for all diseases an treatments
  # need to write && !is.null(input$Ca_inject) since 
  # input$Ca_inject does not exist before Ca_inject is selected
  parameters_disease <- reactive({ 
    c("k_prod_PTHg" = ifelse(is.element("primary-hyperparathyroidism", input$disease_selected), 300*4.192, 
                             ifelse(is.element("hypoparathyroidism", input$disease_selected), 0, 4.192)), 
      "D3_inact" = ifelse(is.element("vitamin D3 deficiency", input$disease_selected), 0.5 * 2.5e-005, 2.5e-005),
      "PTX_coeff" = ifelse(is.element("parathyroid surgery", input$treatment_selected), 0, 1),
      "k_inject_Ca" = ifelse(is.element("Ca iv injection", input$treatment_selected), input$Ca_inject, 0), 
      "Ca_food" = ifelse(is.element("Ca supplementation", input$treatment_selected), input$Ca_food, 2.2e-003),
      "k_inject_D3" = ifelse(is.element("vitamin D3 iv injection", input$treatment_selected), input$D3_inject, 0),
      "k_inject_P" = ifelse(is.element("PO4 iv injection", input$treatment_selected), input$P_inject, 0),
      "P_food" = ifelse(is.element("PO4 supplementation", input$treatment_selected), input$P_food, 1.55e-003))
  })
  
  # Alert user from forbidden couples
  observeEvent(input$disease_selected, {
    if (is.element("primary-hyperparathyroidism", input$disease_selected) 
        && is.element("hypoparathyroidism", input$disease_selected)) { 
      showNotification("Cannot have primary hyperparathyroidism and 
                       hypoparathyroidism at the same time!",
                       type = "error", closeButton = TRUE)
    } #else if (is.element("primary-hyperparathyroidism", input$disease_selected) 
    #            && is.element("vitamin D3 deficiency", input$disease_selected)) {
    #   showNotification("Cannot select to diseases at the same time!",
    #                    type = "error", closeButton = TRUE)
    # } else if (is.element("hypoparathyroidism", input$disease_selected) 
    #            && is.element("vitamin D3 deficiency", input$disease_selected)) {
    #   showNotification("Cannot select to diseases at the same time!",
    #                    type = "error", closeButton = TRUE)
    # }
  })
  
  # make a vector of disease related parameters, fixed_parameters and parameters related to events
  parameters <- reactive({
    c(parameters_disease(), parameters_fixed, parameters_event()) 
  }) 
  
  # Render the event table
  output$event_table <- renderTable({event_table$df})
  
  #------------------------------------------------------------------------- 
  #  
  #  Integrate equations using deSolve package to generate table
  #  out is a reactive intermediate component that is called by
  #  to make plots or other stuffs
  #
  #-------------------------------------------------------------------------
  
  out <- reactive({
    input$play
    isolate({
      parameters <- parameters()
      state <- state()
      times <- times()
      as.data.frame(ode(y = state, 
                        times = times, 
                        func = calcium_phosphate_core, 
                        parms = parameters))
    })
  })
  
  #output$table <- renderDataTable( out(), options = list(pageLength = 10) )
  
  # observe({
  #
  #   out <- out()
  #
  #   input$play
  #
  #   write.csv(x = out, file = "out.csv")
  #
  # })
  
  #------------------------------------------------------------------------- 
  #  
  #  The network part: make interactive diagramms of Ca and PO4 homeostasis
  #  as well as regulation by hormones such as PTH, vitamin D3 and FGF23
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
  
  #-------------------------------------------------------------------------
  #
  #  The graph part: calls out(), parameters_bis()
  #  Interactive graph as a result of click on the diagram
  #
  #-------------------------------------------------------------------------
  
  # Generate a graph when node is clicked. 
  # The graph corresponds to the node clicked
  output$plot_node <- renderPlotly({
    validate(need(input$current_node_id, "Select one node on the graph!"))
    out <- out()
    plot_node(input, node = input$current_node_id , out, parameters_fixed)
  })
  
  output$plot_edge <- renderPlotly({
    validate(need(input$current_edge_id, "Select one edge on the graph!"))
    out <- out()
    # call the plot_edge() function defined in global.R
    plot_edge(edge = input$current_edge_id , out)
  })
  
  
  #-------------------------------------------------------------------------
  #
  #  Events for the CaPO4 Homeostasis diagramm whenever a flux change
  #
  #-------------------------------------------------------------------------
  
  # Change arrow color relatively to the value of fluxes for Ca injection/PO4 
  # injection as well as PO4 gavage
  observe({
    out <- out()
    edges_Ca <- edges_Ca()
    arrow_lighting_live(out, edges = edges_Ca, session, t_target = input$t_now)
  })
  
  #-------------------------------------------------------------------------
  #
  #  Handle dangerous parameter values by the user
  #
  #-------------------------------------------------------------------------
  
  # prevent the user to put infinite value in the max time of integration
  # prevent the user to put infinite value in the max time of integration
  # With compiled code, tmax = 100000 min is a correct value
  observeEvent(input$tmax,{
    # critical value for tmax
    feedbackWarning(
      inputId = "tmax",
      condition = !is.na(input$tmax),
      text = "tmax should exist and set between 1 and 100000."
    )
    
    # check if input tmax does not exists or is not numeric
    if (is.na(input$tmax)) {
      sendSweetAlert(session, 
                     title = "Ooops ...", 
                     text = "Invalid value: tmax should be set correctly.", 
                     type = "error")
      reset("tmax") # value is reset
    } else {
      # if yes, check it is negative
      if (input$tmax <= 0) {
        sendSweetAlert(session, 
                       title = "Ooops ...", 
                       text = "Invalid value: tmax must be higher than 0.", 
                       type = "error")
        reset("tmax") # value is reset
        # check whether it is too high
      } else if (input$tmax > 100000) {
        sendSweetAlert(session, 
                       title = "Ooops ...", 
                       text = "Invalid value: the maximum 
                       time of simulation is too high!", 
                       type = "error")
        reset("tmax") # value is reset
      }
    }
  })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Notification events to explain the user how to play with the app
  #
  #-------------------------------------------------------------------------
  
  # help animation with introjs
  # options are provided to control the size of the help
  # Do not forget to wrap the event content in I('my_function')
  # otherwise it will fail
  observeEvent(input$help,{
    introjs(session, 
            options = list("nextLabel" = "Next step!",
                           "prevLabel" = "Did you forget something?",
                           "tooltipClass" = "newClass",
                           "showProgress" = TRUE,
                           "showBullets" = FALSE),
            events = list(# reset the session to hide sliders and back/next buttons
              "oncomplete" = I('history.go(0)')))
    
    #   "onbeforechange" = I('
    #                                  if (targetElement.getAttribute("data-step")==="2") {
    #                        $(".newClass").css("max-width", "800px").css("min-width","800px");  
    # } else {
    #                        $(".newClass").css("max-width", "500px").css("min-width","500px");
    # }'),
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Toggle each sidebar when a user press the help button
  #
  #-------------------------------------------------------------------------
  
  observe({
    shinyjs::toggleClass(id = "controlbar", 
                         class = "control-sidebar-open",
                         condition = input$help)
  })
  
  # observeEvent(input$help,{
  #   shinyjs::removeClass(selector = "body", 
  #                        class = "sidebar-collapse")
  # })
  
  #-------------------------------------------------------------------------
  #
  #  Useful tasks such as save, reset, load ...
  #
  #-------------------------------------------------------------------------
  
  # reset parameters individually
  button_states <- reactiveValues(values = list())
  observeEvent(input$reset_t_now,{
    # call the function to reset the given slider
    sliders_reset(button_states, input)
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
  
  # change the dashboard skin
  current_skin <- reactiveValues(color = NULL)
  previous_skin <- reactiveValues(color = NULL)
  observeEvent(input$skin,{
    # the first time, previous_skin$color is set to the first
    # skin value at opening. Same thing for current value
    if (is.null(previous_skin$color)) {
      previous_skin$color <- current_skin$color <- input$skin
    } else {
      current_skin$color <- input$skin
      # if the old skin is the same as the current selected skin
      # then, do nothing
      if (previous_skin$color == current_skin$color) {
        NULL
      } else {
        # otherwise, remove the old CSS class and add the new one
        shinyjs::removeClass(selector = "body", class = paste("skin", previous_skin$color, sep = "-"))
        shinyjs::addClass(selector = "body", class = paste("skin", current_skin$color, sep = "-"))
        # the current skin is added to previous_skin to be ready for
        # the next change
        previous_skin$color <- c(previous_skin$color, current_skin$color)[-1]
      }
    }
  })
  
  # Custom footer
  output$dynamicFooter <- renderFooter({ 
    dashboardFooter(
      mainText = h5(
        "2017-2018,", 
        img(src = "interface_logo.png", height = "30px"),
        HTML("<span id=\"tab\"></span>"),
        "Built with", 
        img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
        "by",
        img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
        HTML("<span id=\"tab\"></span>"),
        "Funded by",
        img(src = "nccr_logo.png", height = "50px"),
        img(src = "uzh_logo.png", height = "30px"),
        "and",
        img(src = "unil_logo.png", height = "55px")), 
      subText = HTML("<b>Version:</b> Beta 3")
    ) 
  })
  
  
  # reset all the values of box inputs as well as graphs
  observeEvent(input$resetAll,{
    reset("disease_selected")
    reset("treatment_selected")
    reset("tmax")
    reset("t_now")
    
    edges_Ca <- edges_Ca()
    
    visNetworkProxy("network_Ca") %>%
      visUpdateEdges(edges = edges_Ca)
  })
  
})