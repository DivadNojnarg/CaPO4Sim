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
  
  times <- reactive({seq(0,input$tmax, by = 1)})
  
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
  
  # set up initial conditions for solving ODEs
  states <- reactiveValues(
    val = list(c("PTH_g" = 1288.19, "PTH_p" = 0.0687, "D3_p" = 564.2664, 
                 "FGF_p" = 16.78112, "Ca_p" = 1.2061, "Ca_f" = 1.8363, 
                 "Ca_b" = 250, "PO4_p" = 1.4784, "PO4_f" = 0.7922, 
                 "PO4_b" = 90, "PO4_c" = 2.7719, "CaHPO4_p" = 0.1059, 
                 "CaH2PO4_p" = 0.0038, "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, 
                 "CaH2PO4_f" = 0.0031, "CaProt_p" = 1.4518,"NaPO4_p" = 0.9135, 
                 "Ca_tot" = 2.4914, "PO4_tot" = 2.8354, "EGTA_p" = 0, 
                 "CaEGTA_p" = 0)), 
    counter = 1)
  
  # update initial conditions to the last state of the system each time an event
  # has occured. Do not update if the same disease is triggered twice ...
  observeEvent(input$run,{
    req(input$disease_selected)
    if (is.na(match(input$disease_selected, event$table$event))) {
      out <- out()
      temp_state <- c(
        "PTH_g" = out[nrow(out),"PTH_g"], "PTH_p" = out[nrow(out),"PTH_p"], 
        "D3_p" = out[nrow(out),"D3_p"], "FGF_p" = out[nrow(out),"FGF_p"], 
        "Ca_p" = out[nrow(out),"Ca_p"], "Ca_f" = out[nrow(out),"Ca_f"], 
        "Ca_b" = out[nrow(out),"Ca_b"], "PO4_p" = out[nrow(out),"PO4_p"], 
        "PO4_f" = out[nrow(out),"PO4_f"], "PO4_b" = out[nrow(out),"PO4_b"], 
        "PO4_c" = out[nrow(out),"PO4_c"], "CaHPO4_p" = out[nrow(out),"CaHPO4_p"], 
        "CaH2PO4_p" = out[nrow(out),"CaH2PO4_p"], "CPP_p" = out[nrow(out),"CPP_p"], 
        "CaHPO4_f" = out[nrow(out),"CaHPO4_f"], "CaH2PO4_f" = out[nrow(out),"CaH2PO4_f"], 
        "CaProt_p" = out[nrow(out),"CaProt_p"],"NaPO4_p" = out[nrow(out),"NaPO4_p"], 
        "Ca_tot" = out[nrow(out),"Ca_tot"], "PO4_tot" = out[nrow(out),"PO4_tot"], 
        "EGTA_p" = out[nrow(out),"EGTA_p"], "CaEGTA_p" = out[nrow(out),"CaEGTA_p"]
      )
      states$val[[states$counter]] <- temp_state
      states$counter <- states$counter + 1
      
    } else {
      NULL
    }
  })
  
  # Event to be added in the timeLine
  output$current_event <- renderUI({
    req(input$disease_selected)
    if (input$disease_selected == "primary-hyperparathyroidism") {
      tagList(
        timelineBox(
          #title
          timelineLabel(
            text = HTML(paste("<b>", Sys.Date(),"</b>")), color = "purple"
          ),
          # body
          timelineItem(
            icon = shiny::icon("heartbeat bg-purple"),
            header = HTML('<strong><a href="#">Primary-Hyperparathyroidism</strong>'),
            body = "patient has primary hyperparathyroidism.",
            footer = HTML('<a class="btn btn-primary btn-xs">Read more</a>',
                          '<a class="btn btn-danger btn-xs">Delete</a>'),
            itemText = paste(Sys.time())
          )
        )
      )
    }
  })
  
  
  # Set events parameters in reactiveValues so as to modify them later
  event <- reactiveValues(
    table = data.frame(
      id = NULL,
      event = NULL,
      rate = NULL,
      start_time = NULL,
      stop_time = NULL,
      status = NULL
    ),
    counter = 1
  )
  
  # generate the slider corresponding to a given treatment
  output$sliderInject <- renderUI({
    req(input$treatment_selected)
    generate_slider_events(input)
  })
  
  # create an "add_disease" button
  output$button_add_disease <- renderUI({
    req(input$disease_selected)
    actionBttn(inputId = "add_disease", 
               label = NULL, 
               style = "material-circle", 
               color = "danger", 
               icon = icon("plus"))
  })
  
  # Add/Remove treatments to the event list
  observeEvent(input$add_treatment,{
    # the same treatment can be added
    # multiple times. However, parathyroidectomy
    # cannot be performed more than once
      if (input$treatment_selected != "parathyroid surgery" &
          input$treatment_selected != "cinacalcet") {
        temp_event <- data.frame(
          id = event$counter,
          event = input$treatment_selected,
          rate = input[[paste(input$treatment_selected)]],
          start_time = input$t_start,
          stop_time = input$t_stop,
          status = "active"
        )
        event$table <- rbind(event$table, temp_event)
        event$counter <- event$counter + 1
      } else {
        if (is.na(match("parathyroid surgery", event$table$event))) {
          temp_event <- data.frame(
            id = event$counter,
            event = input$treatment_selected,
            rate = "undefined", 
            start_time = "undefined",
            stop_time = "undefined",
            status = "active"
          )
          event$table <- rbind(event$table, temp_event)
          event$counter <- event$counter + 1
        } else {
          showNotification("Cannot perform parathyroidectomy more than once!",
                           type = "error", closeButton = TRUE)
        }
      }
  })
  
  observe({
    print(event$table)
  })
  
  # Add/Remove diseases to the event list
  observeEvent(input$add_disease, {
    # check if the disease is already present or not
    if (is.na(match(input$disease_selected, event$table$event))) {
      temp_event <- data.frame(
        id = event$counter,
        event = input$disease_selected,
        rate = "undefined",
        start_time = "undefined",
        stop_time = "undefined",
        status = "active"
      )
      event$table <- rbind(event$table, temp_event)
      event$counter <- event$counter + 1
    } else {
      showNotification("Cannot add the same disease twice!",
                       type = "error", closeButton = TRUE)
    }
  })
  
  
  # storing parameters event from the data frame to a reactive list
  parameters_event <- reactive({time_extractor(event$table)})
  
  # Create parameters sets for all diseases an treatments
  # need to write && !is.null(input$Ca_inject) since 
  # input$Ca_inject does not exist before Ca_inject is selected
  parameters_disease <- reactive({ 
    c("k_prod_PTHg" = ifelse(is.element("primary-hyperparathyroidism", input$disease_selected), 300*4.192, 
                             ifelse(is.element("hypoparathyroidism", input$disease_selected), 0, 4.192)), 
      "D3_inact" = ifelse(is.element("vitamin D3 deficiency", input$disease_selected), 0, 2.5e-005),
      "PTX_coeff" = ifelse(is.element("parathyroid surgery", input$treatment_selected), 0, 1),
      "k_inject_Ca" = ifelse(is.element("Ca iv injection", input$treatment_selected), input$Ca_inject, 0), 
      "Ca_food" = ifelse(is.element("Ca supplementation", input$treatment_selected), input$Ca_food, 2.2e-003),
      "k_inject_D3" = ifelse(is.element("vitamin D3 iv injection", input$treatment_selected), input$D3_inject, 0),
      "k_inject_P" = ifelse(is.element("PO4 iv injection", input$treatment_selected), input$P_inject, 0),
      "P_food" = ifelse(is.element("PO4 supplementation", input$treatment_selected), input$P_food, 1.55e-003))
  })
  
  # Alert user from forbidden couples
  observeEvent(input$disease_selected, {
    if ((!is.na(match("primary-hyperparathyroidism", event$table$event)) &
        input$disease_selected == "hypoparathyroidism") |
        (!is.na(match("hypoparathyroidism", event$table$event)) &
         input$disease_selected == "primary-hyperparathyroidism")) { 
      showNotification("Cannot have primary hyperparathyroidism and 
                       hypoparathyroidism at the same time!",
                       type = "error", closeButton = TRUE)
      disable(id = "add_disease")
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
  
  # make a vector of disease related parameters, 
  # fixed_parameters and parameters related to events
  parameters <- reactive({
    c(parameters_disease(), parameters_fixed, parameters_event()) 
  }) 
  
  # Render the event table
  output$event_table <- renderTable({event$table})
  
  output$test <- renderPlot({
    input$run
    x <- 1:10
    plot(x, log(x), type = 'l')
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Integrate equations using deSolve package to generate table
  #  out is a reactive intermediate component that is called by
  #  to make plots or other stuffs
  #
  #-------------------------------------------------------------------------
  
  out <- reactive({
    input$run
    isolate({
      parameters <- parameters()
      times <- times()
      as.data.frame(ode(y = states$val[[length(states$val)]], 
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
  #   input$run
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
    input$network_hormonal_choice
    
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
      visEvents(type = "on", beforeDrawing = "function() {
                this.moveTo({scale:0.6})}") # to set the initial zoom (1 by default)
  })
  
  # handle the size of organ and hormonal nodes
  output$size_nodes_organs <- renderUI({
    tagList(
      knobInput("size_organs", 
                "Organs", 
                min = 50, 
                max = 100, 
                value = 70, 
                step = 5,
                displayPrevious = TRUE,
                fgColor = "#A9A9A9", 
                inputColor = "#A9A9A9",
                skin = "tron",
                width = "100px", 
                height = "100px")
    )
  })
  
  output$size_nodes_hormones <- renderUI({
    tagList(
      knobInput("size_hormones", 
                "Hormones", 
                min = 20, 
                max = 60, 
                value = 40, 
                step = 5,
                displayPrevious = TRUE,
                fgColor = "#A9A9A9", 
                inputColor = "#A9A9A9",
                skin = "tron",
                width = "100px", 
                height = "100px")
    )
  })
  
  # control width of arrows
  output$width_arrows_organs <- renderUI({
    tagList(
      knobInput("width_organs", 
                "Organs",
                angleOffset = -90,
                angleArc = 180,
                min = 4, 
                max = 14, 
                value = 8, 
                step = 1,
                displayPrevious = TRUE,
                fgColor = "#A9A9A9", 
                inputColor = "#A9A9A9",
                skin = NULL,
                width = "100px", 
                height = "100px")
    )
  })
  
  output$width_arrows_hormones <- renderUI({
    tagList(
      knobInput("width_hormones", 
                "Hormones", 
                angleOffset = -90,
                angleArc = 180,
                min = 1, 
                max = 8, 
                value = 4, 
                step = 1,
                displayPrevious = TRUE,
                fgColor = "#A9A9A9", 
                inputColor = "#A9A9A9",
                skin = NULL,
                width = "100px", 
                height = "100px")
    )
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
              "oncomplete" = I('history.go(0)')
              )
            )
    
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
  
  
  
  # prevent user from selecting multiple treatments as the same time
  observe({
    #req(input$treatment_selected)
    if (!is.null(input$treatment_selected)) {
      treatment <- match.arg(input$treatment_selected, treatment_choices)
      idx <- match(input$treatment_selected, treatment_choices)
      other_treatments <- treatment_choices[-idx]
      lapply(seq_along(other_treatments), FUN = function(j) {
        disable(selector = paste0("#treatment_selected input[value='", other_treatments[[j]], "']"))
      })
    } else {
      enable(id = "treatment_selected")
    }
  })
  
  
  # prevent user from selecting multiple diseases as the same time
  observe({
    #req(input$treatment_selected)
    if (!is.null(input$disease_selected)) {
      disease <- match.arg(input$disease_selected, disease_choices)
      idx <- match(input$disease_selected, disease_choices)
      other_diseases <- disease_choices[-idx]
      lapply(seq_along(other_diseases), FUN = function(j) {
        disable(selector = paste0("#disease_selected input[value='", other_diseases[[j]], "']"))
      })
    } else {
      enable(id = "disease_selected")
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
  
  # when enable regulation is selected, activates all the checkboxes
  # the reverse case does not work for unknow reason 
  observeEvent(input$network_hormonal_choice, {
    if (input$network_hormonal_choice == TRUE) {
      updatePrettyCheckboxGroup(session, inputId = "network_Ca_choice", 
                                selected = c("Ca","PO4", "PTH", "D3", "FGF23"))
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
    generate_dynamicFooter() 
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