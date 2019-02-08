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
# |
# |  PART 1: ODE intregration (when needed)
# |  PART 2: CaPO4 network generation (highlight, blink, nodes, edges, ...)
# |  PART 3: Ploting part
# |  PART 4: Educational content (modals, notifications, glossary, userInfo)
# |  PART 5: Useful tasks
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  #------------------------------------------------------------------------- 
  #  
  #  1) Integrate equations using deSolve package to generate table.
  #     out is a reactive intermediate component that is called
  #     to make plots or other stuffs. This part is only needed when
  #     doing dynamic case studies
  #
  #-------------------------------------------------------------------------
  
  # out <- reactive({
  #   
  #   if (input$run_Ca_inject == "TRUE") { # IV Ca injection followed by EGTA infusion
  #     times <- seq(0,input$tmaxCainj,by = 1)
  #     out <- as.data.frame(
  #       ode(y = state, times = times, func = calcium_phosphate_Caiv, parms = parameters)
  #     )
  #   } else if (input$run_PO4_inject == "TRUE") { # PO4 injection 
  #     times <- seq(0,input$tmaxPO4inj,by = 1) 
  #     out <- as.data.frame(
  #       ode(y = state, times = times, func = calcium_phosphate_PO4iv, parms = parameters)
  #     )
  #   } else if (input$run_PO4_gav == "TRUE") { # PO4 gavage
  #     times <- seq(0,input$tmaxPO4gav,by = 1)
  #     out <- as.data.frame(
  #       ode(y = state, times = times, func = calcium_phosphate_PO4gav, parms = parameters)
  #     )
  #   }
  # })
  
  #------------------------------------------------------------------------- 
  #  
  #  2) The network part: make interactive diagramms of Ca and PO4 homeostasis
  #     as well as regulation by hormones such as PTH, vitamin D3 and FGF23
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
    input$network_hormonal_choice
    
    generate_network(input, nodes = nodes_Ca, edges = edges_Ca, usephysics = TRUE) %>%
      # simple click event to select a node
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}") %>% 
      # unselect node event
      visEvents(deselectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', 'null');
                ;}") %>%
      # add the doubleclick for nodes
      visEvents(doubleClick = "function(nodes) {
                Shiny.onInputChange('current_node_bis_id', nodes.nodes);
                }") %>%  
      # simple click event for selecting edges
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', edges.edges);
                ;}") %>%
      # unselect edge event
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', 'null');
                ;}") %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", stabilized = "function() {
                this.moveTo({ position: {x:0, y:-13.43},
                offset: {x: 0, y:0} })}") %>% 
      visEvents(
        type = "on", 
        initRedraw = paste0("
          function() {
            this.moveTo({scale:", if (input$isMobile) 0.3 else 0.6, "});
        }")
      )
  })
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ #
  #
  # Only for development purpose
  #
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ #
  
  output$id <- renderPrint({input$current_edge_id})
  output$id_bis <- renderPrint({input$current_node_id})
  output$background_choice <- renderPrint({input$background_choice})
  
  # node coordinates
  # useful when developing to return x and y position for each node
  vals <- reactiveValues(coords = NULL, viewposition = NULL, scale = NULL)
  output$position <- renderPrint({vals$coords})
  observe({
    invalidateLater(1000)
    visNetworkProxy("network_Ca") %>% visGetPositions()
    vals$coords <- if (!is.null(input$network_Ca_positions)) 
      do.call(rbind, input$network_Ca_positions)
  })
  
  # view position (of the camera)
  # useful to set a proper view
  output$viewposition <- renderPrint({vals$viewposition})
  observe({
    invalidateLater(1000)
    visNetworkProxy("network_Ca") %>% visGetViewPosition()
    vals$viewposition <- if (!is.null(input$network_Ca_viewPosition))
      do.call(rbind, input$network_Ca_viewPosition)
  })
  
  # scale (get the zoomView...)
  output$scale <- renderPrint({vals$scale})
  observe({
    invalidateLater(1000)
    visNetworkProxy("network_Ca") %>% visGetScale()
    vals$scale <- if (!is.null(input$network_Ca_scale))
      do.call(rbind, list(input$network_Ca_scale))
  })
  
  
  # generate the knob to control 
  # node and edges properties
  generate_network_knobs(input, output, session)
  
  
  # change the selected node size to
  # better highlight it
  last <- reactiveValues(selected_node = NULL, selected_edge = NULL)
  
  observeEvent(input$current_node_id, {
    req(input$current_node_id)
    selected_node <- input$current_node_id
    nodes_Ca <- nodes_Ca()
    # javascript return null instead of NULL
    # cannot use is.null
    if (!identical(selected_node, "null")) {
      last$selected_node <- selected_node
      # organ nodes
      if (selected_node %in% c(1:5, 7:8, 11)) {
        nodes_Ca$size[selected_node] <- 100
        # Kidney zoom node
      } else if (selected_node == 6) {
        nodes_Ca$size[selected_node] <- 214
        # regulation nodes
      } else {
        nodes_Ca$size[selected_node] <- 57
      }
      visNetworkProxy("network_Ca") %>%
        visUpdateNodes(nodes = nodes_Ca)
      # reset the node size when unselected
    } else {
      if (last$selected_node %in% c(1:5, 7:8, 11)) {
        nodes_Ca$size[last$selected_node] <- 70
      } else if (last$selected_node == 6) {
        nodes_Ca$size[last$selected_node] <- 150
      } else {
        nodes_Ca$size[last$selected_node] <- 40
      }
      visNetworkProxy("network_Ca") %>%
        visUpdateNodes(nodes = nodes_Ca)
    }
  })
  
  # change the selected edge size to
  # better highlight it
  observeEvent(input$current_edge_id,{
    req(input$current_edge_id)
    selected_edge <- input$current_edge_id
    edges_Ca <- edges_Ca()
    edge_id <- match(selected_edge, edges_Ca$id)
    if (!identical(selected_edge, "null")) {
      last$selected_edge <- edge_id
      # organs edges
      if (edge_id %in% c(1:12)) {
        edges_Ca$width[edge_id] <- 24
        # regulations edges
      } else {
        edges_Ca$width[edge_id] <- 12
      }
      visNetworkProxy("network_Ca") %>%
        visUpdateEdges(edges = edges_Ca)
      # reset the edge size when unselected
    } else {
      if (edge_id %in% c(1:12)) {
        edges_Ca$width[edge_id] <- 8
      } else {
        edges_Ca$width[edge_id] <- 4
      }
      visNetworkProxy("network_Ca") %>%
        visUpdateEdges(edges = edges_Ca)
    }
  })
  

  # Navigation counter
  # create a navigation counter to trigger sequential graph animation
  counter_nav <- reactiveValues(diagram = 0) 
  
  # counter decrease
  observeEvent(input$back1,{
    if (counter_nav$diagram == 0) {
      NULL
    } else {
      counter_nav$diagram <- counter_nav$diagram - 1
    }
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
  observeEvent(eval(parse(text = paste0("input$", extract_running_sim(input)[[2]]))),{
    counter_nav$diagram <- 0
    edges_Ca <- edges_Ca()
    edges_Ca$color <- "black"
    edges_Ca$witdh <- 4
    visNetworkProxy("network_Ca", session) %>%  # then reset the graph
      visUpdateEdges(edges = edges_Ca)
  })
  
  # generate a progress bar
  output$counter_progress <- renderUI({
    if (input$run_php1 == "TRUE" | input$run_hypopara == "TRUE" | 
        input$run_hypoD3 == "TRUE" | input$help) {
      progressBar(
        id = "counter_progress",
        value = counter_nav$diagram,
        total = 6,
        title = "Progress",
        size = "s",
        striped = TRUE,
        status = if (counter_nav$diagram <= 1) {
          "danger"
        } else if (counter_nav$diagram >= 2 & counter_nav$diagram <= 5) {
          "warning"
        } else {
          "success"
        },
        display_pct = FALSE
      )
    }
  })
  
  
  # Animations of arrows when event occurs (php1, hypopara, hypoD3)
  observeEvent(input$next1 | input$back1 ,{
    
    edges_Ca <- edges_Ca()
    current_sim <- extract_running_sim(input)[[1]]
    dynamic_sim <- !(input$run_Ca_inject | input$run_PO4_inject | input$run_PO4_gav)
    # only if a simulation is selected 
    # dynamics simulations are excluded since calculations
    # are performed live contrary to steady-state simulations
    if (!is_empty(current_sim) &&  dynamic_sim) {
      if (eval(parse(text = paste0("input$", current_sim)))) {
        
        # the code below ensures that nodes related to
        # perturbations, ie PTHg for php1 and hypopara
        # D3 nodes for hypoD3, blink when the counter equals 1
        if (counter_nav$diagram == 1) {
          nodes_Ca <- nodes_Ca()
          if (input$run_php1 == TRUE | input$run_hypopara == TRUE) {
            lapply(1:2, FUN = function(i){
              if ((i %% 2) != 0) {
                nodes_Ca$hidden[11] <- TRUE
                visNetworkProxy("network_Ca") %>%
                  visUpdateNodes(nodes = nodes_Ca)
              } else {
                nodes_Ca$hidden[11] <- FALSE
                visNetworkProxy("network_Ca") %>%
                  visUpdateNodes(nodes = nodes_Ca)
              }
              Sys.sleep(0.5)
            })
          } else if (input$run_hypoD3 == TRUE) {
            lapply(1:2, FUN = function(i){
              if ((i %% 2) != 0) {
                nodes_Ca$hidden[c(13:15)] <- TRUE
                visNetworkProxy("network_Ca") %>%
                  visUpdateNodes(nodes = nodes_Ca)
              } else {
                nodes_Ca$hidden[c(13:15)] <- FALSE
                visNetworkProxy("network_Ca") %>%
                  visUpdateNodes(nodes = nodes_Ca)
              }
              Sys.sleep(0.5)
            })
          }
        }
        
        # make arrow yellow and blink
        # (see model_utils.R)
        arrow_lighting(
          edges = edges_Ca,
          simulation = current_sim,
          counter = counter_nav$diagram,
          input,
          session
        )
      } 
    }
  })
  
  
  #  Events for the CaPO4 Homeostasis diagramm whenever a flux change
  # Change arrow color relatively to the value of 
  # fluxes for Ca injection/PO4 injection as well as PO4 gavage
  # observe({
  #   out <- out()
  #   edges_Ca <- edges_Ca()
  #   arrow_lighting_live(out, edges = edges_Ca, session)
  # })
  
  #------------------------------------------------------------------------- 
  #  
  #  3) Make interactive plot by loading tables of diseases
  #     be careful when put on webserver to change the path of table to avoid
  #     reading errors (see all_plot.R script)
  #-------------------------------------------------------------------------
  
  # draw each of the 6 plots as a function of the selected simulation
  output$plot <- renderPlotly({
    
    # extract only the name of the simulation
    # and not "run_simulation", as given by extract_running_sim()
    current_sim <- extract_running_sim(input)[[1]] %>%
      str_extract("_\\w+") %>%
      str_replace("_", "")
    
    # avoid that plotly returns an error when current_sim is empty
    #print(str_detect(names(unlist(reactiveValuesToList(input))), "slider_help"))
    
    if (!is_empty(current_sim)) {
      eval(parse(text = paste0("make_plot_", current_sim, "(input)")))  
    } else {
      if (input$help) {
        make_plot_php1(input)
      } 
    }
  })
  
 
  # Generate sliders for php1, hypopara and hypoD3 and even help
  output$slider <- renderUI({
    current_sim <- extract_running_sim(input)[[1]] %>%
      str_extract("_\\w+") %>%
      str_replace("_", "")
    
    if (input$run_php1 | input$run_hypopara | input$run_hypoD3 | input$help) {
      
      sliderTextInput(
        inputId = ifelse(input$help, "slider_help", paste0("slider_", current_sim)), 
        label = if (input$run_php1 | input$help) {
          "PTH mRNA synthesis fold increase"
        } else if (input$run_hypopara) {
          "PTH mRNA synthesis fold decrease"
        } else if (input$run_hypoD3) {
          "25(OH)D stock"
        }, 
        choices = if (input$run_php1 | input$help) {
          c(20, 100, 200)
        } else {
          c(0.5, 0.1, 0)
        },
        selected = if (input$help) {
          100
        } else {
          ifelse(input$run_php1 | input$run_hypopara | input$run_hypoD3,
                 ifelse(input$run_php1, 100, 0), 1)
        },
        grid = TRUE)
    }
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  4) Educational content: modals, network and graph notifications,
  #     glossary
  #
  #-------------------------------------------------------------------------
  
  # glossary 
  output$glossary <- renderDataTable({
    generate_glossary()
  })
  
  
  
  # generate a patient profile
  userInfo <- reactive({
    generate_userInfo(input)
  })
  output$user <- renderUser({userInfo() %$% head_user})
  
  output$userbttn1 <- renderUI({
    if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
      description <- generate_userFields(input)$description
      if (description == "dead") {
        tagList(
          actionBttn(
            inputId = "cure", 
            label = "Change Rat", 
            style = "fill", 
            color = "success"
          )
        )
      } else {
        tagList(
          actionBttn(
            inputId = "cure", 
            label = "Cure Rat", 
            style = "fill", 
            color = "success"
          )
        )
      }
    } else {
      tagList(
        actionBttn(
          inputId = "cure", 
          label = "Cure Rat", 
          style = "fill", 
          color = "success"
        )
      )
    }
  })
  
  
  
  # Modal for primary hyperparathyroidism, hypopara, ...
  # gives the user some extr information
  observeEvent(
    c(input$run_php1, input$run_hypopara, input$run_hypoD3,
      input$run_Ca_inject, input$run_PO4_inject, input$run_PO4_gav),{
        
        # extract only the last part of the simulation
        # so gives php1, hypopara, hypoD3, ...
        current_sim <- extract_running_sim(input)[[1]] %>%
          str_extract("_\\w+") %>%
          str_replace("_", "")
        
        req(current_sim)
        if (input$modal_switch == TRUE) {
          showModal(eval(parse(text = paste("modal", current_sim, sep = "_"))))
        }
      })
  
  
  # Notification events for PHP1, hypoD3 and hypopara in the CaPO4 network
  # as well as the graph
  observeEvent(
    c(input$run_php1, input$run_hypopara, input$run_hypoD3, 
      counter_nav$diagram, input$notif2_switch), {
        
        current_simulation <- extract_running_sim(input)[[1]]
        req(current_simulation)
        
        if (input$run_php1 == "TRUE" | input$run_hypopara == "TRUE" | 
            input$run_hypoD3 == "TRUE") {
          
          generate_notification(counter = counter_nav$diagram, 
                                simulation = current_simulation,
                                allowed = input$notif2_switch)
          # make it draggable
          jqui_draggable(selector = "#shiny-notification-notifid")
        }
      }, priority = 10)
  
  
  # indicates the user to enable regulations when he launches case studies
  # if they are not already enabled
  observeEvent(
    c(input$run_php1, input$run_hypopara, input$run_hypoD3), {
      
      current_simulation <- extract_running_sim(input)[[1]]
      input_current_simulation <- paste0("input$", extract_running_sim(input)[[1]])
      if (!is_empty(current_simulation)) {
        if (eval(parse(text = input_current_simulation)) & 
            input$network_hormonal_choice == FALSE) {
          sendSweetAlert(
            session = session,
            type = "error",
            title = NULL,
            text = HTML(
              paste("Before going deeper in the case study, 
                    please enable hormonal regulations 
                    in the option part and select both Ca and Pi", 
                    icon("sliders")
              )
            ),
            html = TRUE
            )
        }
      }
    })
  
  # indicate when a user has finished the current activity
  observe({
    if (counter_nav$diagram == 6) {
      current_simulation <- extract_running_sim(input)[[1]] %>%
        str_extract("_\\w+") %>%
        str_replace("_", "")
      input_current_simulation <- paste0("input$", extract_running_sim(input)[[1]])
      sendSweetAlert(
        session = session,
        type = "success",
        title = NULL,
        text = HTML(
          paste("You just finished", current_simulation, "activity.
                You are free to replay the animation or choose another
                activity. Click on the <b>next</b> button again to reset the current
                activity" 
          )
        ),
        html = TRUE
      )
    }
  })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  5) Useful functions: show/hide/reset/...
  #  
  #-------------------------------------------------------------------------
  
  #  Prevent user from selecting multiple boxes using shinyjs functions
  observeEvent(eval(parse(text = paste0("input$", extract_running_sim(input)[[2]]))), {
    
    # extract the list of simulations and the current one as well as its index
    # to properly select boxes to enable/disable
    sim_list <- extract_running_sim(input)[[2]]
    current_simulation <- extract_running_sim(input)[[1]]
    index <- which(sim_list == current_simulation)
    
    # if one simulation run, disable all boxes that are not related to that one
    if (!is_empty(current_simulation)) {
      temp <- eval(parse(text = paste0("input$", current_simulation)))
      if (temp == "TRUE") {
        map(sim_list[-index], disable)
      } 
    } else {# if no simulation runs, all boxes are available
      map(sim_list, enable)
    }
  })
  
  
  
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
  
  
  # Print a short help text in the graph part
  output$info <- renderUI({
    if (sum(c(input$run_php1, input$run_hypopara, input$run_hypoD3)) == 0 &&
        input$help == 0) {
      getting_started()
    } 
  })
  
  
  
  #  Toggle the sidebar when a user press the help button
  observe({
    shinyjs::toggleClass(
      id = "controlbar", 
      class = "control-sidebar-open",
      condition = input$help
    )
  })
  
  # Need to find a way to integrate the userMenu card to the help section
  # observe({
  #   shinyjs::toggleClass(id = "user",
  #                        class = "user-menu open",
  #                        condition = input$help)
  # })
  
  
  # reset all parameters
  observeEvent(input$cure,{
    shinyjs::reset("sidebar_bis")
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
  
  
  # add the blinking button class to the next button in animations
  observe({
    if (input$next1 == 0) {
      addClass(id = "next1", class = "blinking-button")
    } else {
      removeClass(id = "next1", class = "blinking-button")
    }
  })
  
  # prevent user from selecting multiple background
  # observe({
  #   if (is.element("rat", input$background_choice) &&
  #       !is.element("human", input$background_choice)) {
  #     disable(selector = "#background_choice input[value='human']")
  #   } else {
  #     enable(selector = "#background_choice input[value='human']")
  #   }
  #   if (is.element("human", input$background_choice) && 
  #       !is.element("rat", input$background_choice)) {
  #     disable(selector = "#background_choice input[value='rat']")
  #   } else {
  #     enable(selector = "#background_choice input[value='rat']")
  #   }
  # })
  
  # disable dynamic case studies
  observe({
    shinyjs::hide(id = "run_Ca_inject")
    shinyjs::hide(id = "run_PO4_inject")
    shinyjs::hide(id = "run_PO4_gav")
  })
  
  # disable the human background
  observe({
    shinyjs::disable(selector = "#background_choice input[value='human']")
  })
  
  # when enable regulation is selected, activates all the checkboxes
  # the reverse case does not work for unknow reason 
  observeEvent(input$network_hormonal_choice, {
    if (input$network_hormonal_choice == TRUE) {
      updatePrettyCheckboxGroup(
        session, 
        inputId = "network_Ca_choice", 
        selected = c("Ca","PO4", "PTH", "D3", "FGF23")
      )
    } 
  })
  
  # Disable the private section
  observe({
    shinyjs::hide("prettystuff")
  })
  
  # prevent user from unselecting all graph components
  # observeEvent(input$network_Ca_choice, {
  #   
  #   if (is.element("PO4", input$network_Ca_choice) && 
  #       !is.element("Ca", input$network_Ca_choice)) {
  #     disable(selector = "#network_Ca_choice input[value='PO4']")
  #   } else {
  #     enable(selector = "#network_Ca_choice input[value='PO4']")
  #   }
  #   if (is.element("Ca", input$network_Ca_choice) && 
  #       !is.element("PO4", input$network_Ca_choice)) {
  #     disable(selector = "#network_Ca_choice input[value='Ca']")
  #   } else {
  #     enable(selector = "#network_Ca_choice input[value='Ca']")
  #   }
  # })
  
  # reset parameters individually
  button_states <- reactiveValues(values = list())
  
  observeEvent(c(input$reset_tmaxCainj,
                 input$reset_tmaxPO4inj,
                 input$reset_tmaxPO4gav),{
                   
                   # call the function to reset the given slider
                   sliders_reset(button_states, input)
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
        shinyjs::removeClass(
          selector = "body", 
          class = paste("skin", previous_skin$color, sep = "-"))
        shinyjs::addClass(
          selector = "body", 
          class = paste("skin", current_skin$color, sep = "-"))
        # the current skin is added to previous_skin to be ready for
        # the next change
        previous_skin$color <- c(previous_skin$color, current_skin$color)[-1]
      }
    }
  })
  
  # Custom footer
  #output$dynamicFooter <- renderFooter({ 
  #  generate_dynamicFooter()
  #})
  
})