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
      eval(parse(text = paste("make_plot_", current_sim, "(input)", sep = "")))    
    }
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Patient profile 
  #
  #-------------------------------------------------------------------------
  
  # generate a patient profile
  create_userInfo <- reactive({
    
    head_user <- dashboardUser(
      name = "Patient State",
      image = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
        generate_userFields(input)$image
      } else {
        "happy.png"
      },
      description = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
        generate_userFields(input)$description
      } else {
        "healthy"
      },
      sub_description = if (input$run_php1) {
        "Patient has primary-hyperparathyroidism"
      } else if (input$run_hypopara) {
        "Patient suffers from hypoparathyroidism"
      } else if (input$run_hypoD3) {
        "Patient has vitamin D3 defficiency"
      } else {
        "nothing to declare!"
      }, 
      stat1 = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
        generate_userFields(input)$stat1
      } else {
        HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
      },
      stat2 = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
        generate_userFields(input)$stat2
      } else {
        HTML(paste(withMathJax(p("$$[P_i]_p$$ 1.5 mM")), "<br/>", "(0.8-1.6 mM)"))
      },
      stat3 = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
        generate_userFields(input)$stat3
      } else {
        HTML(paste(withMathJax(p("$$[PTH]_p$$ 66 ng/l")), "<br/>", "(20-70 ng/l)"))
      },
      stat4 = NULL
    )
    
    return(list(head_user = head_user))
  })
  
  output$user <- renderUser({create_userInfo() %$% head_user})
  
  output$userbttn1 <- renderUI({
    if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
      description <- generate_userFields(input)$description
      if (description == "dead") {
        tagList(
          actionBttn(inputId = "cure", label = "Change Patient", 
                     style = "fill", color = "success")
        )
      } else {
        tagList(
          actionBttn(inputId = "cure", label = "Cure Patient", 
                     style = "fill", color = "success")
        )
      }
    } else {
      tagList(
        actionBttn(inputId = "cure", label = "Cure Patient", 
                   style = "fill", color = "success")
      )
    }
  })

  
  #------------------------------------------------------------------------- 
  #  
  # Generate sliders for php1, hypopara and hypoD3
  #  
  #-------------------------------------------------------------------------
  
  output$slider <- renderUI({
    current_sim <- extract_running_sim(input)[[1]] %>%
      str_extract("_\\w+") %>%
      str_replace("_", "")
    
    if (input$run_php1 | input$run_hypopara | input$run_hypoD3 | input$help) {
      
      sliderTextInput(
        inputId = ifelse(input$help, "slider_help",
                         paste("slider_", current_sim, sep = "")), 
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
  # options are provided to control the size of the help
  # Do not forget to wrap the event content in I('my_function')
  # otherwise it will fail
  observeEvent(input$help,{
    introjs(session, 
            options = list("nextLabel" = "Next step!",
                           "prevLabel" = "Did you forget something?",
                           "tooltipClass" = "newClass",
                           #"highlightClass" = "newClass",
                           "showProgress" = TRUE,
                           "showBullets" = FALSE),
            events = list(# reset the session to hide sliders and back/next buttons
              "oncomplete" = I('history.go(0)'),
              "onbeforchange" = I("function(steps) {
                Shiny.onInputChange('current_step', steps.steps);
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
  
  output$test <- renderPrint(input$current_step)
  
  # Print a short help text above the graph part
  # removeUI does not work
  output$info <- renderUI({
    if (is_empty(input$php1) | is_empty(input$hypopara) | is_empty(input$hypoD3) |
        is_empty(input$Ca_inject) | is_empty(input$PO4_inject) | is_empty(input$PO4_gav)) {
      HTML(paste("To print me, select a case study.", 
                 "They can be choosen in the", icon("map-o fa-2x"), 
                 "section, in the", "<mark><font color=\"#FF0000\"><b>", 
                 "right sidebar.", "</b></font></mark>", sep = " "))
    } else {
      removeUI(selector = "info")
    }
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Modal for primary hyperparathyroidism, hypopara, ...
  #  gives the user some extr information
  #
  #-------------------------------------------------------------------------
  
  observeEvent(c(input$run_php1, input$run_hypopara, input$run_hypoD3,
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
                     # make it draggable
                     jqui_draggable(selector = "#shiny-notification-notifid")
                     jqui_draggable(selector = "#shiny-notification-graph_notif")
                   }
                 })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Toggle the sidebar when a user press the help button
  #
  #-------------------------------------------------------------------------
  
  
  observe({
    shinyjs::toggleClass(id = "controlbar", 
                         class = "control-sidebar-open",
                         condition = input$help)
  })
  
  observe({
    shinyjs::toggleClass(id = "user",
                         class = "user-menu open",
                         condition = input$help)
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Useful tasks such as save, reset, load ...
  #  
  #-------------------------------------------------------------------------
  
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
        div(style = "display: inline",
            div("2017-2018, the Interface Group", style = "display: inline",
                a(href = "http://interfacegroup.ch/people/", target = "_blank",
                  img(src = "interface_logo.png", height = "30px")
                )),
            HTML("<span id=\"tab\"></span>"),
            div("Built with", style = "display: inline",
                a(href = "https://shiny.rstudio.com", target = "_blank",
                  img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", 
                      height = "30px")
                ),
                "by",
                a(href = "http://www.rstudio.com", target = "_blank",
                  img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                      height = "30px"))),
            HTML("<span id=\"tab\"></span>"),
            div("Funded by", style = "display: inline",
                a(href = "http://www.nccr-kidney.ch", target = "_blank", 
                  img(src = "nccr_logo.png", height = "50px")),
                a(href = "http://www.uzh.ch/de.html", target = "_blank", 
                  img(src = "uzh_logo.png", height = "30px")),
                "and",
                a(href = "https://www.unil.ch/fbm/fr/home.html", target = "_blank",
                  img(src = "unil_logo.png", height = "55px")
                ))
        )), 
      subText = HTML("<b>Version:</b> Beta 3")
    ) 
  })
  
})