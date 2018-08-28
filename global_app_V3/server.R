#-------------------------------------------------------------------------
#  This application is a R-Shiny implementation of a calcium and phosphate 
#  homeostasis model. It aims at being used by medical students but also
#  researchers. See https://divadnojnarg.github.io for more informations
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  #------------------------------------------------------------------------- 
  #  Store times, state and parameters in reactive values that can
  #  react to user inputs
  #  
  #-------------------------------------------------------------------------
  
  # Basic reactive expressions needed by the solver
  times <- reactive({ 
    # add this to avoid error in lsoda if tmax is < 0
    req(input$tmax > 0)
    seq(0,input$tmax, by = 1) 
  }) 
  
  t_target <- reactive({input$t_now})
  
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
  parameters_bis <- reactive({c(parameters(), parameters_fixed)})
  
  #------------------------------------------------------------------------- 
  #  
  #  Integrate equations using deSolve package to generate table
  #  out is a reactive intermediate component that is called by
  #  to make plots or other stuffs
  #
  #-------------------------------------------------------------------------
  
  # out <- reactive({
  #   
  #   parameters_bis <- parameters_bis()
  #   times <- times()
  #   
  #   as.data.frame(ode(y = state, 
  #                     times = times, 
  #                     func = calcium_phosphate_core, 
  #                     parms = parameters_bis))
  #   
  # })
  
  # use the compiled version of the model instead
  # uncomment the code above to use the classic version
  # and comment this one. Compiled code is at least 60 times faster...
  out <- reactive({
    parameters <- parameters_bis()
    times <- times()
    
    temp <- as.data.frame(
      ode(y = state,
          times = times,
          func = "derivs",
          parms = parameters,
          dllname = "compiled_core",
          initfunc = "initmod",
          nout = 33,
          outnames = c("U_Ca", "U_PO4", "Abs_int_Ca", 
                       "Abs_int_PO4", "Res_Ca", "Res_PO4", 
                       "Ac_Ca", "Ac_PO4", "Reabs_Ca", "Reabs_PO4", 
                       "Ca_pf", "Ca_fp", "PO4_pf", "PO4_fp",
                       "PO4_pc", "PO4_cp", "PTHg_synth", 
                       "PTHg_synth_D3", "PTHg_synth_PO4",
                       "PTHg_exo_CaSR", "PTHg_deg", "PTHg_exo", 
                       "PTHp_deg", "Reabs_PT_PTH",
                       "Reabs_TAL_CaSR", "Reabs_TAL_PTH", 
                       "Reabs_DCT_PTH", "Reabs_DCT_D3",
                       "Abs_int_D3", "Res_PTH", "Res_D3", 
                       "Reabs_PT_PO4_PTH", "Reabs_PT_PO4_FGF")))
    temp
  })
  
  output$table <- renderDataTable({ 
    out()[nrow(out()), 55:56]
  })
  
  # generate virtual patient initial conditions
  observe({
    out <- out()
    write.csv(x = c(out[nrow(out),2:23]), file = "init_hyperD3.csv")
  })
  
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
                Shiny.onInputChange('current_node_id', 'null');
                ;}") %>%
      # add the doubleclick function to handle zoom views
      visEvents(doubleClick = "function(nodes) {
                Shiny.onInputChange('current_node_bis_id', nodes.nodes);
                }") %>%  
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', edges.edges);
                ;}") %>%
      # unselect edge event
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', 'null');
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
  
  # Handle Firefox images
  # Firefox still fuck up when redrawing the network
  # even with pngs!!!
  observe({
    req(input$browser)
    if (str_detect(input$browser, "Firefox")) {
      nodes_Ca <- nodes_Ca()
      nodes_Ca$image <- c("intestine.png","plasma.png","rapid-bone.png",
                          "bone.png","kidney.png","kidney_zoom1.png","urine.png",
                          "cells.png","Cap.png","PO4.png","parathyroid_gland.png",
                          "PTH.png","D3.png","D3.png","D3.png","FGF23.png")
      
      visNetworkProxy("network_Ca") %>%
        visUpdateNodes(nodes = nodes_Ca)
    }
  })
  
  # change the selected node size to
  # better highlight it
  last <- reactiveValues(selected_node = NULL)
  
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
  
  
  output$browser <- renderPrint({input$browser})
  output$id <- renderPrint({input$current_edge_id})
  output$id_bis <- renderPrint({input$current_node_id})
  output$node_bis <- renderPrint({input$current_node_bis_id})
  
  #------------------------------------------------------------------------- 
  #  
  # 
  #   The zoom network part: make zoom network of PTHg network
  #  
  #
  #-------------------------------------------------------------------------
  
  nodes_PTHg <- reactive({generate_nodes_PTHg()})
  edges_PTHg <- reactive({generate_edges_PTHg()})
  
  output$network_PTH <- renderVisNetwork({
    
    nodes_PTHg <- nodes_PTHg()
    edges_PTHg <- edges_PTHg()
    
    generate_network(nodes = nodes_PTHg, edges = edges_PTHg) %>%
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_bis_id', edges.edges);
                ;}") %>%
      # set value to NULL to prevent sliders from being displayed
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_bis_id', 'null');
                ;}") %>% 
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:2.5, y:-2.5},
                offset: {x: 0, y:0} })}")
    })
  
  output$id_tris <- renderPrint({input$current_edge_bis_id})
  
  
  
  # node coordinates
  # useful when developing to return x and y position
  output$position <- renderPrint( vals$coords ) 
  vals <- reactiveValues(coords = NULL, viewposition = NULL,
                         connected_edges = NULL)
  observe({
    invalidateLater(1000)
    visNetworkProxy("network_kidney_PT_PO4") %>% visGetPositions()
    vals$coords <- if (!is.null(input$network_kidney_PT_PO4_positions)) 
      do.call(rbind, input$network_kidney_PT_PO4_positions)
  })
  
  # view position (of the camera)
  # useful to set a proper view
  output$viewposition <- renderPrint({vals$viewposition})
  observe({
    invalidateLater(1000)
    visNetworkProxy("network_kidney_PT_PO4") %>% visGetViewPosition()
    vals$viewposition <- if (!is.null(input$network_kidney_PT_PO4_viewPosition))
      do.call(rbind, input$network_kidney_PT_PO4_viewPosition)
  })
  
  # get the connected edges to the selected node 
  observe({
    invalidateLater(1000)
    req(input$current_node_id)
    selected_node <- input$current_node_id
    visNetworkProxy("network_Ca") %>%
      visGetConnectedEdges(id = selected_node)
    vals$connected_edges <- if (!is.null(input$network_Ca_connectedEdges)) 
      do.call(rbind, list(input$network_Ca_connectedEdges))
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  The zoom network part: nephron containing PT, TAL and DCT
  #
  #-------------------------------------------------------------------------
  
  # Generate the kidney_zoom2 Graph network
  
  nodes_kidney_zoom2 <- reactive({generate_nodes_kidney_zoom2()})
  edges_kidney_zoom2 <- reactive({generate_edges_kidney_zoom2()})
  
  # Generate the output of the kidney_zoom2 graph to be used in body
  
  output$network_kidney_zoom2 <- renderVisNetwork({
    
    nodes_kidney_zoom2 <- nodes_kidney_zoom2()
    edges_kidney_zoom2 <- edges_kidney_zoom2()
    
    generate_network(nodes = nodes_kidney_zoom2, edges = edges_kidney_zoom2) %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:-28.8, y:-2.34},
                offset: {x: 0, y:0} })}") %>%
      visEvents(doubleClick = "function(nodes) {
                Shiny.onInputChange('current_node_tris_id', nodes.nodes);
                }") %>%
      # unselect node event
      visEvents(deselectNode = "function(nodes) {
                Shiny.onInputChange('current_node_tris_id', 'null');
                ;}")
  })
  
  output$node_tris <- renderPrint({input$current_node_tris_id})
  
  #------------------------------------------------------------------------- 
  #  
  #  The zoom network part: make zoom network kidney_PT
  #  proximal tubule zoom
  #
  #-------------------------------------------------------------------------
  
  # ========= #
  #     Ca    #
  # ========= #
  
  
  # Generate the kidney_PT Graph network
  
  nodes_kidney_PT <- reactive({generate_nodes_kidney_PT()})
  edges_kidney_PT <- reactive({generate_edges_kidney_PT()})
  
  # Generate the output of the kidney_PT graph to be used in body
  
  output$network_kidney_PT <- renderVisNetwork({
    
    nodes_kidney_PT <- nodes_kidney_PT()
    edges_kidney_PT <- edges_kidney_PT()
    
    generate_network(nodes = nodes_kidney_PT, edges = edges_kidney_PT) %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:-28.8, y:-2.34},
                offset: {x: 0, y:0} })}") %>%
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_tris_id', edges.edges);
                ;}") %>%
      # set value to NULL to prevent sliders from being displayed
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_tris_id', 'null');
                ;}")
      })
  
  output$edges_id_PT <- renderPrint({input$current_edge_tris_id})
  
  # ========= #
  #    PO4    #
  # ========= #
  
  nodes_kidney_PT_PO4 <- reactive({generate_nodes_kidney_PT_PO4()})
  edges_kidney_PT_PO4 <- reactive({generate_edges_kidney_PT_PO4()})
  
  # Generate the output of the kidney_PT graph to be used in body
  
  output$network_kidney_PT_PO4 <- renderVisNetwork({
    
    nodes_kidney_PT_PO4 <- nodes_kidney_PT_PO4()
    edges_kidney_PT_PO4 <- edges_kidney_PT_PO4()
    
    generate_network(nodes = nodes_kidney_PT_PO4, edges = edges_kidney_PT_PO4) %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:-28.8, y:-2.34},
                offset: {x: 0, y:0} })}") %>%
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_PT_PO4_id', edges.edges);
                ;}") %>%
      # set value to NULL to prevent sliders from being displayed
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_PT_PO4_id', 'null');
                ;}")
    })
  
  output$edges_id_PT2 <- renderPrint({input$current_edge_PT_PO4_id})
  
  #------------------------------------------------------------------------- 
  #  
  #  The zoom network part: make zoom network kidney_TAL
  #  Thick ascending limb of Henle zoom
  #
  #-------------------------------------------------------------------------
  
  # Generate the kidney_TAL Graph network
  
  nodes_kidney_TAL <- reactive({generate_nodes_kidney_TAL()})
  edges_kidney_TAL <- reactive({generate_edges_kidney_TAL()})
  
  # Generate the output of the kidney_TAL graph to be used in body
  
  output$network_kidney_TAL <- renderVisNetwork({
    
    nodes_kidney_TAL <- nodes_kidney_TAL()
    edges_kidney_TAL <- edges_kidney_TAL()
    
    generate_network(nodes = nodes_kidney_TAL, edges = edges_kidney_TAL) %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:-13, y:-29},
                offset: {x: 0, y:0} })}") %>%
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_4_id', edges.edges);
                ;}") %>%
      # set value to NULL to prevent sliders from being displayed
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_4_id', 'null');
                ;}")
    })
  
  output$edges_id_TAL <- renderPrint({input$current_edge_4_id})
  
  
  #------------------------------------------------------------------------- 
  #  
  #  The zoom network part: make zoom network kidney_DCT
  #  Distal convoluted tubule zoom
  #
  #-------------------------------------------------------------------------
  
  # Generate the kidney_DCT Graph network
  
  nodes_kidney_DCT <- reactive({generate_nodes_kidney_DCT()})
  edges_kidney_DCT <- reactive({generate_edges_kidney_DCT()})
  
  # Generate the output of the kidney_DCT graph to be used in body
  
  output$network_kidney_DCT <- renderVisNetwork({
    
    nodes_kidney_DCT <- nodes_kidney_DCT()
    edges_kidney_DCT <- edges_kidney_DCT()
    
    generate_network(nodes = nodes_kidney_DCT, edges = edges_kidney_DCT) %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:-13, y:-29},
                offset: {x: 0, y:0} })}") %>%
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_5_id', edges.edges);
                ;}") %>%
      # set value to NULL to prevent sliders from being displayed
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_5_id', 'null');
                ;}")
    })
  
  output$edges_id_DCT <- renderPrint({input$current_edge_5_id})
  
  #------------------------------------------------------------------------- 
  #  
  #  The zoom network part: make zoom network Intestine
  #  Effect of D3 on the intestinal transport of Ca
  #
  #-------------------------------------------------------------------------
  
  # Generate the kidney_DCT Graph network
  
  nodes_intestine <- reactive({generate_nodes_intestine()})
  edges_intestine <- reactive({generate_edges_intestine()})
  
  # Generate the output of the kidney_DCT graph to be used in body
  
  output$network_intestine <- renderVisNetwork({
    
    nodes_intestine <- nodes_intestine()
    edges_intestine <- edges_intestine()
    
    generate_network(nodes = nodes_intestine, edges = edges_intestine) %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:97.5, y:201.0},
                offset: {x: 0, y:0} })}") %>%
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_6_id', edges.edges);
                ;}") %>%
      # set value to NULL to prevent sliders from being displayed
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_6_id', 'null');
                ;}")
  })
  
  output$edges_id_intestine <- renderPrint({input$current_edge_6_id})
  
  #------------------------------------------------------------------------- 
  #  
  #  The zoom network part: make zoom network Bone
  #  Effect of D3 and PTH on bone matabolism
  #
  #-------------------------------------------------------------------------
  
  # Generate the kidney_DCT Graph network
  
  nodes_bone <- reactive({generate_nodes_bone()})
  edges_bone <- reactive({generate_edges_bone()})
  
  # Generate the output of the kidney_DCT graph to be used in body
  
  output$network_bone <- renderVisNetwork({
    
    nodes_bone <- nodes_bone()
    edges_bone <- edges_bone()
    
    generate_network(nodes = nodes_bone, edges = edges_bone) %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", afterDrawing = "function() {
                this.moveTo({ position: {x:-7.1, y:27.8},
                offset: {x: 0, y:0} })}") %>%
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_7_id', edges.edges);
                ;}") %>%
      # set value to NULL to prevent sliders from being displayed
      visEvents(deselectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_7_id', 'null');
                ;}")
  })
  
  output$edges_id_bone <- renderPrint({input$current_edge_7_id})
  
  #------------------------------------------------------------------------- 
  #  
  #  The graph part: calls out(), parameters_bis()
  #  Interactive graph as a result of click on the diagram
  #
  #-------------------------------------------------------------------------
  
  
  # Generate a graph when node is clicked. The graph corresponds to the node clicked
  
  output$plot_node <- renderPlotly({
    # check if the current selected node is not NULL and not 'null'
    # 'null' is given by the javascript code when the node is deselected
    validate(need(
      !identical(input$current_node_id, "null") & !is.null(input$current_node_id), 
      "Select one node on the graph!")
    )
    out <- out()
    parameters_bis <- parameters_bis()
    plot_node(node = input$current_node_id , out, parameters_bis)
  })
  
  output$plot_edge <- renderPlotly({
    # check if the current selected edge is not NULL and not 'null'
    # 'null' is given by the javascript code when the edge is deselected
    validate(need(
      !identical(input$current_edge_id, "null") & !is.null(input$current_edge_id), 
      "Select one edge on the graph!")
    )
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
  # # calculate the base case value of change and let it isolate
  #   as.data.frame(calc_change(out())) 
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
  
  observe({
    
    events <- c(input$Lambda_ac_Ca,
                input$k_p_Ca,input$k_p_P,
                input$k_f_Ca, input$k_f_P,
                input$Lambda_res_min,
                input$delta_res_max,
                input$GFR, input$k_pc,
                input$k_cp, input$k_prod_PTHg,
                input$D3_inact, input$k_deg_D3,
                input$k_prod_FGF)
    
    events_PTH <- c(input$k_prod_PTHg,
                    input$beta_exo_PTHg,
                    input$gamma_exo_PTHg,
                    input$D3_inact,
                    input$k_deg_D3)
    
    # events in the proximal tubule depend on PTH
    events_kidney_PT <- input$k_prod_PTHg
    
    # events in the proximal tubule for PO4: PTH and FGF23
    events_kidney_PT_PO4 <- c(input$k_prod_PTHg,
                              input$k_prod_FGF)
    
    # events in the TAL
    events_kidney_TAL <- input$k_prod_PTHg
    
    # events in the DCT function of PTH and D3
    events_kidney_DCT <- c(input$k_prod_PTHg,
                           input$D3_inact,
                           input$k_deg_D3)
    
    events_intestine <- c(input$D3_inact,
                          input$k_deg_D3)
    
    events_bone <- c(input$k_prod_PTHg,
                     input$D3_inact,
                     input$k_deg_D3)
    
    out <- out()
    edges_Ca <- edges_Ca()
    edges_PTHg <- edges_PTHg()
    edges_kidney_PT <- edges_kidney_PT()
    edges_kidney_PT_PO4 <- edges_kidney_PT_PO4()
    edges_kidney_TAL <- edges_kidney_TAL()
    edges_kidney_DCT <- edges_kidney_DCT()
    edges_intestine <- edges_intestine()
    edges_bone <- edges_bone()
    
    # create the list of arguments needed by pmap
    network_list <- list(
      edges = list(edges_Ca, edges_PTHg, edges_kidney_PT,
                   edges_kidney_PT_PO4, edges_kidney_TAL,
                   edges_kidney_DCT, edges_intestine, edges_bone),
      
      network = list("network_Ca", "network_PTH", "network_kidney_PT",
                     "network_kidney_PT_PO4", "network_kidney_TAL",
                     "network_kidney_DCT", "network_intestine",
                     "network_bone"),
      
      events = list(events, events_PTH, events_kidney_PT,
                    events_kidney_PT_PO4, events_kidney_TAL,
                    events_kidney_DCT, events_intestine, events_bone))
    
    # apply flux_lighting for all networks using pmap function
    pmap(network_list, flux_lighting, out, t_target())
    
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
  #  Toggle the sidebar when a user select a parameter
  #  (which is of course in the model core)
  #
  #-------------------------------------------------------------------------
  
  # Be careful: currently, current_node_id is never reset via js code
  # contrary to current_edge_bis_id which is null when the arrow is
  # unselected (see the network part above to change this behaviour)
  observe({
    if (is.null(input$current_edge_bis_id)) {
      # the only time current_node_id is NULL is at the begining
      # then when the user selects a value, it is not NULL anymore (see network)
      if (is.null(input$current_node_id)) {
        # if no arrow is selected as well as no nodes, 
        # remove the "control-sidebar-open" class so that the 
        # control sidebar does not show
        shinyjs::removeClass(id = "controlbar", class = "control-sidebar-open")
      } else {
        # if a node is selected and contains parameters, display the controlbar
        shinyjs::toggleClass(id = "controlbar", class = "control-sidebar-open",
                             condition = input$current_node_id %in% c(1,3,4,8,13,14,15,16) & 
                               !is.null(input$current_node_id))
      } 
    } else if (input$current_edge_bis_id == "null") {
      # this is a special case. Javascript uses "null" instead of NULL in R.
      # This part of the code ensures that when an arrow has been selected and
      # unselected, which ultimately sets current_edge_bis_id to null (not NULL), 
      # node selection can still trigger hide/show of the controlbar
      shinyjs::toggleClass(id = "controlbar", class = "control-sidebar-open",
                           condition = input$current_node_id %in% c(1,3,4,8,13,14,15,16) & 
                             !is.null(input$current_node_id))
    } else {
      # then if an arrow containing parameters is selected, show the 
      # controlbar, not matter the value of node id
      shinyjs::toggleClass(id = "controlbar", class = "control-sidebar-open",
                           condition = input$current_edge_bis_id %in% c(1,3,4) & 
                             !is.null(input$current_edge_bis_id))
    }
  })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Add notification in the header when a parameter was changed
  #  as well as how much parameters are changed. To remind the user...
  #
  #-------------------------------------------------------------------------
  
  output$parameter_changed <- renderMenu({
    param_notif <- find_parameter_change(parameters())
    
    # if notif is not NULL, we can fill the notification dropdown menu
    if (!is.null(param_notif)) {
      items <- lapply(1:length(param_notif$value), function(i) {
        notificationItem(status = param_notif$color[i],
                         text = param_notif$text[i])
      })
      dropdownMenu(
        type = "notifications", badgeStatus = "warning",
        .list = items
      )
    } else {
      # req ensure that no error is displayed when notif is empty since
      # remove UI need at least one element to work
      req(param_notif)
      # remove the corresponding id
      removeUI(selector = "parameter_changed")
    }
  })

  # try to make notif collapse/uncollapse. Seems like shinyjs does not
  # work even if good ids are provided
  # observe({
  #   param_notif <- find_parameter_change(parameters())
  #   #req(param_notif)
  #   if (length(param_notif) > 0) {
  #     shinyjs::removeClass(id = "parameter_changed", class = "notifications-menu")
  #     shinyjs::addClass(id = "parameter_changed", class = "notifications-menu open") # parameter_changed
  #   } else {
  #     shinyjs::removeClass(id = "parameter_changed", class = "notifications-menu open")
  #     shinyjs::addClass(id = "parameter_changed", class = "notifications-menu")
  #   }
  # })
  
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
  #  Handle dangerous parameter values by the user
  #  
  #-------------------------------------------------------------------------
  
  observeEvent(input$beta_exo_PTHg,{# critical value for PTHg
    
    if (input$beta_exo_PTHg < 0.9) {
      
      sendSweetAlert(session, 
                     title = "Ooops ...", 
                     text = "Invalid parameter value", 
                     type = "error")
      reset("beta_exo_PTHg") # value is reset
    }
  })
  
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