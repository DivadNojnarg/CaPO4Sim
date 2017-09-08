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
  
  nodes_Ca <- reactive({
    
    d <- data.frame(
      id = 1:21,
      shape = c("image","image","image","image","image","image","image","image",
                "circle","circle","circle","image","image","image","image","image",
                "image","image","image","image","image"), 
      image = c("food.svg","intestine.svg","feces.svg","plasma.svg","rapid-bone.svg",
                "bone.svg","kidney.svg","kidney_zoom1.svg", rep("",3),"urine.svg",
                "cells.svg","Cap.svg","PO4.svg","parathyroid_gland.svg","PTH.svg",
                "D3.svg","D3.svg","D3.svg","FGF23.svg"),
      label = c(rep("", 8),"PT","TAL","DCT", rep("",10)),
      group = c(rep("without hormones",13),rep("only hormones",8)),
      fixed = list("x" = TRUE, "y" = TRUE),
      title = c("", 
                paste(a("About intestinal Ca absorption", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23937&Menu=1079&backbar=0",
                        target="_blank"),br(),
                      a("About intestinal PO4 absorption", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23408&Menu=1079&backbar=0)",
                        target="_blank")),
                rep("",2),
                paste(a("About rapid bone pool", 
                        href = "https://academic.oup.com/ndt/article/26/8/2438/1917340/The-exchangeable-calcium-pool-physiology-and",
                        target="_blank")),
                paste(a("About bone", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=24035&Menu=1079&backbar=0",
                        target="_blank")),
                paste(a("About Ca kidney handling", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23415&Menu=1079&backbar=0",
                        target="_blank"), br(),
                      a("About PO4 kidney handling", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23410&Menu=1079&backbar=0",
                        target="_blank")),
                "",
                paste(a("About PTH", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23466&Menu=1079&backbar=0", 
                        target="_blank")),
                paste(a("About vitamin D3", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23484&Menu=1079&backbar=0",
                        target="_blank")),
                paste(a("About vitamin D3", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23484&Menu=1079&backbar=0",
                        target="_blank")),
                paste(a("About FGF23", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23408&Menu=1079&backbar=0",
                        target="_blank")),
                rep("",1),
                paste(a("About Calcium", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=18891&Menu=1079&backbar=0",
                        target="_blank")),
                paste(a("About Phosphate", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=18893&Menu=1079&backbar=0",
                        target="_blank")),
                rep("",6)), # tooltip to display an image
      x = c(3,38,64,-65,-65,-256,180,360,380,450,430,170,-190,290,320,41,-418,330,385,-386,481),
      y = c(-633,-150,551,195,472,460,0,230,110,250,130,506,0,-317,-633,-452,240,-452,0,-106,-452),
      color = list(background = "#97C2FC", border = "#97C2FC", 
                   highlight = list(background = "orange", border = "orange")),
      size = c(rep(60,7), 150, rep(60,5), rep(25,2), 60, rep(25,5)),
      #fixed = list("x" = TRUE, "y" = TRUE),
      physics = rep(FALSE,21),
      hidden = c(rep(FALSE,21)))
    
  })
  
  edges_Ca <- reactive({
    
    d <- data.frame(
      from = c(1,rep(2,2),rep(4,2),rep(5,2),rep(5,2),6,4,rep(7,2),rep(7,2),4,13,
               rep(14,3),rep(15,3),rep(16,2),17,rep(18,2),rep(19,2),rep(20,2),
               rep(21,2)), 
      to = c(2,4,3,rep(5,2),rep(4,2),rep(6,2),4,7,rep(4,2),rep(12,2),13,4,16,7,
             18,16,18,21,7,18,6,16,21,19,7,6,2,18,7),
      #label = c(rep("",17),),
      id = 1:34,
      width = c(rep(8,17), rep(4,17)),
      font.size = 11,
      #font.align = c(rep("",10),rep("",2),"bottom","top","","bottom",
      #               rep("",5),"bottom","","top","bottom","bottom",rep("",8)),
      color = list(color = c(rep("black", 34)), 
                   highlight = "yellow"),
      dashes = c(rep(FALSE,17), rep(TRUE,17)),
      title = c(rep("",5),
                paste(a("About bone formation", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=24281&Menu=1079&backbar=0",
                        target="_blank")),
                paste(a("About bone resorption", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=25370&Menu=1079&backbar=0",
                        target="_blank")),
                rep("",14),
                paste(a("About the Calcium Sensing Receptor", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23415&Menu=1079&backbar=0",
                        target="_blank")),
                rep("",4),
                paste(a("About the Calcium Sensing Receptor", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23415&Menu=1079&backbar=0",
                        target="_blank")),
                rep("",6),
                paste(a("About bone formation", 
                        href = "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=24281&Menu=1079&backbar=0",
                        target="_blank"))),
      smooth = c(rep(TRUE,34)),
      length = c(rep(200,3),rep(300,4),rep(300,2),200,300,200,300,300,300,300,300,rep(200,17)),
      # to show either Ca or PO4 or CaPO4 network arrows
      hidden = c(rep(FALSE,3), 
                 ifelse(is.element("Ca", input$network_Ca_choice), 
                        ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE), 
                 ifelse(is.element("Ca", input$network_Ca_choice), 
                        ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 ifelse(is.element("Ca", input$network_Ca_choice), 
                        ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 FALSE,
                 ifelse(is.element("Ca", input$network_Ca_choice), 
                        ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 FALSE,
                 ifelse(is.element("Ca", input$network_Ca_choice), 
                        ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 rep(FALSE,8),
                 ifelse(is.element("PO4", input$network_Ca_choice), 
                        ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 ifelse(is.element("PO4", input$network_Ca_choice), 
                        ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 ifelse(is.element("PO4", input$network_Ca_choice), 
                        ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 rep(FALSE,9),
                 ifelse(is.element("PO4", input$network_Ca_choice), 
                        ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 ifelse(is.element("PO4", input$network_Ca_choice), 
                        ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 ifelse(is.element("PO4", input$network_Ca_choice), 
                        ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
                 ifelse(is.element("PO4", input$network_Ca_choice), 
                        ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE)), 
      stringsAsFactors=FALSE) 
    # to change edges color do not forget this "stringsAsFactors=FALSE"
  })
  
  # Generate the output of the Ca graph to be used in body
  
  output$network_Ca <- renderVisNetwork({
    
    nodes_Ca <- nodes_Ca()
    edges_Ca <- edges_Ca()
    
    legend_nodes <- data.frame(
      shape = c("image","image"),
      image = c("food.svg","D3.svg"),
      label = c("compartment", "hormones"),
      size = c(15,15))
    
    legend_edges <- data.frame(
      from = c(5,1,15,9), 
      to = c(4,2,12,7),
      width = 4,
      arrows = "to",
      label = c("inhibited flux", "stimulated flux", 
                "hormonal regulation","perturbation"),
      color = list(color = c("red","green","black","yellow")),
      font.align = "bottom",
      dashes = c(F,F,T,F),
      smooth = c(T,T,T,F))
    
    visNetwork(nodes_Ca, 
               edges_Ca, 
               width = "100%", 
               height = "100%") %>%
      visNodes(shapeProperties = list(useBorderWithImage = FALSE)) %>%
      # put shadow on false
      visEdges(shadow = FALSE, 
               font = list(align = "horizontal"), 
               arrows = list(to = list(enabled = TRUE, 
                                       scaleFactor = 1, 
                                       type = "arrow"))) %>%
      # add group selection option
      visOptions(highlightNearest = FALSE, 
                 clickToUse = FALSE, 
                 manipulation = FALSE, 
                 selectedBy = "group", 
                 collapse = FALSE) %>% 
      # prevent edge from being selected when a node is selected
      visInteraction(hover = TRUE, 
                     hoverConnectedEdges = FALSE, 
                     selectConnectedEdges = FALSE, 
                     dragNodes = TRUE, 
                     dragView = FALSE, 
                     zoomView = FALSE,
                     navigationButtons = FALSE) %>% 
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
            this.moveTo({scale:2})}") %>% # to set the initial zoom (1 by default)
      #visIgraphLayout(smooth = TRUE) %>% # to disable repulsion
      # add the legend, ncol = 2 to show edges otherwise a bug appear
      #visLegend(addNodes = legend_nodes, addEdges = legend_edges, 
      #          useGroup = FALSE, ncol = 2, width = 0.1) %>% 
      # stabilization prevents arrows from bouncing
      visPhysics(stabilization = TRUE, enabled = TRUE) %>%
      visExport(type = "pdf", style = css_export) # export the graph as pdf
    
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
  
  nodes_PTHg <- reactive({
    
    d <- data.frame(
      id = 1:5,
      shape = c("circle","circle","circle","circle","circle"), 
      label = c("","PTHg","","",""),
      x = c(-50,81,51,184,109), 
      y = c(-6,-6,119,-106,-165), 
      color = list(background = "#97C2FC", border = "#97C2FC", 
                   highlight = list(background = "orange", border = "orange")),
      size = c(10,10,10,10,10), 
      #fixed = list("x" = TRUE, "y" = TRUE),
      hidden = c(TRUE,FALSE,TRUE,TRUE,TRUE))
    
  })
  
  edges_PTHg <- reactive({
    
    d <- data.frame(
      from = c(1,2,2,5), 
      to = c(2,3,4,4),
      label = c("PTHg synthesis", "PTHg degradation", 
                "PTHg exocytosis", "CaSR inhibition"),
      id = 1:4,
      width = 4,
      font.size = 12,
      color = list(color = c(rep("black", 4)), 
                   highlight = "yellow"),
      dashes = c(rep(FALSE,3),TRUE),
      smooth = c(rep(TRUE,4)),
      hidden = rep(FALSE,4),
      stringsAsFactors=FALSE)
    
  })
  
  output$network_PTH <- renderVisNetwork({
    
    nodes_PTHg <- nodes_PTHg()
    edges_PTHg <- edges_PTHg()
    
    visNetwork(nodes_PTHg, edges_PTHg) %>%
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
                offset: {x: 0, y:0} })}") %>% 
      visEdges(shadow = FALSE, 
               font = list(align = "horizontal"), # put shadow on false
               arrows = list(to = list(enabled = TRUE, 
                                       scaleFactor = 1, 
                                       type = "arrow"))) %>%
      visInteraction(hover = TRUE, 
                     hoverConnectedEdges = FALSE, 
                     selectConnectedEdges = FALSE, 
                     multiselect = TRUE, 
                     zoomView = FALSE, 
                     dragNodes = FALSE, 
                     dragView = FALSE) %>%
      visOptions(highlightNearest = FALSE, 
                 clickToUse = FALSE, 
                 manipulation = FALSE) %>%
      visExport(type = "pdf", style = css_export_zoom) # export the graph as pdf
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

  observe({ 
    
    events <- c(input$I_Ca,input$I_P,
                input$Lambda_ac_Ca,
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
                    input$gamma_exo_PTHg)
    
    
    out <- out()
    edges_Ca <- edges_Ca()
    edges_PTHg <- edges_PTHg()
    
    # for Ca/PO4 fluxes, call the flux_lighting() function
    flux_lighting(edges_Ca, network = "network_Ca", out, events = events)
    # for the PTH network
    flux_lighting(edges_PTHg, network = "network_PTH", out, events = events_PTH)
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Notification events to explain the user how to play with the app
  #
  #-------------------------------------------------------------------------
  
  # Show a welcome notification in the menu bar part
  
  observe({ 
    
    if (input$notif_switch == "TRUE") {
      
      help_text_generator()
      
    } else {
      
      # need the session argument!!!
      help_text_destructor(session)
      
    }
    
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
  
  # reset all the values of box inputs as well as graphs
  
  observeEvent(input$resetAll,{
    
    reset("boxinput")
    reset("tmax")
    
    edges_Ca <- edges_Ca()
    edges_PTHg <- edges_PTHg()
    
    graphs_reset(network = "network_Ca", edges = edges_Ca)
    graphs_reset(network = "network_PTH", edges = edges_PTHg)
    
  })
  
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
                 input$resetkcp),{
                   
                   # call the function to reset the given slider
                   sliders_reset(button_states, input)
                   
                 })
  
  # Share the state of the App via server bookmarking
  observeEvent(input$bookmark, {
    session$doBookmark()
  })
  
})