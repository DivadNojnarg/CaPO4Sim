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
      times <- seq(0,input$tmax,by = 1)
      out <- as.data.frame(ode(y = state, 
                               times = times, 
                               func = calcium_phosphate_Caiv, 
                               parms = parameters))
    } else if (input$run_PO4_inject == "TRUE") { # PO4 injection 
      times <- seq(0,input$tmaxbis,by = 1) 
      out <- as.data.frame(ode(y = state, 
                               times = times, 
                               func = calcium_phosphate_PO4iv, 
                               parms = parameters))
    } else if (input$run_PO4_gav == "TRUE") { # PO4 gavage
      times <- seq(0,input$tmaxtris,by = 1)
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

  observeEvent(input$back1,{# counter decrease
    if (counter_nav$diagram == 0) {
    } else {counter_nav$diagram <- counter_nav$diagram - 1}
  })

  observeEvent(input$next1,{# counter incrementation
      counter_nav$diagram <- counter_nav$diagram + 1
  })

  
  observeEvent(input$next1,{# reset the counter if higher than 5
    if (counter_nav$diagram > 6) {
      counter_nav$diagram <- 0
      edges_Ca <- edges_Ca()
      edges_Ca$color <- "black"
      edges_Ca$witdh <- 4
      visNetworkProxy("network_Ca", session) %>%  # then reset the graph
        visUpdateEdges(edges = edges_Ca)
    }
  })
  
  output$counter_nav <- renderPrint({counter_nav$diagram}) 
  
  # #------------------------------------------------------------------------- 
  # #  
  # #  Events for the CaPO4 Homeostasis diagramm during php1
  # #  
  # #-------------------------------------------------------------------------
  
  # 
  # # Events when php1 is selected
  # 
  # observeEvent(input$next1 | input$back1,{ # primary perturbation
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 1 && input$run_php1 == "TRUE") { # need && input$run_php1 == "TRUE" since run_Ca_inject also use back and next buttons
  #   
  #   edges_Ca$color.color[c(11,12,15)] <- "yellow" # perturbation
  #   edges_Ca$width[c(11,12,15)] <- 8
  #   
  #   visNetworkProxy("network_Ca", session) %>%  
  #     visUpdateEdges(edges = edges_Ca)
  #   
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 2 && input$run_php1 == "TRUE") { # secondary hormonal regulation (vitamin D3)
  #     
  #     edges_Ca$color.color[c(11,12,15)] <- "khaki" # previous arrows are less bright
  #     # edges_Ca$color.opacity[c(11,12,15)] <- 0
  #     
  #     edges_Ca$color.color[c(13,14,16,17)] <- "yellow"
  #     edges_Ca$width[c(13,14,16,17)] <- 8
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # FGF23 hormonal regulation
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 3 && input$run_php1 == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(18,19)] <- "yellow"
  #     edges_Ca$width[c(18,19)] <- 8
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # Fluxes are colored
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 4 && input$run_php1 == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(3,4,5,6,7,8,10,29)] <- "green" # increased fluxes
  #     edges_Ca$color.color[c(21,22,23,30,31,32)] <- "red" # decreased fluxesd
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # Ca and PO4 final retrocontrol
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 5 && input$run_php1 == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(3,4,5,6,7,8,10,29)] <- "green" # increased fluxes
  #     edges_Ca$color.color[c(21,22,23,30,31,32)] <- "red" # decreased fluxesd
  #     
  #     edges_Ca$color.color[c(20,24,25,26,27,28)] <- "yellow"
  #     edges_Ca$width[c(20,24,25)] <- 8 # Ca
  #     edges_Ca$width[c(26,27,28)] <- 2 # PO4
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$back1,{ # reset diagram when back is clicked
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram == 0 && input$run_php1 == "TRUE") { # reset if counter_diagram is 0
  #     
  #     edges_Ca$color <- "black"
  #     edges_Ca$witdh <- 4
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # #------------------------------------------------------------------------- 
  # #  
  # #  Events for the CaPO4 Homeostasis diagramm during hypoparathyroidism
  # #  
  # #-------------------------------------------------------------------------
  # 
  # # Events when hypopara is selected: basically it is the reverse case of php1
  # 
  # observeEvent(input$next1 | input$back1,{ # primary perturbation
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 1 && input$run_hypopara == "TRUE") { # need && input$run_hypopara == "TRUE" since run_Ca_inject also use back and next buttons
  #     
  #     edges_Ca$color.color[c(11,12,15)] <- "yellow" # perturbation
  #     edges_Ca$width[c(11,12,15)] <- 2 # decreased size compared to php1
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 2 && input$run_hypopara == "TRUE") { # secondary hormonal regulation (vitamin D3)
  #     
  #     edges_Ca$color.color[c(11,12,15)] <- "khaki" # previous arrows are less bright
  #     # edges_Ca$color.opacity[c(11,12,15)] <- 0
  #     
  #     edges_Ca$color.color[c(13,14,16,17)] <- "yellow"
  #     edges_Ca$width[c(13,14,16,17)] <- 2
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # FGF23 hormonal regulation
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 3 && input$run_hypopara == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(18,19)] <- "yellow"
  #     edges_Ca$width[c(18,19)] <- 2
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # Fluxes are colored
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 4 && input$run_hypopara == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(21,22,23,30,31,32)] <- "green"
  #     edges_Ca$color.color[c(3,4,5,6,7,8,10,29)] <- "red"
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # Ca and PO4 final retrocontrol
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 5 && input$run_hypopara == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(21,22,23,30,31,32)] <- "green"
  #     edges_Ca$color.color[c(3,4,5,6,7,8,10,29)] <- "red"
  #     
  #     edges_Ca$color.color[c(20,24,25,26,27,28)] <- "yellow"
  #     edges_Ca$width[c(20,24,25)] <- 2 # Ca
  #     edges_Ca$width[c(26,27,28)] <- 8 # PO4
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$back1,{ # reset diagram
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram == 0 && input$run_hypopara == "TRUE") { # reset if counter_diagram is 0
  #     
  #     edges_Ca$color <- "black"
  #     edges_Ca$witdh <- 4
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # #------------------------------------------------------------------------- 
  # #  
  # #  Events for the CaPO4 Homeostasis diagramm during vitamin D3 deficiency
  # #  
  # #-------------------------------------------------------------------------
  # 
  # # Events when hypoD3 is selected
  # 
  # observeEvent(input$next1 | input$back1,{
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 1 && input$run_hypoD3 == "TRUE") { # primary perturbation
  #     
  #     edges_Ca$color.color[c(13,14,16,17)] <- "yellow"
  #     edges_Ca$width[c(13,14,16,17)] <- 2
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # PTH
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 2 && input$run_hypoD3 == "TRUE") { # need && input$run_hypopara == "TRUE" since run_Ca_inject also use back and next buttons
  #     
  #     edges_Ca$color.color[c(13,14,16,17)] <- "khaki"
  #     edges_Ca$color.color[c(11,12,15)] <- "yellow" # perturbation
  #     edges_Ca$width[c(11,12,15)] <- 8 # decreased size compared to php1
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # 
  # 
  # observeEvent(input$next1 | input$back1,{ # FGF23 hormonal regulation
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 3 && input$run_hypoD3 == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(18,19)] <- "yellow"
  #     edges_Ca$width[c(18,19)] <- 2
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # Fluxes are colored
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 4 && input$run_hypoD3 == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(3,4,5,6,7,8,10,21,22,23,29,30,31,32)] <- "red"
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$next1 | input$back1,{ # Ca and PO4 final retrocontrol
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram >= 5 && input$run_hypoD3 == "TRUE") { # secondary hormonal regulation
  #     
  #     edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
  #     
  #     edges_Ca$color.color[c(3,4,5,6,7,8,10,21,22,23,29,30,31,32)] <- "red"
  #     
  #     edges_Ca$color.color[c(20,24,25,26,27,28)] <- "yellow"
  #     edges_Ca$width[c(20,24,25)] <- 2 # Ca
  #     edges_Ca$width[c(26,27,28)] <- 2 # PO4
  #     
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$back1,{ # reset diagram
  #   
  #   counter_nav$diagram
  #   
  #   edges_Ca <- edges_Ca()
  #   
  #   if (counter_nav$diagram == 0 && input$run_hypoD3 == "TRUE") { # reset if counter_diagram is 0
  #     
  #     edges_Ca$color <- "black"
  #     edges_Ca$witdh <- 4
  #     visNetworkProxy("network_Ca", session) %>%  
  #       visUpdateEdges(edges = edges_Ca)
  #     
  #   }
  #   
  # })
  
  #------------------------------------------------------------------------- 
  #  
  #  Events for the CaPO4 Homeostasis diagramm whenever a flux change
  #  
  #-------------------------------------------------------------------------
  
  # Change arrow color relatively to the value of fluxes for Ca injection/PO4 injection as well as PO4 gavage
  
  # observe({
  #   
  #   out <- out()
  #   edges_Ca <- edges_Ca()
  #   
  #   calc_change_t <- calc_change(out)
  #   calc_change_t$X <- NULL # remove column X
  #   
  #   # calculate the difference between live fluxes and base-case values
  #   index <- c(3,10,29,7,6,32,8,23,4,31,5,30,22,21) # index of arrows in the graph (which are fluxes and not regulations)
  #   calc_change_t <- rbind(calc_change_t, index)
  #   
  #   flux_changed_index <- which(calc_change_t[1,] != 0) # calculate which element in the sum table is different of 0 and store the index
  #   arrow_index <- as.numeric(t(calc_change_t[2,flux_changed_index])) # convert to arrow index in the interactive diagramm
  #   
  #   if (!is.null(flux_changed_index)) {
  #     for (i in (1:ncol(calc_change_t))) {
  #       arrow_index_i <- arrow_index[i] # change edge color according to an increase or decrease of the flux
  #       ifelse(calc_change_t[[i]][1] > 0, 
  #              edges_Ca$color.color[arrow_index_i] <- "green", 
  #              edges_Ca$color.color[arrow_index_i] <- "red")
  #     }
  #     
  #   }
  #   
  #   if (input$run_PO4_gav) { # PO4 gavage
  #     
  #     edges_Ca$color.color[1] <- "yellow" # perturbation
  #     edges_Ca$width[1] <- 8
  #     
  #   }
  #   if (input$run_Ca_inject) {
  #     if (input$tmax<60) { # Ca infusion
  #       
  #       edges_Ca$color.color[c(20,24,25,33)] <- "yellow" # perturbation
  #       edges_Ca$width[c(20,24,25,33)] <- 8
  #       
  #     } else { # EGTA infusion
  #       
  #       edges_Ca$color.color[c(20,24,25,35)] <- "yellow" # perturbation
  #       edges_Ca$width[c(20,24,25)] <- 2
  #       edges_Ca$width[35] <- 8
  #       
  #     }
  #   }
  #   if (input$run_PO4_inject) { # PO4 injection
  #     edges_Ca$color.color[c(26,27,28)] <- "yellow" # perturbation
  #     edges_Ca$width[c(26,27,28)] <- 8
  #     
  #     if (input$tmaxbis <= 3) {
  #       
  #       edges_Ca$color.color[34] <- "yellow" # perturbation
  #       edges_Ca$width[34] <- 8
  #       
  #     }
  #     
  #   }
  #   
  #   visNetworkProxy("network_Ca") %>%
  #     visUpdateEdges(edges = edges_Ca)
  #   
  # })
  
  #------------------------------------------------------------------------- 
  #  
  #  Make interactive plot by loading tables of diseases
  #  be careful when put on webserver to change the path of table to avoid
  #  reading errors
  #-------------------------------------------------------------------------
  
  # php1
  
  path_to_php1 <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/php1.csv" 
  #path_to_php1 <- "/srv/shiny-server/capApp/case_studies_app/www/php1.csv" 
  php1_table <- read.csv(path_to_php1)
  
  php1_vec <- 4.192*seq(1,300,by = 10) # create the sequence of PTH production rate
  names(php1_vec) <- paste("k_prod_PTHg =", php1_vec)
  
  
  output$php1_plot <- renderPlotly({
      
      input$run_php1
      
      xvar <- list(title = "k_prod_PTHg fold increase", 
                   range = c(min(php1_vec/php1_vec[1]), 
                             max(php1_vec)/php1_vec[1]))
      yvar1 <- list(title = "Normalized concentrations", range = c(0, 2))
      yvar2 <- list(title = "Normalized concentrations", range = c(0,7))
      yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,5))
      yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,3))
      
      plot_CaP_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], 
                               y = php1_table[,"Ca_p"]/php1_table[1,"Ca_p"],
                               type = "scatter", mode = "lines", 
                               line = list(color = 'rgb(27, 27, 244)', width = 2), 
                               showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"PO4_p"]/php1_table[1,"PO4_p"], 
                  line = list(color = 'rgb(244, 27, 27)', width = 2), showlegend = F) %>%
        add_annotations(x = 400, y = 2.6, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", showarrow = T) %>%
        add_annotations(x = 400, y = 0.3, xref = "x", yref = "y",text = "<b>[PO4]p</b>", showarrow = T) %>%
        layout(xaxis = NULL, yaxis = yvar1)
      
      plot_hormones_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], 
                                    y = php1_table[,"PTH_p"]/php1_table[1,"PTH_p"],
                                    type = "scatter", mode = "lines", 
                                    line = list(color = 'black', width = 2, dash = "dash"), 
                                    showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"D3_p"]/php1_table[1,"D3_p"], 
                  line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"FGF_p"]/php1_table[1,"FGF_p"], 
                  line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        add_annotations(x = 200, y = 5.2, xref = "x2", yref = "y2", text = "<b>[PTH]p</b>", showarrow = T, ax = 20, ay = 25) %>%
        add_annotations(x = 400, y = 10, xref = "x2", yref = "y2", text = "<b>[D3]p</b>", showarrow = T) %>%
        add_annotations(x = 600, y = 2, xref = "x2", yref = "y2", text = "<b>[FGF23]p</b>", showarrow = T) %>%
        layout(xaxis = NULL, yaxis = yvar2)
      
      plot_Ca_fluxes_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], 
                                     y = php1_table[,"U_Ca"]/php1_table[1,"U_Ca"],
                                     type = "scatter", mode = "lines", 
                                     line = list(color = 'black', width = 2, dash = "dash"), 
                                     showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Abs_int_Ca"]/php1_table[1,"Abs_int_Ca"], 
                  line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Res_Ca"]/php1_table[1,"Res_Ca"], 
                  line = list(color = 'black', width = 2, dash = "dashdot"), showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Ac_Ca"]/php1_table[1,"Ac_Ca"], 
                  line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        layout(xaxis = xvar, yaxis = yvar3)
      
      plot_PO4_fluxes_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], 
                                      y = php1_table[,"U_PO4"]/php1_table[1,"U_PO4"],
                                      type = "scatter", mode = "lines", 
                                      line = list(color = 'black', width = 2, dash = "dash"), 
                                      name = "Urinary Excretion") %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Abs_int_PO4"]/php1_table[1,"Abs_int_PO4"], 
                  line = list(color = 'black', width = 2, dash = "dot"), 
                  name = "Intestinal absorption") %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Res_PO4"]/php1_table[1,"Res_PO4"], 
                  line = list(color = 'black', width = 2, dash = "dashdot"), 
                  name = "Bone resorption") %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Ac_PO4"]/php1_table[1,"Ac_PO4"], 
                  line = list(color = 'black', width = 2, dash = "solid"), 
                  name = "FLux into bone") %>%
        #add_annotations(x= 600, y= 4, xref = "x4", yref = "y4", text = "<b>Urinary excretion</b>", showarrow = T) %>%
        #add_annotations(x= 200, y= 3, xref = "x4", yref = "y4", text = "<b>Bone resorption</b>", showarrow = T) %>%
        #add_annotations(x= 400, y= 3, xref = "x4", yref = "y4", text = "<b>Intestinal absorption</b>", showarrow = T) %>%
        #add_annotations(x= 200, y= 2, xref = "x4", yref = "y4", text = "<b>Flux into bone</b>", showarrow = T) %>%
        layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'v', x = 0.05, y = 0.3))
      
      
      plot_php1 <- subplot(plot_CaP_php1, plot_hormones_php1, plot_Ca_fluxes_php1, plot_PO4_fluxes_php1, 
                           titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.07, 
                           heights = c(0.5,0.5))
    
  })
  
  # Vitamin D3 deficiency
  
  path_to_hypoD3 <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/hypoD3.csv"
  #path_to_hypoD3 <- "/srv/shiny-server/capApp/case_studies_app/www/hypoD3.csv"
  hypoD3_table <- read.csv(path_to_hypoD3)
  
  hypoD3_vec <- rev(2.5e-005*seq(0, 1, by = 0.01)) # create the sequence of D3 inact and reverse the vector
  names(hypoD3_vec) <- paste("D3_inact =", hypoD3_vec)
  
  output$hypoD3_plot <- renderPlotly({
      
      input$run_hypoD3
      
      xvar <- list(title = "D3_inact fold decrease", 
                   range = c(max(hypoD3_vec/hypoD3_vec[1]), 
                             min(hypoD3_vec)/hypoD3_vec[1]),
                   autorange = F, autorange = "reversed")
      xvar_bis <- list(title = "", range = c(max(hypopara_vec/hypopara_vec[1]), 
                                             min(hypopara_vec)/hypopara_vec[1]),
                       autorange = F, autorange = "reversed")
      yvar1 <- list(title = "Normalized concentrations", range = c(0, 1.1))
      yvar2 <- list(title = "Normalized concentrations", range = c(0,4))
      yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,1.2))
      yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,1.1))
      
      plot_CaP_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], 
                                 y = hypoD3_table[,"Ca_p"]/hypoD3_table[1,"Ca_p"],
                                 type = "scatter", mode = "lines", 
                                 line = list(color = 'rgb(27, 27, 244)', width = 2), 
                                 showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"PO4_p"]/hypoD3_table[1,"PO4_p"], 
                  line = list(color = 'rgb(244, 27, 27)', width = 2), showlegend = F) %>%
        add_annotations(x = 0.7, y = 0.95, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", 
                        showarrow = T, ax = -20, ay = 40) %>%
        add_annotations(x = 0.4, y = 1.1, xref = "x", yref = "y",text = "<b>[PO4]p</b>", 
                        showarrow = T, ax = -20, ay = -20) %>%
        layout(xaxis = xvar_bis, yaxis = yvar1)
      
      plot_hormones_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], 
                                      y = hypoD3_table[,"PTH_p"]/hypoD3_table[1,"PTH_p"],
                                      type = "scatter", mode = "lines", 
                                      line = list(color = 'black', width = 2, dash = "dash"), 
                                      showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"D3_p"]/hypoD3_table[1,"D3_p"], 
                  line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"FGF_p"]/hypoD3_table[1,"FGF_p"], 
                  line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        add_annotations(x = 0, y = 2.3, xref = "x2", yref = "y2", text = "<b>[PTH]p</b>", showarrow = T) %>%
        add_annotations(x = 0.9, y = 0.9, xref = "paper", yref = "y2", text = "<b>[D3]p</b>", showarrow = T) %>%
        add_annotations(x = 0.9, y = 0.4, xref = "paper", yref = "y2", text = "<b>[FGF23]p</b>", showarrow = T, 
                        ax = -20, ay = 30) %>%
        layout(xaxis = xvar_bis, yaxis = yvar2)
      
      plot_Ca_fluxes_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], 
                                       y = hypoD3_table[,"U_Ca"]/hypoD3_table[1,"U_Ca"],
                                       type = "scatter", mode = "lines", 
                                       line = list(color = 'black', width = 2, dash = "dash"), 
                                       showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Abs_int_Ca"]/hypoD3_table[1,"Abs_int_Ca"], 
                  line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Res_Ca"]/hypoD3_table[1,"Res_Ca"], 
                  line = list(color = 'black', width = 2, dash = "dashdot"), showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Ac_Ca"]/hypoD3_table[1,"Ac_Ca"], 
                  line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        layout(xaxis = xvar, yaxis = yvar3)
      
      plot_PO4_fluxes_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], 
                                        y = hypoD3_table[,"U_PO4"]/hypoD3_table[1,"U_PO4"],
                                        type = "scatter", mode = "lines", 
                                        line = list(color = 'black', width = 2, dash = "dash"),
                                        name = "Urinary Excretion") %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Abs_int_PO4"]/hypoD3_table[1,"Abs_int_PO4"], 
                  line = list(color = 'black', width = 2, dash = "dot"), 
                  name = "Intestinal absorption") %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Res_PO4"]/hypoD3_table[1,"Res_PO4"], 
                  line = list(color = 'black', width = 2, dash = "dashdot"), 
                  name = "Bone resorption") %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Ac_PO4"]/hypoD3_table[1,"Ac_PO4"], 
                  line = list(color = 'black', width = 2, dash = "solid"), 
                  name = "Flux into bone") %>%
        layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'v', x = 0.01, y = 0.05))
      
      
      plot_hypoD3 <- subplot(plot_CaP_hypoD3, plot_hormones_hypoD3, plot_Ca_fluxes_hypoD3, 
                             plot_PO4_fluxes_hypoD3, titleX = TRUE, titleY = TRUE,
                             nrows = 2, margin = 0.07, heights = c(0.5,0.5))
    
  })
  
  # Hypoparathyroidism
  
  path_to_hypopara <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/hypopara.csv"
  #path_to_hypopara <- "/srv/shiny-server/capApp/case_studies_app/www/hypopara.csv"
  hypopara_table <- read.csv(path_to_hypopara)
  
  hypopara_vec <- rev(4.192*seq(0, 1, by = 0.01)) # create the sequence of PTH production rate
  names(hypopara_vec) <- paste("k_prod_PTHg =",  hypopara_vec)
  
  output$hypopara_plot <- renderPlotly({
    
    
      
      input$run_hypopara
      
      xvar <- list(title = "k_prod_PTHg fold decrease", 
                   range = c(max(hypopara_vec/hypopara_vec[1]), 
                             min(hypopara_vec)/hypopara_vec[1]),
                   autorange = F, autorange = "reversed")
      xvar_bis <- list(title = "", 
                       range = c(max(hypopara_vec/hypopara_vec[1]), 
                                 min(hypopara_vec)/hypopara_vec[1]),
                       autorange = F, autorange = "reversed")
      yvar1 <- list(title = "Normalized concentrations", range = c(0, 1.4))
      yvar2 <- list(title = "Normalized concentrations", range = c(0,1))
      yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,1))
      yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,1.4))
      
      plot_CaP_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], 
                                   y = hypopara_table[,"Ca_p"]/hypopara_table[1,"Ca_p"],
                                   type = "scatter", mode = "lines", 
                                   line = list(color = 'rgb(27, 27, 244)', width = 2), 
                                   showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"PO4_p"]/hypopara_table[1,"PO4_p"], 
                  line = list(color = 'rgb(244, 27, 27)', width = 2), 
                  showlegend = F) %>%
        add_annotations(x = 0.5, y = 0.85, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", 
                        showarrow = T, ax = -20, ay = 40) %>%
        add_annotations(x = 0.1, y = 1.7, xref = "x", yref = "y",text = "<b>[PO4]p</b>", 
                        showarrow = T) %>%
        layout(xaxis = xvar_bis, yaxis = yvar1)
      
      plot_hormones_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], 
                                        y = hypopara_table[,"PTH_p"]/hypopara_table[1,"PTH_p"],
                                        type = "scatter", mode = "lines", 
                                        line = list(color = 'black', width = 2, dash = "dash"), 
                                        showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"D3_p"]/hypopara_table[1,"D3_p"], 
                  line = list(color = 'black', width = 2, dash = "dot"), 
                  showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"FGF_p"]/hypopara_table[1,"FGF_p"], 
                  line = list(color = 'black', width = 2, dash = "solid"), 
                  showlegend = F) %>%
        add_annotations(x = 0.9, y = 0.2, xref = "paper", yref = "y2", text = "<b>[PTH]p</b>", 
                        showarrow = T, ax = -20, ay = 40) %>%
        add_annotations(x = 0.9, y = 0.67, xref = "paper", yref = "y2", text = "<b>[D3]p</b>", 
                        showarrow = T, ax = 15, ay = -30) %>%
        add_annotations(x = 0.88, y = 0.5, xref = "paper", yref = "y2", text = "<b>[FGF23]p</b>", 
                        showarrow = T, ax = -50, ay = 10) %>%
        layout(xaxis = xvar_bis, yaxis = yvar2)
      
      plot_Ca_fluxes_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], 
                                         y = hypopara_table[,"U_Ca"]/hypopara_table[1,"U_Ca"],
                                         type = "scatter", mode = "lines", 
                                         line = list(color = 'black', width = 2, dash = "dash"), 
                                         showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"Abs_int_Ca"]/hypopara_table[1,"Abs_int_Ca"], 
                  line = list(color = 'black', width = 2, dash = "dot"), 
                  showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"Res_Ca"]/hypopara_table[1,"Res_Ca"], 
                  line = list(color = 'black', width = 2, dash = "dashdot"), 
                  showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"Ac_Ca"]/hypopara_table[1,"Ac_Ca"], 
                  line = list(color = 'black', width = 2, dash = "solid"), 
                  showlegend = F) %>%
        layout(xaxis = xvar, yaxis = yvar3)
      
      plot_PO4_fluxes_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], 
                                          y = hypopara_table[,"U_PO4"]/hypopara_table[1,"U_PO4"],
                                          type = "scatter", mode = "lines", 
                                          line = list(color = 'black', width = 2, dash = "dash"),
                                          name = "Urinary Excretion") %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"Abs_int_PO4"]/hypopara_table[1,"Abs_int_PO4"], 
                  line = list(color = 'black', width = 2, dash = "dot"),
                  name = "Intestinal absorption") %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"Res_PO4"]/hypopara_table[1,"Res_PO4"], 
                  line = list(color = 'black', width = 2, dash = "dashdot"),
                  name = "Bone resorption") %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], 
                  y = hypopara_table[,"Ac_PO4"]/hypopara_table[1,"Ac_PO4"], 
                  line = list(color = 'black', width = 2, dash = "solid"),
                  name = "Flux into bone") %>%
        layout(xaxis = xvar, yaxis = yvar4, 
               legend = list(orientation = 'v', x = 0.01, y = 0.05))
      
      
      plot_hypopara <- subplot(plot_CaP_hypopara, plot_hormones_hypopara, 
                               plot_Ca_fluxes_hypopara, plot_PO4_fluxes_hypopara, 
                               titleX = TRUE, titleY = TRUE,
                               nrows = 2, margin = 0.07, heights = c(0.5,0.5))
      
    
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
      add_lines(x = input$tmax, y = c(0,2), 
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
      add_lines(x = input$tmax, y = c(0,10), 
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
      add_lines(x = input$tmaxbis, y = c(0,8), 
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
      add_lines(x = input$tmaxbis, y = c(0,2), 
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
      add_lines(x = input$tmaxbis, y = c(0,20), 
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
      add_lines(x = input$tmaxtris, y = c(0,8), 
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
      add_lines(x = input$tmaxtris, y = c(0,2), 
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
      add_lines(x = input$tmaxtris, y = c(0,20), 
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
      inputId = c("tmax","tmaxbis","tmaxtris"),
      value = c(input$tmax - 10 , input$tmaxbis - 10, input$tmaxtris - 10))
    
    pmap(sliderlist, 
         updateSliderInput, 
         session = session, 
         label = "Current Time", 
         min = 1, 
         max = 250, 
         step = 1)
    
  })
  
  observeEvent(input$next1,{# if the user clicks on next
    
    sliderlist <- list(
      inputId = c("tmax","tmaxbis","tmaxtris"),
      value = c(input$tmax + 10 , input$tmaxbis + 10, input$tmaxtris + 10))
    
    pmap(sliderlist, 
         updateSliderInput, 
         session = session, 
         label = "Current Time", 
         min = 1, 
         max = 250, 
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
  
})