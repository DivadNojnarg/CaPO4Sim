#-------------------------------------------------------------------------
#  This application is a R-Shiny implementation of a calcium and phosphate 
#  homeostasis model. It aims at being used by medical students but also
#  researchers. See https://divadnojnarg.github.io for more informations
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------


library(shiny)
library(plotly)
library(deSolve)
require(visNetwork)
library(shinyBS)
#library(parallel)
#library(sparklines)

#library(tikzDevice) # For Latex rendering in graphs but does not work

source("cap_fixed_parameters.R")
source("cap_calc_parameters.R")
source("calcium_phosphate_core.R") # core model
source("calc_change.R")
source("box_close.R")
#source("chatBox_tools.R")

#path_to_images <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/global_app/www"

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
  
  # Load state values based on files previously created for each case (php1, hypopara, hypoD3)
  
  state_php1 <- reactive({

    state_php1 <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/treatments_app/init_php1.csv", # for local config
                           stringsAsFactors = FALSE)
    #state_php1 <- read.csv("/srv/shiny-server/capApp/treatments_app/init_php1.csv", # use in the server
                           #stringsAsFactors = FALSE)
    state_php1 <- unlist(state_php1[,-1]) # need unlist to convert the dataframe in vector as required for the state variable

  })

  state_hypopara <- reactive({

    state_hypopara <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/treatments_app/init_hypopara.csv", # for local config
                           stringsAsFactors = FALSE)
    #state_hypopara <- read.csv("/srv/shiny-server/capApp/treatments_app/init_hypopara.csv", # use in the server
    #stringsAsFactors = FALSE)
    state_hypopara <- unlist(state_hypopara[,-1]) # need unlist to convert the dataframe in vector as required for the state variable

  })

  state_hypoD3 <- reactive({

    state_hypoD3 <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/treatments_app/init_hypoD3.csv", # for local config
                               stringsAsFactors = FALSE)
    #state_hypoD3 <- read.csv("/srv/shiny-server/capApp/treatments_app/init_hypoD3.csv", # use in the server
    #stringsAsFactors = FALSE)
    state_hypoD3 <- unlist(state_hypoD3[,-1]) # need unlist to convert the dataframe in vector as required for the state variable

  })
  
  state <- reactive({ 
    
    if(!is.null(input$disease_selected)){
      if(input$disease_selected == "primary-hyperparathyroidism"){

       state_php1()

      }
      else if (input$disease_selected == "hypoparathyroidism"){

        state_hypopara()

      }
      else if (input$disease_selected == "vitamin D3 deficiency"){

        state_hypoD3()

       }
     }
     else{ # default initial state
      
      c("PTH_g" = 1288.19, "PTH_p" = 0.0687, "D3_p" = 564.2664, "FGF_p" = 16.78112, "Ca_p" = 1.2061,
        "Ca_f" = 1.8363, "Ca_b" = 250, "PO4_p" = 1.4784, "PO4_f" = 0.7922, "PO4_b" = 90, "PO4_c" = 2.7719,
        "CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, "CaProt_p" = 1.4518,
        "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0) 
      
    }
  })
  
  # Ca_iv control interface when Ca iv injection is choosen in the menu
  
  output$Ca_iv_control <- renderUI({
    
    if(is.element("Ca iv injection", input$treatment_selected)){
      
      tagList( sliderInput("Ca_inject", "$$ k_{inject}^{Ca} $$", min = 0, max = 0.002, value = 0.001, step = 0.0001) %>%
                 shinyInput_label_embed(
                   icon("info") %>%
                     bs_embed_tooltip(title = "Rate of injection of calcium in plasma (μmol/min)")),
               
               numericInput("t_start_inject","Time when begins the Ca iv injection:", 0, min = 0, max = NA, width = "50%"),
               numericInput("t_stop_inject","Time,when stops the Ca iv injection:", input$tmax, min = 0, max = NA, width = "50%")
      )
      
    }
    
  })
  
  # Create parameters sets specific for events
  
  parameters_event <- reactive({
    
    input$treatment_selected

    if(!is.null(input$Ca_inject)){ # for Ca iv injection

      c("t_start_inject" = input$t_start_inject, "t_stop_inject" = input$t_stop_inject)

    }else c("t_start_inject" = 0, "t_stop_inject" = 0)

    })

  # Create parameters sets for all diseases an treatments
  
  parameters <- reactive({ 
    
    if(!is.null(input$Ca_inject) && (!is.null(input$disease_selected) || !is.null(input$treatment_selected))){
    
    c("k_prod_PTHg" = ifelse(is.element("primary-hyperparathyroidism", input$disease_selected), 300*4.192, 4.192), "beta_exo_PTHg" = 5.9e-002, 
    "gamma_exo_PTHg" = 5.8e-002, "D3_inact" = 2.5e-005, "k_deg_D3" = 1e-003,
    "k_prod_FGF" = 6.902e-011, "I_Ca" = 2.2e-003, "Lambda_ac_Ca" = 5.5e-004,
    "Lambda_ac_P" = 2.75e-004, "Lambda_res_min" = 1e-004, 
    "delta_res_max" = 6e-004, "k_p_Ca" = 0.44, "k_f_Ca" = 2.34e-003, "I_P" = 1.55e-003, "k_pc" = 0.1875,
    "k_cp" = 1e-003, "k_p_P" = 13.5, "k_f_P" = 0.25165, "k_fet" = 0.3,
    "k_c_CPP" = 3, "Na" = 142, "Prot_tot_p" = 0.6, "Vp" = 0.01,
    "GFR" = 2e-003, "pH" = 7.4, "r" = 4, "a" = 0.8, "b" = 0.2, "PTX_coeff" = ifelse(is.element("parathyroid surgery", input$treatment_selected), 0, 1),
    "k_inject_Ca" = ifelse(is.element("Ca iv injection", input$treatment_selected), input$Ca_inject, 0))
      
    }
    
    else{
      
      c("k_prod_PTHg" = 4.192, "beta_exo_PTHg" = 5.9e-002, 
        "gamma_exo_PTHg" = 5.8e-002, "D3_inact" = 2.5e-005, "k_deg_D3" = 1e-003,
        "k_prod_FGF" = 6.902e-011, "I_Ca" = 2.2e-003, "Lambda_ac_Ca" = 5.5e-004,
        "Lambda_ac_P" = 2.75e-004, "Lambda_res_min" = 1e-004, 
        "delta_res_max" = 6e-004, "k_p_Ca" = 0.44, "k_f_Ca" = 2.34e-003, "I_P" = 1.55e-003, "k_pc" = 0.1875,
        "k_cp" = 1e-003, "k_p_P" = 13.5, "k_f_P" = 0.25165, "k_fet" = 0.3,
        "k_c_CPP" = 3, "Na" = 142, "Prot_tot_p" = 0.6, "Vp" = 0.01,
        "GFR" = 2e-003, "pH" = 7.4, "r" = 4, "a" = 0.8, "b" = 0.2, "PTX_coeff" = 1,
        "k_inject_Ca" = 0)
      
    }
    
  })
  
  # make a vector of input$parameters, fixed_parameters and calculated parameters
  
  parameters_bis <- reactive({ c(parameters(), parameters_fixed, parameters_event()) }) # parameters_calc(input) does not work
  
  #------------------------------------------------------------------------- 
  #  
  #  Integrate equations using deSolve package to generate table
  #  out is a reactive intermediate component that is called by
  #  to make plots or other stuffs
  #
  #-------------------------------------------------------------------------
  
  out <- reactive({
    
    parameters_bis <- parameters_bis()
    state <- state()
    times <- times()
    
    as.data.frame(ode(y = state, times = times, func = calcium_phosphate_core, parms = parameters_bis))
    
  })
  
  #output$table <- renderDataTable( out(), options = list(pageLength = 10) )
  
  #------------------------------------------------------------------------- 
  #  
  #  The network part: make interactive diagramms of Ca and PO4 homeostasis
  #  as well as regulation by hormones such as PTH, vitamin D3 and FGF23
  #  
  #
  #-------------------------------------------------------------------------
  
  # Generate the CaP Graph network
  
  nodes_Ca <- reactive({
    
    # ifelse(input$run_php1, 
    #        image <- c("food.png","intestine.png","feces.png","kidney.png","kidney.png","bone.png","kidney.png","urine.png",
    #                   "php1.jpg","kidney.png","kidney.png","kidney.png","cells.png","kidney.png","kidney.png"), 
    #        image <- c("food.png","intestine.png","feces.png","kidney.png","kidney.png","bone.png","kidney.png","urine.png",
    #                   "parathyroid_gland.png","kidney.png","kidney.png","kidney.png","cells.png","kidney.png","kidney.png"))
    
    d <- data.frame(id = 1:18,
                    shape = c("image","image","image","image","image","image","image","image","image","image","image",
                              "image","image","image","image","image","image","text"), 
                    # square are always scalable
                    image = c("food.svg","intestine.svg","feces.svg","plasma.svg","rapid-bone.svg","bone.svg","kidney.svg","urine.svg",
                              "parathyroid_gland.svg","D3.svg","D3.svg","FGF23.svg","cells.svg","Cap.svg","PO4.svg","infusion.png","injection.png",""),
                    label = c("", "","", "", "", "", "", "", "", 
                              "", "","","","","","","","EGTA"),
                    #title = out()[nrow(out()),"Ca_p"],
                    #title = c(rep(as.character(as.tags(sparklines::sparkline(out()[nrow(out()),"Ca_p"]))),17)), # does not work
                    title = paste0(img(src ="pth_synthesis.png")), # tooltip to display an image
                    x = c(8, 12, 20, 12, 0, 3, 22, 30, 12, 5, 22, 28, 11, 18, 28, 18, 28, 20),
                    y = c(0, 4, 0, 12, 12, 20, 12, 14, 27, 6, 24, 20, 20, 32, 32, 36, 36, 36),
                    color = list(background = "#97C2FC", border = "#97C2FC", highlight = list(background = "orange", border = "orange")),
                    size = c(50,50,50,50,50,50,50,50,50,25,25,25,50,25,25,25,35,35), # rep(50,13)
                    hidden = c(rep(FALSE,18)))
    
  })
  
  edges_Ca <- reactive({
    
    d <- data.frame(from = c(1,2,2,4,5,5,6,7,4,7,9,9,10,11,9,11,11,12,12,7,13,4,7,14,14,15,15,15,7,5,4,5,16,17,18), 
                    to = c(2,3,4,5,4,6,4,4,7,8,6,7,2,9,11,7,12,11,7,7,4,13,4,11,9,9,11,12,8,4,5,6,14,15,14),
                    label = c("Intake", "Fecal Excretion", " Intestinal absorption ", "Rapid Ca storage", "Rapid Ca release", "Ca Flux into bone", 
                              "Resorption", "Ca reabsorption", "filtration", "Urinary Ca excretion","+","","+","-","+","","+","-","",
                              "CaSR (-)","Cells-plasma", "Plasma-cells", "PO4 reabsorption","-","CaSR(-)","+","-","+","Urinary PO4 excretion","Rapid PO4 release",
                              "Rapid PO4 storage","PO4 flux into bone","Ca iv injection","PO4 iv injection","EGTA infusion"),
                    id = 1:35,
                    width = 4,
                    font.size = c(rep(11,10),rep(30,5),11,rep(30,2),rep(11,5),30,12,rep(30,3), rep(11,7)),
                    color = list(color = c(rep("black", 35)), 
                                 highlight = "yellow", opacity = 1.0),
                    dashes = c(rep(F,10),rep(T,10),rep(F,3),rep(T,5), rep(F,7)),
                    arrowStrikethrough = TRUE,
                    #title = paste("Edge", 1:8),
                    smooth = c(rep(T,35)),
                    length = c(200,200,200,400,300,300,rep(200,22),200,300,300,300,200,200,200),
                    stringsAsFactors=FALSE) # to change edges color do not forget this "stringsAsFactors=FALSE"
    
  })
  
  # Generate the output of the Ca graph to be used in body
  
  output$network_Ca <- renderVisNetwork({
    
    nodes_Ca <- nodes_Ca()
    edges_Ca <- edges_Ca()
    
    legend_nodes <- data.frame(shape = c("image","image"),
                               image = c("food.svg","D3.svg"),
                               label = c("compartment", "hormones"),
                               size = c(15,15))
    
    legend_edges <- data.frame(from = c(5,1,15,9), 
                               to = c(4,2,12,7),
                               width = 4,
                               arrows = "to",
                               label = c("inhibited flux", "stimulated flux", "hormonal regulation","perturbation"),
                               color = list(color = c("red","green","black","yellow")),
                               font.align = "bottom",
                               dashes = c(F,F,T,F),
                               smooth = c(T,T,T,F))
    
    visNetwork(nodes_Ca, edges_Ca, width = "100%", height = "100%") %>%
      visNodes(shapeProperties = list(useBorderWithImage = FALSE)) %>%
      visEdges(shadow = FALSE, font = list(align = "horizontal"), # put shadow on false
               arrows =list(to = list(enabled = TRUE, scaleFactor = 1, type = "arrow"))) %>%
      visOptions(highlightNearest = FALSE, clickToUse = FALSE, manipulation = FALSE) %>%
      visInteraction(hover = TRUE, hoverConnectedEdges = FALSE, selectConnectedEdges = FALSE) %>% # prevent edge from being selected when a node is selected
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}") %>%
      # visEvents(deselectNode = "function(nodes) {
      #   Shiny.onInputChange('current_node_id', 0);
      #           ;}") %>%
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', edges.edges);
                ;}") %>%
      visEvents(stabilized = "function() { 
                this.setOptions({nodes : {physics : false}})}") %>%
      # visEvents(deselectEdge = "function(edges) {
      #   Shiny.onInputChange('current_edge_id', 0);
      #           ;}") %>%
      #visLayout(randomSeed = 12, improvedLayout = TRUE)
      visIgraphLayout(smooth = TRUE) %>% # to disable repulsion
      visLegend(addNodes = legend_nodes, addEdges = legend_edges, useGroup = FALSE, ncol = 2) %>% # add the legend, ncol = 2 to show edges otherwise a bug appear
      #visPhysics(forceAtlas2Based = list(avoidOverlap = 1)) %>%
      visExport(type = "pdf") # export the graph as pdf
    
  })
  
  output$id <- renderPrint({
    input$current_edge_id
  })
  
  output$id_bis <- renderPrint({
    input$current_node_id
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  The graph part: calls out(), parameters_bis()
  #  Interactive graph as a result of click on the diagram
  #
  #-------------------------------------------------------------------------
  
  
  # Generate a graph when node is clicked. The graph corresponds to the node clicked
  
  output$hover_graph <- renderPlotly({
    
    validate(
      need(input$current_node_id, 'Select one node on the graph!')
    )
    
    validate(
      need(input$current_node_id != 0, 'Select one node on the graph!')
    )
    
    if (input$current_node_id != 0 && !is.null(input$current_node_id)){
      
      out <- out()
      parameters_bis <- parameters_bis()
      
      xvar <- list(title = "time (min)", range = c(0, max(out[,1]))) 
      yvar1 <- list(title = "Concentrations (mM)", range = c(min(out[,"Ca_p"],out[,"PO4_p"])*0.8,max(out[,"Ca_p"],out[,"PO4_p"])*1.2))
      yvar2 <- list(title = "[PTH]p (pM)", range = c(min(out[,"PTH_p"]/parameters_bis["Vp"])*0.8,max(out[,"PTH_p"]/parameters_bis["Vp"])*1.2))
      yvar3 <- list(title = "[D3]p (pM)", range = c(min(out[,"D3_p"])*0.8,max(out[,"D3_p"])*1.2))
      yvar4 <- list(title = "[FGF23]p (pM)", range = c(min(out[,"FGF_p"])*0.8,max(out[,"FGF_p"])*1.2))
      yvar5 <- list(title = "[Ca]f (mmol)", range = c(min(out[,"Ca_f"])*0.8,max(out[,"Ca_f"])*1.2))
      yvar6 <- list(title = "[PO4]f (mmol)", range = c(min(out[,"PO4_f"])*0.8,max(out[,"PO4_f"])*1.2))
      yvar7 <- list(title = "[Ca]b (mmol)", range = c(min(out[,"Ca_b"])*0.8,max(out[,"Ca_b"])*1.2))
      yvar8 <- list(title = "[PO4]b (mmol)", range = c(min(out[,"PO4_b"])*0.8,max(out[,"PO4_b"])*1.2))
      yvar9 <- list(title = "[PTH]g (pmol)", range = c(min(out[,"PTH_g"])*0.8,max(out[,"PTH_g"])*1.2))
      yvar10 <- list(title = "[PO4]c (mmol)", range = c(min(out[,"PO4_c"])*0.8,max(out[,"PO4_c"])*1.2))
      
      # id = 4 corresponds to plasma elements
      if (input$current_node_id == 4){
        
        p1 <- plot_ly(out, x = out[,1], y = out[,"Ca_p"], type = "scatter", mode = "lines", line = list(color = 'rgb(27, 102, 244)', width = 2)) %>%
          add_lines(x = out[,1], y = out[,"PO4_p"], line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          add_annotations(x = out[10,1], y = out[1,"Ca_p"]+0.5, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", showarrow = FALSE , 
                          font = list(color = "blue", size = 10)) %>%
          add_annotations(x = out[10,1], y = out[1,"PO4_p"]+1, xref = "x", yref = "y",text = "<b>[PO4]p</b>", showarrow = FALSE , 
                          font = list(color = "red", size = 10)) %>%
          layout(xaxis = NULL, yaxis = yvar1)
        
        p2 <- plot_ly(data = out, x = out[,1], y = out[,"PTH_p"]/parameters_bis["Vp"], type = "scatter", mode = "lines",
                      line = list(color = 'black', width = 2)) %>%
          layout(xaxis = NULL, yaxis = yvar2)
        
        p3 <- plot_ly(data = out, x = out[,1], y = out[,"D3_p"], type = "scatter", mode = "lines",
                      line = list(color = 'black', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar3)
        
        p4 <- plot_ly(data = out, x = out[,1], y = out[,"FGF_p"], type = "scatter", mode = "lines",
                      line = list(color = 'black', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar4)
        
        p <- subplot(p1, p2, p3, p4, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.12, heights = c(0.5,0.5))
        
        hide_legend(p)
        
      }
      # id = 5 corresponds to Ca and PO4 fast bone pool
      else if(input$current_node_id == 5){
        
        p1 <- plot_ly(out, x = out[,1], y = out[,"Ca_f"], type = "scatter", mode = "lines", line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = NULL, yaxis = yvar5)
        
        p2 <- plot_ly(out, x = out[,1], y = out[,"PO4_f"], type = "scatter", mode = "lines", line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar6)
        
        p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.12)
        
        hide_legend(p)
        
      }
      # id = 6 corresponds to Ca and PO4 deep bone pool
      else if(input$current_node_id == 6){
        
        p1 <- plot_ly(out, x = out[,1], y = out[,"Ca_b"], type = "scatter", mode = "lines", line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = NULL, yaxis = yvar7)
        
        p2 <- plot_ly(out, x = out[,1], y = out[,"PO4_b"], type = "scatter", mode = "lines", line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar8)
        
        p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.12)
        
        hide_legend(p)
        
      }
      # id = 9 corresponds to PTH pool in PT glands
      else if(input$current_node_id == 9){
        
        p <- plot_ly(out, x = out[,1], y = out[,"PTH_g"], type = "scatter", mode = "lines", line = list(color = 'black', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar9, title = "PTH quantity in parathyroid glands")
        
        hide_legend(p)
        
      }
      # id = 17 corresponds to PO4 pool in cells
      else if(input$current_node_id == 13){
        
        p <- plot_ly(out, x = out[,1], y = out[,"PO4_c"], type = "scatter", mode = "lines", line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar10, title = "PO4 quantity in cells")
        
        hide_legend(p)
        
      }
      else{
        p <- plot_ly() %>%
          add_annotations("Please select another node!", showarrow = FALSE, font = list(color = "red", size = 20))
      }
      
    }
    
  })
  
  output$hover_graph_bis <- renderPlotly({
    
    validate(
      need(input$current_edge_id, 'Select one edge on the graph!')
    )
    
    validate(
      need(input$current_edge_id != 0, 'Select one edge on the graph!')
    )
    
    if (input$current_edge_id != 0 && !is.null(input$current_edge_id)){
      
      out <- out()
      parameters_bis <- parameters_bis()
      
      xvar <- list(title = "time (min)", range = c(0, max(out[,1]))) # add slider xaxis: rangeslider = list(type = "time")
      yvar2 <- list(title = "Ca (µmol/min)", range = c(min(out[,"Abs_int_Ca"]*1000*0.8),max(out[,"Abs_int_Ca"]*1000*1.2)))
      yvar3 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"Abs_int_PO4"]*1000*0.8),max(out[,"Abs_int_PO4"]*1000*1.2)))
      yvar6 <- list(title = "Ca (µmol/min)", range = c(min(out[,"Ac_Ca"]*1000*0.8),max(out[,"Ac_Ca"]*1000*1.2)))
      yvar7 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"Ac_PO4"]*1000*0.8),max(out[,"Ac_PO4"]*1000*1.2)))
      yvar8 <- list(title = "Ca (µmol/min)", range = c(min(out[,"Res_Ca"]*1000*0.8),max(out[,"Res_Ca"]*1000*1.2)))
      yvar9 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"Res_PO4"]*1000*0.8),max(out[,"Res_PO4"]*1000*1.2)))
      yvar10 <- list(title = "Ca (µmol/min)", range = c(min(out[,"Reabs_Ca"]*1000*0.8),max(out[,"Reabs_Ca"]*1000*1.2)))
      yvar11 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"Reabs_PO4"]*1000*0.8),max(out[,"Reabs_PO4"]*1000*1.2)))
      yvar12 <- list(title = "Ca (µmol/min)", range = c(min(out[,"U_Ca"]*1000*0.8),max(out[,"U_Ca"]*1000*1.2)))
      yvar13 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"U_PO4"]*1000*0.8),max(out[,"U_PO4"]*1000*1.2)))
      yvar14 <- list(title = "Ca (µmol/min)", range = c(min(out[,"Ca_pf"]*1000*0.8),max(out[,"Ca_pf"]*1000*1.2)))
      yvar15 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"PO4_pf"]*1000*0.8),max(out[,"PO4_pf"]*1000*1.2)))
      yvar16 <- list(title = "Ca (µmol/min)", range = c(min(out[,"Ca_fp"]*1000*0.8),max(out[,"Ca_fp"]*1000*1.2)))
      yvar17 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"PO4_fp"]*1000*0.8),max(out[,"PO4_fp"]*1000*1.2)))
      yvar18 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"PO4_pc"]*1000*0.8),max(out[,"PO4_pc"]*1000*1.2)))
      yvar19 <- list(title = "PO4 (µmol/min)", range = c(min(out[,"PO4_cp"]*1000*0.8),max(out[,"PO4_cp"]*1000*1.2)))
      
      # id = 3 corresponds to intestinal Ca and PO4 absorption
      if(input$current_edge_id == 3){
        
        p1 <- plot_ly(data = out, x = out[,1], y = out[,"Abs_int_Ca"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar2)
        
        p2 <- plot_ly(data = out, x = out[,1], y = out[,"Abs_int_PO4"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar3)
        
        p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.12) %>%
          layout(title = "Intestinal Absorption")
        
        hide_legend(p)
        
      }
      # id = 4 corresponds to rapid storage of Ca in the rapidly exchangeable bone pool
      else if (input$current_edge_id == 4){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"Ca_pf"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar14, title = "Rapid Ca storage in bone")
        
      }
      # id = 5 corresponds to rapid release of Ca from the rapidly exchangeable bone pool
      else if (input$current_edge_id == 5){
        
        p1 <- plot_ly(data = out, x = out[,1], y = out[,"Ca_fp"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar16, title = "Rapid Ca release from bone")
        
      }
      # id = 6 corresponds to storage of Ca in the bone pool
      else if (input$current_edge_id == 6){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"Ac_Ca"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar6, title = "Ca flux into bone")
        
      }
      # id = 7 corresponds to release of Ca and PO4 in the bone pool (resorption)
      else if (input$current_edge_id == 7){
        
        p1 <- plot_ly(data = out, x = out[,1], y = out[,"Res_Ca"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar8)
        
        p2 <- plot_ly(data = out, x = out[,1], y = out[,"Res_PO4"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar9)
        
        p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.12) %>%
          layout(title = "Bone resorption")
        
        hide_legend(p)
        
      }
      # id = 8 corresponds to renal reabsorption of Ca
      else if (input$current_edge_id == 8){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"Reabs_Ca"]*1000, type = "scatter", mode = "lines",
                     line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar10, title = "Ca renal reabsorption")
        
      }
      # id = 10 corresponds to U_Ca 
      else if (input$current_edge_id == 10){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"U_Ca"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar12, title = "Urinary Ca excretion")
        
      }
      # id = 24 corresponds to PO4 release from cells
      else if (input$current_edge_id == 21){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"PO4_pc"]*1000, type = "scatter", mode = "lines",
                     line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar18, title = "PO4 Cell release")
        
      }
      # id = 22 corresponds to PO4 storage in cells
      else if (input$current_edge_id == 22){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"PO4_cp"]*1000, type = "scatter", mode = "lines",
                     line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar19, title = "PO4 Cell storage")
        
      }
      # id = 23 corresponds to renal reabsorption of PO4
      else if (input$current_edge_id == 23){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"Reabs_PO4"]*1000, type = "scatter", mode = "lines",
                     line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar11, title = "PO4 renal reabsorption")
        
      }
      # id = 29 corresponds to U_PO4
      else if (input$current_edge_id == 29){
        
        p2 <- plot_ly(data = out, x = out[,1], y = out[,"U_PO4"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar13, title = "Urinary PO4 excretion")
        
      }
      # id = 30 corresponds to rapid release of PO4 from the rapidly exchangeable bone pool
      else if (input$current_edge_id == 30){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"PO4_fp"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar17, title = "Rapid PO4 release from bone")

      }
      # id = 31 corresponds to rapid storage of PO4 in the rapidly exchangeable bone pool
      else if(input$current_edge_id == 31){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"PO4_pf"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar15, title = "Rapid PO4 storage in bone")
        
      }
      # id = 32 corresponds to storage of PO4 in the bone pool
      else if (input$current_edge_id == 32){
        
        p <- plot_ly(data = out, x = out[,1], y = out[,"Ac_PO4"]*1000, type = "scatter", mode = "lines",
                      line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
          layout(xaxis = xvar, yaxis = yvar7, title = "PO4 flux into bone")
        
      }
      else{
        p <- plot_ly() %>%
          add_annotations("Please select another arrow!", showarrow = FALSE, font = list(color = "red", size = 20))
      }
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Events in the network, triggered by changing input,...
  #  
  #
  #-------------------------------------------------------------------------
  
  # To develop
  
  #------------------------------------------------------------------------- 
  #  
  #  Events for the CaPO4 Homeostasis diagramm whenever a flux change
  #  
  #-------------------------------------------------------------------------
  
  # Change arrow color relatively to the value of fluxes for Ca injection/PO4 injection as well as PO4 gavage
  
  observe({
    
    out <- out()
    edges_Ca <- edges_Ca()
    
    calc_change_t <- calc_change(out)
    calc_change_t$X <- NULL # remove column X
    
    path_to_calc_change_base <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/treatments_app/www/calc_change_base.csv" # only run in local
    #path_to_calc_change_base <- "/srv/shiny-server/capApp/treatments_app/www/calc_change_base.csv" # only works on the linux server
    calc_change_base <- read.csv(path_to_calc_change_base) # load the base case fluxes file to compare with "live" fluxes
    calc_change_base$X <- NULL
    calc_change_sum <- round(calc_change_t,2) - round(calc_change_base,2) # calculate the difference between live fluxes and base-case values
    index <- c(3,10,29,7,6,32,8,23,4,31,5,30,22,21) # index of arrows in the graph (which are fluxes and not regulations)
    calc_change_sum <- rbind(calc_change_sum, index)
    
    flux_changed_index <- which(calc_change_sum[1,] != 0) # calculate which element in the sum table is different of 0 and store the index
    arrow_index <- as.numeric(t(calc_change_sum[2,flux_changed_index])) # convert to arrow index in the interactive diagramm
    
    if(!is.null(flux_changed_index)){
      for (i in (1:ncol(calc_change_sum))){
        arrow_index_i <- arrow_index[i] # change edge color according to an increase or decrease of the flux
        ifelse(calc_change_sum[[i]][1] > 0, edges_Ca$color.color[arrow_index_i] <- "green", edges_Ca$color.color[arrow_index_i] <- "red")
      }
      
    }
    
    visNetworkProxy("network_Ca") %>%
      visUpdateEdges(edges = edges_Ca)
    
  })
  
  # generate UI box by clicking on a node or edge
  
  # observeEvent(input$current_node_id,{
  #   
  #                insertUI(
  #                  selector = "#boxinfo",
  #                  where = "afterEnd",
  #                  ui = box_close(title = "test",
  #                                 solidHeader = TRUE,
  #                                 width = 4,
  #                                 removable = TRUE,
  #                                 img(src ="osa_warning.png"))
  #                )
  #   
  # })
  
  #------------------------------------------------------------------------- 
  #  
  #  Handle dangerous parameter values by the user
  #  
  #-------------------------------------------------------------------------
  
  # prevent the user to put infinite value in the max time of integration
  
  # observeEvent(input$tmax,{ # critical value for tmax
  #   
  #   if (input$tmax > 30000){
  #     
  #     sendSweetAlert(messageId = "failSw", title = "Ooops ...", text = "Invalid parameter value: the maximum time of simulation is too high!", type = "error")
  #     reset("tmax") # value is reset
  #     
  #   }
  #   
  # })
  
  #------------------------------------------------------------------------- 
  #  
  #  Make report of the results and download it: to implement
  #  
  #-------------------------------------------------------------------------
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      #params <- list(n = input$slider)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  #------------------------------------------------------------------------- 
  #  
  #  Useful tasks such as save, reset, load ...
  #  
  #-------------------------------------------------------------------------
  
  # reset all the values of box inputs as well as graphs
  observe({
    
    input$resetAll
    
    reset("boxinput")
    reset("tmax")
    
    edges_Ca <- edges_Ca()
    
    visNetworkProxy("network_Ca") %>%
      visUpdateEdges(edges = edges_Ca)
    
  })
  
  # save and load a session
  
  observeEvent(input$save, {
    values <<- lapply(reactiveValuesToList(input), unclass)
  })
  
  observeEvent(input$load, {
    if (exists("values")) {
      lapply(names(values), function(x) session$sendInputMessage(x, 
                                                                 list(value = values[[x]])))
    }
  })
  
  # Share the state of the App via url bookmarking
  
  observeEvent(input$bookmark, {
    session$doBookmark()
  })
  
  # close the App with the button
  
  observeEvent(input$close, {
    js$closeWindow()
    #stopApp()
  })
  
  
  # When the button is clicked
  # `withBusyIndicatorServer()`
  
  observeEvent(input$save, {
    withBusyIndicatorServer("save", {
      Sys.sleep(1)
    })
  })
  
  observeEvent(input$load, {
    withBusyIndicatorServer("load", {
      Sys.sleep(1)
    })
  })
  
  #session$onSessionEnded(stopApp)  # stop shiny app when the web-window is closed
  
})