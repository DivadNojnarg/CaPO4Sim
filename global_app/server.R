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
  state <- reactive({ c("PTH_g" = 1288.19, "PTH_p" = 0.0687, "D3_p" = 564.2664, "FGF_p" = 16.78112, "Ca_p" = 1.2061,
                        "Ca_f" = 1.8363, "Ca_b" = 250, "PO4_p" = 1.4784, "PO4_f" = 0.7922, "PO4_b" = 90, "PO4_c" = 2.7719,
                        "CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, "CaProt_p" = 1.4518,
                        "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0) })
  
  # Parameters: multiply the real parameter value by the user input. By default, user input = 1 so that parameters are well defined
  
  parameters <- reactive({ c("k_prod_PTHg" = 4.192*input$k_prod_PTHg, "beta_exo_PTHg" = 5.9e-002*input$beta_exo_PTHg,
                             "gamma_exo_PTHg" = 5.8e-002*input$gamma_exo_PTHg, "D3_inact" = 2.5e-005*input$D3_inact, "k_deg_D3" = 1e-003*input$k_deg_D3,
                             "k_prod_FGF" = 6.902e-011*input$k_prod_FGF, "I_Ca" = 2.2e-003*input$I_Ca, "Lambda_ac_Ca" = 5.5e-004*input$Lambda_ac_Ca,
                             "Lambda_ac_P" = 2.75e-004*input$Lambda_ac_Ca, "Lambda_res_min" = 1e-004*input$Lambda_res_min, 
                             "delta_res_max" = 6e-004*input$delta_res_max,
                             "k_p_Ca" = 0.44*input$k_p_Ca, "k_f_Ca" = 2.34e-003*input$k_f_Ca, "I_P" = 1.55e-003*input$I_P, "k_pc" = 0.1875*input$k_pc,
                             "k_cp" = 1e-003*input$k_cp, "k_p_P" = 13.5*input$k_p_P, "k_f_P" = 0.25165*input$k_f_P, "k_fet" = 0.3*input$k_fet,
                             "k_c_CPP" = 3*input$k_c_CPP, "Na" = 142*input$Na, "Prot_tot_p" = 0.6*input$Prot_tot_p, "Vp" = 0.01*input$Vp,
                             "GFR" = 2e-003*input$GFR, "pH" = 7.4, "r" = 4, "a" = 0.8, "b" = 0.2) })
  
  # make a vector of input$parameters, fixed_parameters and calculated parameters
  
  parameters_bis <- reactive({ c(parameters(), parameters_fixed )}) # parameters_calc(input) does not work
  
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
    
    d <- data.frame(id = 1:15,
                    shape = c("image","image","image","image","image","image","image","image","image","image","image","image","image","image","image"), 
                    # square are always scalable
                    image = c("food.svg","intestine.svg","feces.svg","plasma.svg","rapid-bone.svg","bone.svg","kidney.svg","urine.svg",
                              "parathyroid_gland.svg","D3.svg","D3.svg","FGF23.svg","cells.svg","Cap.svg","PO4.svg"),
                    label = c("","","","","","","","","","","","","","",""),
                    #title = out()[nrow(out()),"Ca_p"],
                    #title = c(rep(as.character(as.tags(sparklines::sparkline(out()[nrow(out()),"Ca_p"]))),17)), # does not work
                    #title = paste0(img(src ="bone.png")), # tooltip to display an image
                    x = c(8, 12, 20, 12, 0, 3, 22, 32, 12, 5, 22, 28, 11, 18, 28),
                    y = c(0, 4, 0, 12, 12, 22, 12, 14, 27, 6, 24, 22, 20, 32, 32),
                    color = list(background = "#97C2FC", border = "#97C2FC", highlight = list(background = "orange", border = "orange")),
                    size = c(50,50,50,50,50,50,50,50,50,25,25,25,50,25,25), # rep(50,13)
                    hidden = c(rep(FALSE,15)))
    
  })
  
  edges_Ca <- reactive({
    
    d <- data.frame(from = c(1,2,2,4,5,5,6,7,4,7,9,9,10,11,9,11,11,12,12,7,13,4,7,14,14,15,15,15,7,5,4,5), 
                      to = c(2,3,4,5,4,6,4,4,7,8,6,7,2,9,11,7,12,11,7,7,4,13,4,11,9,9,11,12,8,4,5,6),
                    label = c("Intake", "Fecal Excretion", " Intestinal absorption ", "Rapid Ca storage", "Rapid Ca release", "Ca Flux into bone", 
                              "Resorption", "Ca reabsorption", "filtration", "Urinary Ca excretion","+","","+","-","+","","+","-","",
                              "CaSR (-)","Cells-plasma", "Plasma-cells", "PO4 reabsorption","-","CaSR(-)","+","-","+","Urinary PO4 excretion","Rapid PO4 release",
                              "Rapid PO4 storage","PO4 flux into bone"),
                    id = 1:32,
                    width = 4*c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                    font.size = c(rep(11,10),rep(30,5),11,rep(30,2),rep(11,5),30,11,rep(30,3), rep(11,4)),
                    color = list(color = c(rep("black", 32)), 
                                 highlight = "yellow"),
                    dashes = c(rep(F,10),rep(T,10),rep(F,3),rep(T,5), rep(F,4)),
                    #title = paste("Edge", 1:8),
                    smooth = c(rep(T,32)),
                    length = c(200,200,200,200,300,200,rep(200,22),200,300,200,200),
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
  
  # Events related to Ca and PO4 intake
  
  observe({
    
    input$I_Ca 
    
    isolate({
      
      input$I_P # isolate Ca part from PO4
      
      if (input$I_Ca != 1){ # prevent the graph from moving when I_Ca is reset by reset button
        
        ifelse(input$I_Ca > 1, showNotification("Calcium intake has been increased!", type = "warning", duration = 2), 
               showNotification("Calcium intake has been decreased!", type = "warning", duration = 2)) # ifelse is better!
        
        edges_Ca <- edges_Ca()
        edges_Ca$width[1] <- ifelse(input$I_Ca == 1 , 4*input$I_Ca, 2*input$I_Ca) # prevent arrow from being too big
        
        visNetworkProxy("network_Ca") %>%
          #visFit(nodes = 1, animation = list(duration = 1500, easingFunction
                                             #= "easeInOutQuad")) %>%
          # visFocus(id = 1, scale = 2, offset = list(x = 0, y = 0), locked = TRUE,
          #          animation = list(duration = 1500, easingFunction = "easeInOutQuad")) %>% # Zoom on the intake area
          visSetSelection(edgesId = 1) %>% # If I_Ca is changed, then arrow becomes yellow to signal the user what is changed and the width change
          visUpdateEdges(edges = edges_Ca) 
      }
      
    })
    
    input$I_P
    
    isolate({
      
      input$I_Ca # isolate PO4 part from Ca
      
      if (input$I_P != 1){ # prevent the graph from moving when I_P is reset by reset button
        
        ifelse(input$I_P > 1, showNotification("Phosphate intake has been increased!", type = "warning", duration = 2), 
               showNotification("Phosphate intake has been decreased!", type = "warning", duration = 2)) # ifelse is better!
        
        edges_Ca <- edges_Ca()
        edges_Ca$width[1] <- ifelse(input$I_P == 1 , 4*input$I_P, 2*input$I_P)
        
        visNetworkProxy("network_Ca") %>%
          #visFit(nodes = 1, animation = list(duration = 1500, easingFunction
                                             #= "easeInOutQuad")) %>%
          # visFocus(id = 1, scale = 2, offset = list(x = 0, y = 0), locked = TRUE,
          #          animation = list(duration = 1500, easingFunction = "easeInOutQuad")) %>% # Zoom on the intake area
          visSetSelection(edgesId = 1) %>% # If I_Ca is changed, then arrow becomes yellow to signal the user what is changed and the width change
          visUpdateEdges(edges = edges_Ca) 
      }
      
    })
    
  })
  
  # Events related to Ca and PO4 flux into bone
  
  observe({
    
    input$Lambda_ac_Ca
    
    if (input$Lambda_ac_Ca != 1){
      
      ifelse(input$Lambda_ac_Ca > 1, showNotification("Calcium flux into bone has been increased!", type = "warning", duration = 2), 
             showNotification("Calcium flux into bone has been decreased!", type = "warning", duration = 2))
      
      edges_Ca <- edges_Ca()
      edges_Ca$width[6] <- ifelse(input$Lambda_ac_Ca == 1 , 4*input$Lambda_ac_Ca, 2*input$Lambda_ac_Ca)
      
      visNetworkProxy("network_Ca") %>%
        #visFit(nodes = 6, animation = list(duration = 1500, easingFunction
                                           #= "easeInOutQuad")) %>%
        visSetSelection(edgesId = 6) %>%
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  # Events related to Ca and PO4 storage in the rapid bone pool
  
  observe({
    
    input$k_p_Ca
    
    isolate({
      
      input$k_p_P
    
    if (input$k_p_Ca != 1){
      
      ifelse(input$k_p_Ca > 1, showNotification("Calcium storage in the bone rapid pool has been increased!", type = "warning", duration = 2), 
             showNotification("Calcium storage in the bone rapid pool has been decreased!", type = "warning", duration = 2))
      
      edges_Ca <- edges_Ca()
      edges_Ca$width[4] <- 4*input$k_p_Ca
      
      visNetworkProxy("network_Ca") %>%
        #visFit(nodes = 5, animation = list(duration = 1500, easingFunction
                                                #= "easeInOutQuad")) %>%
        visSetSelection(edgesId = 4) %>%
        visUpdateEdges(edges = edges_Ca)
      
    }
      
    })
    
    input$k_p_P
    
    isolate({
      
      input$k_p_Ca
      
      if (input$k_p_P != 1){
        
        ifelse(input$k_p_P > 1, showNotification("PO4 storage in the bone rapid pool has been increased!", type = "warning", duration = 2), 
               showNotification("PO4 storage in the bone rapid pool has been decreased!", type = "warning", duration = 2))
        
        edges_Ca <- edges_Ca()
        edges_Ca$width[4] <- 4*input$k_p_P
        
        visNetworkProxy("network_Ca") %>%
          #visFit(nodes = 5, animation = list(duration = 1500, easingFunction
                                             #= "easeInOutQuad")) %>%
          visSetSelection(edgesId = 4) %>%
          visUpdateEdges(edges = edges_Ca)
        
      }
      
    })
    
  })
  
  # Events related to Ca and PO4 release from the rapid bone pool
  
  observe({
    
    input$k_f_Ca
    
    isolate({
      
      input$k_f_P
    
    if (input$k_f_Ca != 1){
      
      ifelse(input$k_f_Ca > 1, showNotification("Calcium release from the bone rapid pool has been increased!", type = "warning", duration = 2), 
             showNotification("Calcium release from the bone rapid pool has been decreased!", type = "warning", duration = 2))
      
      edges_Ca <- edges_Ca()
      edges_Ca$width[5] <- 4*input$k_f_Ca
      
      visNetworkProxy("network_Ca") %>%
        #visFit(nodes = 5, animation = list(duration = 1500, easingFunction
                                           #= "easeInOutQuad")) %>%
        visSetSelection(edgesId = 5) %>%
        visUpdateEdges(edges = edges_Ca)
      
    }
      
    })
    
    input$k_f_P
    
    isolate({
      
      input$k_f_Ca
      
      if (input$k_f_P != 1){
        
        ifelse(input$k_f_P > 1, showNotification("PO4 release from the bone rapid pool has been increased!", type = "warning", duration = 2), 
               showNotification("PO4 release from the bone rapid pool has been decreased!", type = "warning", duration = 2))
        
        edges_Ca <- edges_Ca()
        edges_Ca$width[5] <- 4*input$k_f_P
        
        visNetworkProxy("network_Ca") %>%
          #visFit(nodes = 5, animation = list(duration = 1500, easingFunction
                                             #= "easeInOutQuad")) %>%
          visSetSelection(edgesId = 5) %>%
          visUpdateEdges(edges = edges_Ca)
        
      }
      
    })
    
  })
  
  # Events related to Ca and PO4 resorption from deep bone
  
  observe({
    
    input$Lambda_res_min
    input$delta_res_max
    
    if (input$Lambda_res_min != 1 || input$delta_res_max != 1){
      
      ifelse(input$Lambda_res_min > 1 || input$delta_res_max > 1, 
             showNotification("Calcium release from the bone (resorption) has been increased!", type = "warning", duration = 2),
             showNotification("Calcium release from the bone (resorption) has been decreased!", type = "warning", duration = 2))
      
      edges_Ca <- edges_Ca()
      edges_Ca$width[7] <- ifelse(input$Lambda_res_min == 1 , 4*input$Lambda_res_min, 2*input$Lambda_res_min)
      
      visNetworkProxy("network_Ca") %>%
        #visFit(nodes = c(4,6), animation = list(duration = 1500, easingFunction
                                           #= "easeInOutQuad")) %>%
        visSetSelection(edgesId = 7) %>%
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  # Events related to GFR changes
  
  observe({
    
    input$GFR
    
    if (input$GFR != 1){
      
      ifelse(input$GFR > 1, showNotification("GFR has been increased!", type = "warning", duration = 2), 
             showNotification("GFR has been decreased!", type = "warning", duration = 2))
      
      edges_Ca <- edges_Ca()
      edges_Ca$width[9] <- 4*input$GFR
      
      visNetworkProxy("network_Ca") %>%
        #visFit(nodes = c(4,7), animation = list(duration = 1500, easingFunction
                                           #= "easeInOutQuad")) %>%
        visSetSelection(edgesId = 9) %>%
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  # Events related to PO4 fluxes in and out cells
  
  # Events related to Ca and PO4 release from the rapid bone pool
  
  observe({
    
    input$k_pc
    
    isolate({
      
      input$k_cp
      
      if (input$k_pc != 1){
        
        ifelse(input$k_pc > 1, showNotification("PO4 storage into cells has been increased!", type = "warning", duration = 2), 
               showNotification("PO4 storage into cells has been decreased!", type = "warning", duration = 2))
        
        edges_Ca <- edges_Ca()
        edges_Ca$width[22] <- 4*input$k_pc
        
        visNetworkProxy("network_Ca") %>%
          #visFit(nodes = 13, animation = list(duration = 1500, easingFunction
                                             #= "easeInOutQuad")) %>%
          visSetSelection(edgesId = 22) %>%
          visUpdateEdges(edges = edges_Ca)
        
      }
      
    })
    
    input$k_cp
    
    isolate({
      
      input$k_pc
      
      if (input$k_cp != 1){
        
        ifelse(input$k_cp> 1, showNotification("PO4 release from cells to plasma has been increased!", type = "warning", duration = 2), 
               showNotification("PO4 release from cells to plasma has been decreased!", type = "warning", duration = 2))
        
        edges_Ca <- edges_Ca()
        edges_Ca$width[21] <- 4*input$k_cp
        
        visNetworkProxy("network_Ca") %>%
          #visFit(nodes = 13, animation = list(duration = 1500, easingFunction
                                             #= "easeInOutQuad")) %>%
          visSetSelection(edgesId = 21) %>%
          visUpdateEdges(edges = edges_Ca)
        
      }
      
    })
    
  })
  
  # Events related to PTH parameters
  
  observe({
    
    input$k_prod_PTHg
    
    if (input$k_prod_PTHg != 1){
      
      ifelse(input$k_prod_PTHg> 1, showNotification("PTH synthesis has been increased!", type = "warning", duration = 2), 
             showNotification("PTH synthesis has been decreased!", type = "warning", duration = 2))
      
      edges_Ca <- edges_Ca()
      edges_Ca$width[c(11,12,15)] <- ifelse(input$k_prod_PTHg == 1 , 4*input$k_prod_PTHg, 0.2*input$k_prod_PTHg)
      
      visNetworkProxy("network_Ca") %>%
        #visFit(nodes = 9, animation = list(duration = 1500, easingFunction
                                            #= "easeInOutQuad")) %>%
        visSetSelection(edgesId = c(11,12,15)) %>%
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  # Events related to D3 parameters
  
  observe({
    
    input$D3_inact
    
    isolate({
      
      input$k_deg_D3
      
      if (input$D3_inact != 1){
        
        ifelse(input$D3_inact> 1, showNotification("25(OH)D stock has been increased!", type = "warning", duration = 2), 
               showNotification("25(OH)D stock has been decreased!", type = "warning", duration = 2))
        
        edges_Ca <- edges_Ca()
        edges_Ca$width[c(13,14,16,17)] <- ifelse(input$D3_inact == 1 , 4*input$D3_inact, 0.2*input$D3_inact)
        
        visNetworkProxy("network_Ca") %>%
          #visFit(nodes = 11, animation = list(duration = 1500, easingFunction
                                              #= "easeInOutQuad")) %>%
          visSetSelection(edgesId = c(13,14,16,17)) %>%
          visUpdateEdges(edges = edges_Ca)
        
      }
      
    })
    
    input$k_deg_D3
    
    isolate({
      
      input$D3_inact
      
      if (input$k_deg_D3 != 1){
        
        ifelse(input$k_deg_D3> 1, showNotification("Vitamin D3 degradation has been increased!", type = "warning", duration = 2), 
               showNotification("Vitamin D3 degradation stock has been decreased!", type = "warning", duration = 2))
        
        edges_Ca <- edges_Ca()
        edges_Ca$width[c(13,14,16,17)] <- ifelse(input$k_deg_D3 == 1 , 4*input$k_deg_D3, 0.2*input$k_deg_D3)
        
        visNetworkProxy("network_Ca") %>%
          #visFit(nodes = 11, animation = list(duration = 1500, easingFunction
                                              #= "easeInOutQuad")) %>%
          visSetSelection(edgesId = c(13,14,16,17)) %>%
          visUpdateEdges(edges = edges_Ca)
        
      }
      
    })
    
  })
  
  # Events related to FGF parameters
  
  observe({
    
    input$k_prod_FGF
    
    if (input$k_prod_FGF != 1){
      
      ifelse(input$k_prod_FGF> 1, showNotification("FGF23 synthesis has been increased!", type = "warning", duration = 2), 
             showNotification("FGF23 synthesis has been decreased!", type = "warning", duration = 2))
      
      edges_Ca <- edges_Ca()
      edges_Ca$width[c(18,19)] <- ifelse(input$k_prod_FGF == 1 , 4*input$k_prod_FGF, 2*input$k_prod_FGF)
      
      visNetworkProxy("network_Ca") %>%
        #visFit(nodes = 12, animation = list(duration = 1500, easingFunction
                                           #= "easeInOutQuad")) %>%
        visSetSelection(edgesId = c(18,19)) %>%
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  # Change arrow color relatively to the value of fluxes
  
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

    out <- out()
    edges_Ca <- edges_Ca()

    calc_change_t <- calc_change(out)
    calc_change_t$X <- NULL # remove column X

    path_to_calc_change_base <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/global_app/www/calc_change_base.csv" # only run in local
    #path_to_calc_change_base <- "/srv/shiny-server/capApp/global_app/www/calc_change_base.csv" # only works on the linux server
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
  #  Notification events to explain the user how to play with the app
  #
  #-------------------------------------------------------------------------
  
  # Show a welcome notification in the menu bar part
  
  observe({ 
    
    if(input$notif_switch == "TRUE"){
      
      showNotification( 
        id = "menu_notif",
        "In this panel you can enable/disable notifications, bookmark the state of your app to share it with colleagues, save it, load the last state you saved and change
        the global theme.",
        duration = 9999, # sufficient amount of time
        closeButton = TRUE,
        type = "error")
      
      #jqui_draggable('menu_notif', options = list(grid = c(80, 80))) # does not work
      
    }
    else{ # this notifications can be removed at anytime
      
      removeNotification(id = "menu_notif", session)
      
    }
    
  })
  
  # Show a welcome notification in the graph part
  
  observe({ 
    
    if(input$notif_switch == "TRUE"){
      
      showNotification( 
        id = "graph_notif",
        "In this panel are displayed the graph of CaPO4 homeostasis. To see results, start by clicking on a node and/or an edge on the interactive diagram.
        The value of tmax which is the maximum time of simulation can be increased or decreased as required (but higher than 0).",
        duration = 9999, # sufficient amount of time
        closeButton = TRUE,
        type = "error")
      
    }
    else{ # this notifications can be removed at anytime
      
      removeNotification(id = "graph_notif", session)
      
    }
    
  })
  
  # Show a welcome notification in the diagram area 
  
  observe({ 
    
    if(input$notif_switch == "TRUE"){
      
      showNotification( 
        id = "diagram_notif",
        "In this panel is displayed the interactive diagram (see legend). Basically, when a parameter is changed in the control center 
        initial perturbations are shown in yellow. The arrow size increases if
        it is a stimulatory effect and inversely. Fluxes are shown in red if they decrease or in green if they are enhanced. Colors correspond to the final state of the system
        (which is the value of tmax in minutes).",
        duration = 9999,
        closeButton = TRUE,
        type = "error")
    }
    else{
      
      removeNotification(id = "diagram_notif", session)
      
    }
    
  })
  
  # Show a welcome notification in the control center 
  
  observe({ 
    
    if(input$notif_switch == "TRUE"){
      
      showNotification( 
        id = "control_notif",
        "In this panel you can select several parameters and change their value using sliders.",
        #sliderInput("useless_slider", "", min = 0, max = 100, value = 50), # possible to add everything inside
        duration = 9999,
        closeButton = TRUE,
        type = "error") # important so in red
      
    }
    else{
      
      removeNotification(id = "control_notif", session)
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Handle dangerous parameter values by the user
  #  
  #-------------------------------------------------------------------------
  
  observeEvent(input$beta_exo_PTHg,{ # critical value for PTHg
    
    if (input$beta_exo_PTHg < 0.9){
      
      sendSweetAlert(messageId = "failSw", title = "Ooops ...", text = "Invalid parameter value", type = "error")
      reset("beta_exo_PTHg") # value is reset
      
    }
    
  })
  
  # prevent the user to put infinite value in the max time of integration
  
  observeEvent(input$tmax,{ # critical value for tmax
    
    if (input$tmax > 30000){
      
      sendSweetAlert(messageId = "failSw", title = "Ooops ...", text = "Invalid parameter value: the maximum time of simulation is too high!", type = "error")
      reset("tmax") # value is reset
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Generate a chat for users (with ygdashboard import)
  #
  #-------------------------------------------------------------------------
  
  # create_dynamicChatMessage <- reactive({
  #   
  #   input$chatBtn
  #   
  #   # we would need a global allChatMessage to keep history of chat
  #   
  #   # set default
  #   user_name <- "Ex Machina"
  #   message_pos <- "right"
  #   
  #   # parse user
  #   query <- parseQueryString(session$clientData$url_search)
  # 
  #   allChatMessage <- list(
  #     chatMessage(
  #       name = user_name,
  #       text = input$chatText,
  #       position = message_pos,
  #       timestamp = Sys.time()
  #     )
  #   )
  #   
  #   return(allChatMessage)
  #   
  # })
  # 
  # output$dynamicChatMessage <- renderChatMessage({ create_dynamicChatMessage() })
  
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
  #  Prevent user from selecting multiple boxes of parameters
  #  
  #-------------------------------------------------------------------------

  observeEvent(input$PTHgroup, {
    # Change the following line for more examples
    if (input$PTHgroup == "TRUE"){
      disable("D3group")
      disable("FGFgroup")
      disable("Cagroup")
      disable("PO4group")
      disable("CaPO4group")
      disable("others")
    }else{
      enable("D3group")
      enable("FGFgroup")
      enable("Cagroup")
      enable("PO4group")
      enable("CaPO4group")
      enable("others")
    }
  })
  
  observeEvent(input$D3group, {
    # Change the following line for more examples
    if (input$D3group == "TRUE"){
      disable("PTHgroup")
      disable("FGFgroup")
      disable("Cagroup")
      disable("PO4group")
      disable("CaPO4group")
      disable("others")
    }else{
      enable("PTHgroup")
      enable("FGFgroup")
      enable("Cagroup")
      enable("PO4group")
      enable("CaPO4group")
      enable("others")
    }
  })
  
  observeEvent(input$FGFgroup, {
    # Change the following line for more examples
    if (input$FGFgroup == "TRUE"){
      disable("D3group")
      disable("PTHgroup")
      disable("Cagroup")
      disable("PO4group")
      disable("CaPO4group")
      disable("others")
    }else{
      enable("D3group")
      enable("PTHgroup")
      enable("Cagroup")
      enable("PO4group")
      enable("CaPO4group")
      enable("others")
    }
  })
  
  observeEvent(input$Cagroup, {
    # Change the following line for more examples
    if (input$Cagroup == "TRUE"){
      disable("D3group")
      disable("FGFgroup")
      disable("PTHgroup")
      disable("PO4group")
      disable("CaPO4group")
      disable("others")
    }else{
      enable("D3group")
      enable("FGFgroup")
      enable("PTHgroup")
      enable("PO4group")
      enable("CaPO4group")
      enable("others")
    }
  })
  
  observeEvent(input$PO4group, {
    # Change the following line for more examples
    if (input$PO4group == "TRUE"){
      disable("D3group")
      disable("FGFgroup")
      disable("Cagroup")
      disable("PTHgroup")
      disable("CaPO4group")
      disable("others")
    }else{
      enable("D3group")
      enable("FGFgroup")
      enable("Cagroup")
      enable("PTHgroup")
      enable("CaPO4group")
      enable("others")
    }
  })
  
  observeEvent(input$CaPO4group, {
    # Change the following line for more examples
    if (input$CaPO4group == "TRUE"){
      disable("D3group")
      disable("FGFgroup")
      disable("Cagroup")
      disable("PO4group")
      disable("PTHgroup")
      disable("others")
    }else{
      enable("D3group")
      enable("FGFgroup")
      enable("Cagroup")
      enable("PO4group")
      enable("PTHgroup")
      enable("others")
    }
  })
  
  observeEvent(input$others, {
    # Change the following line for more examples
    if (input$others == "TRUE"){
      disable("D3group")
      disable("FGFgroup")
      disable("Cagroup")
      disable("PO4group")
      disable("CaPO4group")
      disable("PTHgroup")
    }else{
      enable("D3group")
      enable("FGFgroup")
      enable("Cagroup")
      enable("PO4group")
      enable("CaPO4group")
      enable("PTHgroup")
    }
  })
  
  
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
      #visFocus(id = 4, scale = 1, offset = list(x = 0, y = 0), locked = TRUE,
      #animation = list(duration = 1500, easingFunction = "easeOutQuad")) %>%
      #visFit(nodes = NULL, animation = list(duration = 1500, easingFunction
                                            #= "easeInOutQuad")) %>%
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