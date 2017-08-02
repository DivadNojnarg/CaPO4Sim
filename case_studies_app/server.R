#-------------------------------------------------------------------------
#  This application is a R-Shiny implementation of a calcium and phosphate 
#  homeostasis model. It aims at being used by medical students but also
#  researchers. See https://divadnojnarg.github.io for more informations
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------


library(shiny)
require(visNetwork)
library(shinyBS)
library(Rcpp)
library(dplyr)
library(plotly)
library(deSolve)
#library(timevis)
#library(lubridate)

source("calcium_phosphate_Caiv.R")
source("calcium_phosphate_PO4iv.R")
source("calcium_phosphate_PO4gav.R")
source("cap_fixed_parameters.R")
source("cap_calc_parameters.R")
source("calc_change.R")

path_to_images <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/www"
#path_to_images <- "/srv/shiny-server/capApp/case_studies_app/www"

shinyServer(function(input, output, session) {
  
  
  #------------------------------------------------------------------------- 
  #  Store times, state and parameters in reactive values that can
  #  react to user inputs
  #  
  #-------------------------------------------------------------------------
  
  # Basic reactive expressions needed by the solver
  
  state <- reactive({ c("PTH_g" = 1288.19, "PTH_p" = 0.0687, "D3_p" = 564.2664, "FGF_p" = 16.78112, "Ca_p" = 1.2061,
                        "Ca_f" = 1.8363, "Ca_b" = 250, "PO4_p" = 1.4784, "PO4_f" = 0.7922, "PO4_b" = 90, "PO4_c" = 2.7719,
                        "CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, "CaProt_p" = 1.4518,
                        "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0) })
  
  # Parameters: multiply the real parameter value by the user input. By default, user input = 1 so that parameters are well defined
  
  parameters <- reactive({ c("k_prod_PTHg" = 4.192, "beta_exo_PTHg" = 5.9e-002,
                             "gamma_exo_PTHg" = 5.8e-002, "D3_inact" = 2.5e-005, "k_deg_D3" = 1e-003,
                             "k_prod_FGF" = 6.902e-011, "I_Ca" = 2.2e-003, "Lambda_ac_Ca" = 5.5e-004,
                             "Lambda_ac_P" = 2.75e-004, "Lambda_res_min" = 1e-004, 
                             "delta_res_max" = 6e-004, "k_p_Ca" = 0.44, "k_f_Ca" = 2.34e-003, "I_P" = 1.55e-003, "k_pc" = 0.1875,
                             "k_cp" = 1e-003, "k_p_P" = 13.5, "k_f_P" = 0.25165, "k_fet" = 0.3,
                             "k_c_CPP" = 3, "Na" = 142, "Prot_tot_p" = 0.6, "Vp" = 0.01,
                             "GFR" = 2e-003, "pH" = 7.4, "r" = 4, "a" = 0.8, "b" = 0.2) })
  
  # make a vector of input$parameters, fixed_parameters and calculated parameters
  
  parameters_bis <- reactive({ c(parameters(), parameters_fixed )}) # parameters_calc(input) does not work
  
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Integrate equations using deSolve package to generate table.
  #  out is a reactive intermediate component that is called by
  #  to make plots or other stuffs
  #
  #-------------------------------------------------------------------------
  
  out <- reactive({
    
    parameters_bis <- parameters_bis()
    state <- state()
    
    if(input$Ca_inject == "TRUE"){ # IV Ca injection followed by EGTA infusion
      times <- seq(0,input$tmax,by=1)
      out <- as.data.frame(ode(y = state, times = times, func = calcium_phosphate_Caiv, parms = parameters_bis))
    }
    else if(input$PO4_inject == "TRUE"){ # PO4 injection 
      times <- seq(0,input$tmaxbis,by=1) 
      out <- as.data.frame(ode(y = state, times = times, func = calcium_phosphate_PO4iv, parms = parameters_bis))
    }
    else if(input$PO4_gav == "TRUE"){ # PO4 gavage
      times <- seq(0,input$tmaxtris,by=1)
      out <- as.data.frame(ode(y = state, times = times, func = calcium_phosphate_PO4gav, parms = parameters_bis))
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
  #  Events for the CaPO4 Homeostasis diagramm during php1
  #  
  #-------------------------------------------------------------------------
  
  counter_nav <- reactiveValues(diagram = 0) # create a navigation counter to trigger sequential graph animation
  
  observeEvent(input$back1,{ # counter decrease
    
    if(counter_nav$diagram == 0){

    }
    else{counter_nav$diagram <- counter_nav$diagram - 1}
    
  })
  
  observeEvent(input$next1,{ # counter incrementation
    
      counter_nav$diagram <- counter_nav$diagram + 1
    
  })
  
  observeEvent(input$next1,{ # reset the counter if higher than 5
    
    if(counter_nav$diagram > 5){
      
      counter_nav$diagram <- 0
      
      edges_Ca <- edges_Ca()
      
      edges_Ca$color <- "black"
      edges_Ca$witdh <- 4
      visNetworkProxy("network_Ca", session) %>%  # then reset the graph
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  # Events when php1 is selected
  
  observeEvent(input$next1 | input$back1,{ # primary perturbation
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 1 && input$run_php1 == "TRUE"){ # need && input$run_php1 == "TRUE" since Ca_inject also use back and next buttons
    
    edges_Ca$color.color[c(11,12,15)] <- "yellow" # perturbation
    edges_Ca$width[c(11,12,15)] <- 8
    
    visNetworkProxy("network_Ca", session) %>%  
      visUpdateEdges(edges = edges_Ca)
    
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 2 && input$run_php1 == "TRUE"){ # secondary hormonal regulation (vitamin D3)
      
      edges_Ca$color.color[c(11,12,15)] <- "khaki" # previous arrows are less bright
      # edges_Ca$color.opacity[c(11,12,15)] <- 0
      
      edges_Ca$color.color[c(13,14,16,17)] <- "yellow"
      edges_Ca$width[c(13,14,16,17)] <- 8
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # FGF23 hormonal regulation
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 3 && input$run_php1 == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(18,19)] <- "yellow"
      edges_Ca$width[c(18,19)] <- 8
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # Fluxes are colored
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 4 && input$run_php1 == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(3,4,5,6,7,8,10,29)] <- "green" # increased fluxes
      edges_Ca$color.color[c(21,22,23,30,31,32)] <- "red" # decreased fluxesd
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # Ca and PO4 final retrocontrol
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 5 && input$run_php1 == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(3,4,5,6,7,8,10,29)] <- "green" # increased fluxes
      edges_Ca$color.color[c(21,22,23,30,31,32)] <- "red" # decreased fluxesd
      
      edges_Ca$color.color[c(20,24,25,26,27,28)] <- "yellow"
      edges_Ca$width[c(20,24,25)] <- 8 # Ca
      edges_Ca$width[c(26,27,28)] <- 2 # PO4
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$back1,{ # reset diagram when back is clicked
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram == 0 && input$run_php1 == "TRUE"){ # reset if counter_diagram is 0
      
      edges_Ca$color <- "black"
      edges_Ca$witdh <- 4
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Events for the CaPO4 Homeostasis diagramm during hypoparathyroidism
  #  
  #-------------------------------------------------------------------------
  
  # Events when hypopara is selected: basically it is the reverse case of php1
  
  observeEvent(input$next1 | input$back1,{ # primary perturbation
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 1 && input$run_hypopara == "TRUE"){ # need && input$run_hypopara == "TRUE" since Ca_inject also use back and next buttons
      
      edges_Ca$color.color[c(11,12,15)] <- "yellow" # perturbation
      edges_Ca$width[c(11,12,15)] <- 2 # decreased size compared to php1
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 2 && input$run_hypopara == "TRUE"){ # secondary hormonal regulation (vitamin D3)
      
      edges_Ca$color.color[c(11,12,15)] <- "khaki" # previous arrows are less bright
      # edges_Ca$color.opacity[c(11,12,15)] <- 0
      
      edges_Ca$color.color[c(13,14,16,17)] <- "yellow"
      edges_Ca$width[c(13,14,16,17)] <- 2
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # FGF23 hormonal regulation
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 3 && input$run_hypopara == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(18,19)] <- "yellow"
      edges_Ca$width[c(18,19)] <- 2
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # Fluxes are colored
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 4 && input$run_hypopara == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(21,22,23,30,31,32)] <- "green"
      edges_Ca$color.color[c(3,4,5,6,7,8,10,29)] <- "red"
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # Ca and PO4 final retrocontrol
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 5 && input$run_hypopara == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(21,22,23,30,31,32)] <- "green"
      edges_Ca$color.color[c(3,4,5,6,7,8,10,29)] <- "red"
      
      edges_Ca$color.color[c(20,24,25,26,27,28)] <- "yellow"
      edges_Ca$width[c(20,24,25)] <- 2 # Ca
      edges_Ca$width[c(26,27,28)] <- 8 # PO4
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$back1,{ # reset diagram
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram == 0 && input$run_hypopara == "TRUE"){ # reset if counter_diagram is 0
      
      edges_Ca$color <- "black"
      edges_Ca$witdh <- 4
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Events for the CaPO4 Homeostasis diagramm during vitamin D3 deficiency
  #  
  #-------------------------------------------------------------------------
  
  # Events when hypoD3 is selected
  
  observeEvent(input$next1 | input$back1,{
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 1 && input$run_hypoD3 == "TRUE"){ # primary perturbation
      
      edges_Ca$color.color[c(13,14,16,17)] <- "yellow"
      edges_Ca$width[c(13,14,16,17)] <- 2
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # PTH
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 2 && input$run_hypoD3 == "TRUE"){ # need && input$run_hypopara == "TRUE" since Ca_inject also use back and next buttons
      
      edges_Ca$color.color[c(13,14,16,17)] <- "khaki"
      edges_Ca$color.color[c(11,12,15)] <- "yellow" # perturbation
      edges_Ca$width[c(11,12,15)] <- 8 # decreased size compared to php1
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  
  
  observeEvent(input$next1 | input$back1,{ # FGF23 hormonal regulation
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 3 && input$run_hypoD3 == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(18,19)] <- "yellow"
      edges_Ca$width[c(18,19)] <- 2
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # Fluxes are colored
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 4 && input$run_hypoD3 == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(3,4,5,6,7,8,10,21,22,23,29,30,31,32)] <- "red"
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$next1 | input$back1,{ # Ca and PO4 final retrocontrol
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram >= 5 && input$run_hypoD3 == "TRUE"){ # secondary hormonal regulation
      
      edges_Ca$color.color[c(11,12,13,14,15,16,17,18,19)] <- "khaki" # previous arrows are less bright
      
      edges_Ca$color.color[c(3,4,5,6,7,8,10,21,22,23,29,30,31,32)] <- "red"
      
      edges_Ca$color.color[c(20,24,25,26,27,28)] <- "yellow"
      edges_Ca$width[c(20,24,25)] <- 2 # Ca
      edges_Ca$width[c(26,27,28)] <- 2 # PO4
      
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
    }
    
  })
  
  observeEvent(input$back1,{ # reset diagram
    
    counter_nav$diagram
    
    edges_Ca <- edges_Ca()
    
    if(counter_nav$diagram == 0 && input$run_hypoD3 == "TRUE"){ # reset if counter_diagram is 0
      
      edges_Ca$color <- "black"
      edges_Ca$witdh <- 4
      visNetworkProxy("network_Ca", session) %>%  
        visUpdateEdges(edges = edges_Ca)
      
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
    
    calc_change_t <- calc_change(out)
    calc_change_t$X <- NULL # remove column X
    
    # calculate the difference between live fluxes and base-case values
    index <- c(3,10,29,7,6,32,8,23,4,31,5,30,22,21) # index of arrows in the graph (which are fluxes and not regulations)
    calc_change_t <- rbind(calc_change_t, index)
    
    flux_changed_index <- which(calc_change_t[1,] != 0) # calculate which element in the sum table is different of 0 and store the index
    arrow_index <- as.numeric(t(calc_change_t[2,flux_changed_index])) # convert to arrow index in the interactive diagramm
    
    if(!is.null(flux_changed_index)){
      for (i in (1:ncol(calc_change_t))){
        arrow_index_i <- arrow_index[i] # change edge color according to an increase or decrease of the flux
        ifelse(calc_change_t[[i]][1] > 0, edges_Ca$color.color[arrow_index_i] <- "green", edges_Ca$color.color[arrow_index_i] <- "red")
      }
      
    }
    
    if(input$PO4_gav){ # PO4 gavage
      
      edges_Ca$color.color[1] <- "yellow" # perturbation
      edges_Ca$width[1] <- 8
      
    }
    if(input$Ca_inject){
      if(input$tmax<60){ # Ca infusion
        
        edges_Ca$color.color[c(20,24,25,33)] <- "yellow" # perturbation
        edges_Ca$width[c(20,24,25,33)] <- 8
        
      }
      else{ # EGTA infusion
        
        edges_Ca$color.color[c(20,24,25,35)] <- "yellow" # perturbation
        edges_Ca$width[c(20,24,25)] <- 2
        edges_Ca$width[35] <- 8
        
      }
    }
    if(input$PO4_inject){ # PO4 injection
      edges_Ca$color.color[c(26,27,28)] <- "yellow" # perturbation
      edges_Ca$width[c(26,27,28)] <- 8
      
      if(input$tmaxbis <= 3){
        
        edges_Ca$color.color[34] <- "yellow" # perturbation
        edges_Ca$width[34] <- 8
        
      }
      
    }
    
    visNetworkProxy("network_Ca") %>%
      visUpdateEdges(edges = edges_Ca)
    
  })
  
  
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
        "In this panel are displayed the graphs of each simulation. To see results, start by clicking on a case study in the control center. Each graph can be saved as png.",
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
        "In this panel will be displayed the interactive diagram. It is not available at start, since some simulations require to fulfill a start quiz in order to evaluate your
         entry level. You can navigate then by clicking on next or back buttons, to see animations. Basically, initial perturbations are shown in yellow. The arrow size increases if
         it is a stimulatory effect and inversely. Fluxes are shown in red if they decrease or in green if they are enhanced. Color corresponds to the final state of the system
        (see graph on the right).",
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
        "In this panel you can select several case studies. On the right, the information sign can give you further details about the experimental conditions
      of simulations. When you select Calcium infusion, PO4 injection or PO4 gavage, a slider input bar will be displayed to enable you to control the current time.",
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
  #  Notification events for PHP1
  #
  #-------------------------------------------------------------------------
  
  # When the PHP1 quiz is finished, show a notification explaining what will happen for the current simulation
  
  observe({
    
    if(!is.null(counter_quiz$php1) && counter_nav$diagram == 0 && input$notif2_switch == "TRUE"){
    
    showNotification( 
      id = "php11_notif",
      "PTH synthesis is increased in parathyroid gland, mainly caused by a tumor. 
      Hover on PTH node in the diagram to have more details. You can click on the info button 
      (on the right of each checkbox in the control center), to have more information about the current simulation. To launch the simulation,
      click on next button below, and on back if you want to go back.", 
      duration = 9999, 
      closeButton = TRUE,
      type = "message")
      
    }
    else{
      
      removeNotification(id = "php11_notif", session)
      
    }
    
  })
  
  observe({
    
    if(!is.null(counter_quiz$php1) && counter_nav$diagram == 1 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "php12_notif",
        "PTH increases bone resorption, Ca reabsorption (in TAL and DCT/CNT ) and enhances vitamin D3 conversion into its active form. Furthermore, 
        it represses PO4 reabsorption in the proximal tubule", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "php12_notif", session)
      
    }
    
  })
  
  # PHP1: when next is clicked, show D3 notification
  
  observe({
    
    if(!is.null(counter_quiz$php1) && counter_nav$diagram == 2 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "php13_notif",
        "1,25 vitamin D3 synthesis is enhanced by PTH, which stimulates Cyp27b1 and inhibits Cyp24a1. Active vitamin D3 increases
        intestinal absorption of Ca and PO4, Ca reabsorption in kidney (DCT/CNT) as well as FGF23 synthesis in bone. Besides, it represses PTH synhtesis in parathyroid glands.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "php13_notif", session)
      
    }
    
  })
  
  # PHP1: when next is clicked, show FGF23 notification
  
  observe({
    
    if(!is.null(counter_quiz$php1) && counter_nav$diagram == 3 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "php14_notif",
        "FGF23 synthesis is mainly enhanced by vitamin D3 and to a lesser extent by PO4. Its main role is to prevent vitamin D3 toxicity by repressing its synthesis (activates 
        Cyp24a1 and represses Cyp27b1, so antagonist of PTH). Additionally, it blunts PO4 reasbsorption in the proximal tubule. The relationship between PTH and FGF23
        are still controversial, to date.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "php14_notif", session)
      
    }
    
  })
  
  # PHP1: when next is clicked, show Ca and PO4 notification
  
  observe({
    
    if(!is.null(counter_quiz$php1) && counter_nav$diagram == 5 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "php15_notif",
        "Ionized plasma Ca concentration is widely increased during primary hyperparathyroidism, mainly because of an higher intestinal absorption, resorption and reabsorption 
        in kidney. Inversely, PO4 plasma concentration is significantly reduced as a consequence of PTH and FGF23 effects on its renal reabsorption. Ca and PO4 are also known to
         regulate hormonal synthesis, for example via the CaSR in parathyroid glands.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "php15_notif", session)
      
    }
    
  })
  
  # Show description of k_prod_PTHg in the graph part
  
  observe({
    
    if(!is.null(counter_quiz$php1) && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "php1_xaxis_notif",
        "The higher the value of k_prod_PTHg, the more severe primary hyperparathyroidism.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "warning")
      
    }
    else{
      
      removeNotification(id = "php1_xaxis_notif", session)
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Notification events for hypopara
  #
  #-------------------------------------------------------------------------
  
  # When the HYPOPARA quiz is finished, show a notification explaining what will happen for the current simulation
  
  observe({
    
    if(!is.null(counter_quiz$hypopara) && counter_nav$diagram <= 1 && input$notif2_switch == "TRUE"){
    
    showNotification( 
      id = "hypopara1_notif",
      "PTH synthesis is decreased in parathyroid gland, mainly caused by a loss of function of parathyroid glands. There may be several explanations
      such as after a thyroid surgery or an immune-system disease. 
      Hover on PTH node in the diagram to have more details. You can click on the info button 
      (on the right of each checkbox in the control center), to have more information about the current simulation.", # put text in notification
      #img(src="bone.png"), # we can put image in notification also
      #action = a(href = "javascript:location.reload();", "Reload page"),
      #action = img(src = "pth_synthesis.png"), # we can put image in action
      duration = 9999, 
      closeButton = TRUE,
      type = "message")
      
    }
    else{
      
      removeNotification(id = "hypopara1_notif", session)
      
    }
    
  })
  
  # hypopara: when next is clicked, show D3 notification
  
  observe({
    
    if(!is.null(counter_quiz$hypopara) && counter_nav$diagram == 2 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypopara2_notif",
        "Less vitamin D3 is converted into 1,25 vitamin D3, resulting in a decrease of intestinal absorption of both Ca and PO4, a decreased reabsorption of Ca.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "hypopara2_notif", session)
      
    }
    
  })
  
  # hypopara: when next is clicked, show FGF23 notification
  
  observe({
    
    if(!is.null(counter_quiz$hypopara) && counter_nav$diagram == 3 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypopara3_notif",
        "FGF23 synthesis is reduced due to lower vitamin D3 levels. Moreover, the elevation of PO4 levels is not enough to compensate the loss of vitamin D3.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "hypopara3_notif", session)
      
    }
    
  })
  
  # hypopara: when next is clicked, show Ca and PO4 notification
  
  observe({
    
    if(!is.null(counter_quiz$hypopara) && counter_nav$diagram == 5 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypopara4_notif",
        "Ionized plasma Ca concentration is widely decreased since intestinal absorption, resorption and reabsorption 
        in kidney are substantially reduced. Inversely, PO4 plasma concentration is significantly enhanced as a consequence of PTH and FGF23 effects on its renal reabsorption, which 
        are lower. Besides, the PO4 rise is cannot compensate the loss of parathyroid function.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "hypopara4_notif", session)
      
    }
    
  })
  
  # Show description of k_prod_PTHg in the graph part
  
  observe({
    
    if(!is.null(counter_quiz$hypopara) && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypopara_xaxis_notif",
        "PTH synthesis is normal when k_prod_PTHg = 1 and decreases progressively following k_prod_PTHg, until it is null.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "warning")
      
    }
    else{
      
      removeNotification(id = "hypopara_xaxis_notif", session)
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Notification events for Vitamin D3 deficiency
  #
  #-------------------------------------------------------------------------
  
  # When the hypoD3 quiz is finished, show a notification explaining what will happen for the current simulation
  
  observe({
    
    if(!is.null(counter_quiz$hypoD3) && input$notif2_switch == "TRUE"){
    
    showNotification( 
      id = "hypoD31_notif",
      "Vitamin D3 deficiency can be due to a deficit in sun exposure (especially in winter) as well as low vitamin D intake.
      Besides, several diseases are known to cause vitamin D3 deficiency such as chronic kidney disease.
      Hover on D3 node in the diagram to have more details. You can click on the info button 
      (on the right of each checkbox in the control center), to have more information about the current simulation.",
      duration = 9999, 
      closeButton = TRUE,
      type = "message")
      
    }
    else{
      
      removeNotification(id = "hypoD31_notif", session)
      
    }
    
  })
  
  observe({
    
    if(!is.null(counter_quiz$hypoD3) && counter_nav$diagram == 1 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypoD32_notif",
        "1,25 vitamin D3 effects on PTH synthesis, FGF23 synthesis and kidney are reduced.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "hypoD32_notif", session)
      
    }
    
  })
  
  # hypoD3: when next is clicked, show D3 notification
  
  observe({
    
    if(!is.null(counter_quiz$hypoD3) && counter_nav$diagram == 2 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypoD33_notif",
        "Plasma PTH concentration increases because of the blunted repression by 1,25 vitamin D3, as well as the reduction of ionized plasma Ca concentration 
         via the calcium sensing receptor. Thus, resorption is increased in order to compensate the reduced intestinal absorption of both Ca and PO4. Similarly, 
         the elevation of PTH levels aims at increasing Ca reabsorption as well as decreasing PO4 reabsorption in kidney. Besides, the increase of PTH also slightly 
         counteract the decrease of vitamin D3 stocks.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "hypoD33_notif", session)
      
    }
    
  })
  
  # hypoD3: when next is clicked, show FGF23 notification
  
  observe({
    
    if(!is.null(counter_quiz$hypoD3) && counter_nav$diagram == 3 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypoD34_notif",
        "FGF23 synthesis is reduced as a result of vitamin D3 deficiency.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "hypoD34_notif", session)
      
    }
    
  })
  
  # hypoD3: when next is clicked, show Ca and PO4 notification
  
  observe({
    
    if(!is.null(counter_quiz$hypoD3) && counter_nav$diagram == 5 && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypoD35_notif",
        "Ionized plasma Ca concentration and PO4 remain quite stable as long as vitamin D3 stocks are not totally depleted. However, they start to decrease as soon as the level of
        vitamin D3 is below a given critical threshold. Consequently, all fluxes are significantly reduced.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "message")
      
    }
    else{
      
      removeNotification(id = "hypoD35_notif", session)
      
    }
    
  })
  
  # Show description of D3_inact in the graph part
  
  observe({
    
    if(!is.null(counter_quiz$hypoD3) && input$notif2_switch == "TRUE"){
      
      showNotification( 
        id = "hypoD3_xaxis_notif",
        "D3_inact is the quantity of 25(OH)D3 (inactive). 0 means that their is no more stock and 1 means that D3 stocks are at base-case value.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "warning")
      
    }
    else{
      
      removeNotification(id = "hypoD3_xaxis_notif", session)
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Notification alerts for Ca iv EGTA iv / PO4 iv and PO4 gavage
  #
  #-------------------------------------------------------------------------
  
  observeEvent(input$Ca_inject | input$PO4_inject | input$PO4_gav,{
    
    if(input$Ca_inject == "TRUE" || input$PO4_inject == "TRUE" || input$PO4_gav == "TRUE"){
      
      showNotification( 
        id = "tmax_notif",
        "By moving this slider, you can control the current time of the simulation (orange vertical line in the graph).", 
        duration = 9999, 
        closeButton = TRUE,
        type = "warning")
      
      showNotification( 
        id = "backnext_notif",
        "Besides, you can also use the back and next
        buttons of the diagram section, which will move the slider.", 
        duration = 9999, 
        closeButton = TRUE,
        type = "warning")
      
    }
    else{
      
      removeNotification(id = "tmax_notif", session)
      removeNotification(id = "backnext_notif", session)
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Set timeline graph events
  #
  #-------------------------------------------------------------------------
  
  # myevents <- reactive({
  #   
  #   input$Ca_inject
  #   
  #   data.frame(
  #     id      = 1,
  #     content = "Item one",
  #     start   = now(),  
  #     end     = now() + 1, # time in seconds (so need to convert...)
  #     editable = TRUE,
  #     type = "range",
  #     title = "Item one"
  #   )
  #   
  # })
  # 
  # timeline <- reactive({
  #   myevents <- myevents()
  #   # need evenReactive otherwise, cannot dispaly visNetwork and timeline together (since they both use vis.js)
  #   if(input$Ca_inject == "TRUE"){
  #     timevis(myevents)
  #   }
  # })
  # 
  # output$timeline <- renderTimevis({
  #   timeline()
  # })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Make the start quiz: triggered when a button of case study is clicked
  #  
  #-------------------------------------------------------------------------
  
  # set each counter and score
  
  score <- reactiveValues(php1 = NULL, hypopara = NULL, hypoD3 = NULL)
  counter_quiz <- reactiveValues(php1 = NULL, hypopara = NULL, hypoD3 = NULL)
  
  # Primary hyperparathyroidism start quiz
  # For the moment, the user can display it only once due to the counter
  
  observeEvent(input$run_php1,{
    
    if(input$run_php1 == "TRUE" && (score$php1 != 3 || is.null(score$php1)) && is.null(counter_quiz$php1)){ # click on php1 button and if the previous score at quiz was lower than 3/3
      
      showModal(modalDialog( # generate the quiz
        title = "PHP1 Start Quiz (only one answer per question)",
        "(1) What is primary hyperparathyroidism?",
        checkboxGroupInput(inputId = "php1q1", label = "", choices = c( "high concentration of calcium" = "php1q11", "high levels of PTH due to low vitamin D3 levels" = "php1q12", 
                                                                        "high levels of PTH due to a tumor in parathyroid glands" = "php1q13",
                                                                        "high PTH levels due to an altered function of the calcium sensing receptor" = "php1q14")),
        br(),
        "(2) What are the symptomes of primary hyperparathyroidism?",
        checkboxGroupInput(inputId = "php1q2", label = "", choices = c("high concentration of calcium, low vitamin D3 levels, bone loss, hypercalciuria" = "php1q21", 
                                                                       "low concentration of calcium, high vitamin D3 levels, bone gain, hypocalciuria" = "php1q22", 
                                                                       "high Ca concentration, low PO4 levels, high vitamin D3 levels, bone loss and hypocalciuria" = "php1q23",
                                                                       "high Ca concentration, low PO4 levels, high vitamin D3 levels, bone loss and hypercalciuria" = "php1q24")),
        
        br(),
        "(3) What are the treatments of primary hyperparathyroidism?",
        checkboxGroupInput(inputId = "php1q3", label = "", choices = c("calcimimetic treatments (cinacalcet)" = "php1q31", "vitamin D3 supplementation" = "php1q32",
                                                                       "EGTA infusion (hypocalcemia)" = "php1q33", "high fat diet and PO4 supplementation" = "php1q34", 
                                                                       "parathyroid surgery" = "php1q35")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("goQuizphp1", "Submit quiz!")
        )
        
      ))
      
    }
    else if (input$run_php1 == "FALSE"){ # if run_php1 is unckecked, reset the quiz so that graph 
      
      counter_quiz$php1 <- NULL
      
    }
    
  })
  
  # treat the PTH quiz: display the score based on a score reactiveValues counter
  
  output$counter_quiz_php1 <- reactive({ !is.null(counter_quiz$php1) }) # these two lines are needed to hide the diagram while the quiz is not completed
  outputOptions(output, 'counter_quiz_php1', suspendWhenHidden=FALSE) # these two lines are needed to hide the diagram while the quiz is not completed
  
  observeEvent(input$goQuizphp1,{
    
    if(is.null(input$php1q1) || is.null(input$php1q2) || is.null(input$php1q3)){ # 
      #score$php1 <- NULL
      sendSweetAlert(messageId = "successQuizphp1", title = "Oops ...", 
                     text = "Please answer all questions!", type = "error")
    }
    else{ # all fields are filled
      
      counter_quiz$php1 <- 1 # set the counter equal to 1 so that quiz cannot be displayed again
      write.csv(x = c(input$php1q1,input$php1q2,input$php1q3), file = "answer_php1.csv") # write answers in a local file
      f <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/answer_php1.csv", 
                    stringsAsFactors = F) # do not forget to change this path if needed
      #f <- read.csv("/srv/shiny-server/capApp/case_studies_app/www/answer_php1.csv", 
                    #stringsAsFactors = F) # do not forget to change this path if needed
      f$X <- NULL
      
      if(f$x[1] == "php1q13" && f$x[2] == "php1q24" && f$x[3] == "php1q31"){ # read the answer files 3/3
        score$php1 <- 3
        sendSweetAlert(messageId = "successQuizphp1", title = "Congratulations ...", 
                       text = paste("You have", score$php1,"/3.", " You are already a specialist of primary-hyperparathyroidism!"), type = "success")
        
      }
      else if((f$x[1] == "php1q13" && f$x[2] == "php1q24") || 
              (f$x[1] == "php1q13" && f$x[3] == "php1q31") || 
              (f$x[2] == "php1q24" && f$x[3] == "php1q31")){ # 2/3
        score$php1 <- 2
        sendSweetAlert(messageId = "successQuizphp1", title = "Congratulations ...", 
                       text = paste("You have", score$php1,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "success")
      }
      else if(f$x[1] == "php1q13" || f$x[2] == "php1q24" || f$x[3] == "php1q31"){ # 1/3
        
        score$php1 <- 1
        sendSweetAlert(messageId = "successQuizphp1", title = "Fail ...", 
                       text = paste("You have", score$php1,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "warning")
        
      }else{ # 0/3
        
        score$php1 <- 0
        sendSweetAlert(messageId = "successQuizphp1", title = "Fail ...", 
                       text = paste("You have", score$php1,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "error")
        
      }
      
    }
    
  })
  
  # hypoparathyroidism start quiz
  # For the moment, the user can display it only once due to the counter
  
  observeEvent(input$run_hypopara,{
    
    if(input$run_hypopara == "TRUE" && (score$hypopara != 3 || is.null(score$hypopara)) && is.null(counter_quiz$hypopara)){ # click on hypopara button 
      
      showModal(modalDialog( # generate the quiz
        title = "Hypoparathyroidism Start Quiz (Shoback, N Engl J Med, 359: 2008.) (only one answer per question)",
        "(1) What is hypoparathyroidism?",
        checkboxGroupInput(inputId = "hypoparaq1", label = "", choices = c( "A surgery to remove parathyroid glands" = "hypoparaq11", 
                                                                            "High vitamin D3 levels causing low PTH concentration" = "hypoparaq12", 
                                                                            "A gain of function of the calcium sensing receptor in parathyroid chief cells" = "hypoparaq13",
                                                                            "Decreased function of parathyroid glands" = "hypoparaq14")),
        br(),
        "(2) What are the consequences on Ca/PO4 homeostasis?",
        checkboxGroupInput(inputId = "hypoparaq2", label = "", choices = c("low concentration of Ca, high vitamin D3 levels, hypophosphatemia" = "hypoparaq21", 
                                                                       "low concentration of Ca, low vitamin D3 levels, normal to high PO4 levels" = "hypoparaq22", 
                                                                       "high Ca concentration, low PO4 levels, and bone loss" = "hypoparaq23",
                                                                       "Hypercalciuria, high FGF23 levels" = "hypoparaq24")),
        
        br(),
        "(3) What are the available treatments?",
        checkboxGroupInput(inputId = "hypoparaq3", label = "", choices = c("vitamin D3 injections" = "hypoparaq31", "Ca supplementation/ PTH injections (minipump)" = "hypoparaq32",
                                                                       "bisphosphonate treatments" = "hypoparaq33")),
        br(),
        
        h5("Notes"),
        "Hypoparathyroidism has not to be be confused with pseudohypoparathyroidism, which is caused by a loss of sensitivity to PTH, despite normal PTH levels.",
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("goQuizhypopara", "Submit quiz!")
        )
        
      ))
      
    }
    else if (input$run_hypopara == "FALSE"){ # if run_hypopara is unckecked, reset the quiz so that graph
      
      counter_quiz$hypopara <- NULL
      
    }
    
  })
  
  # treat the Hypopara quiz: display the score based on a score reactiveValues counter
  
  output$counter_quiz_hypopara <- reactive({ !is.null(counter_quiz$hypopara) }) # these two lines are needed to hide the diagram while the quiz is not completed
  outputOptions(output, 'counter_quiz_hypopara', suspendWhenHidden=FALSE) # these two lines are needed to hide the diagram while the quiz is not completed
  
  observeEvent(input$goQuizhypopara,{
    
    if(is.null(input$hypoparaq1) || is.null(input$hypoparaq2) || is.null(input$hypoparaq3)){ # 
      sendSweetAlert(messageId = "successQuizhypopara", title = "Oops ...", 
                     text = "Please answer all questions!", type = "error")
    }
    else{ # all fields are filled
      
      counter_quiz$hypopara <- 1 # set the counter equal to 1 so that quiz cannot be displayed again
      write.csv(x = c(input$hypoparaq1,input$hypoparaq2,input$hypoparaq3), file = "answer_hypopara.csv") # write answers in a local file
      f <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/answer_hypopara.csv", 
                    stringsAsFactors = F) # do not forget to change this path if needed
      #f <- read.csv("/srv/shiny-server/capApp/case_studies_app/www/answer_hypopara.csv", 
                    #stringsAsFactors = F) # do not forget to change this path if needed
      f$X <- NULL
      
      if(f$x[1] == "hypoparaq14" && f$x[2] == "hypoparaq22" && f$x[3] == "hypoparaq32"){ # read the answer files 3/3: this is the perfect answer
        score$hypopara <- 3
        sendSweetAlert(messageId = "successQuizhypopara", title = "Congratulations ...", 
                       text = paste("You have", score$hypopara,"/3.", " You are already an expert of hypoparathyroidism!"), type = "success")
        
      }
      else if((f$x[1] == "hypoparaq14" && f$x[2] == "hypoparaq22") || 
              (f$x[1] == "hypoparaq14" && f$x[3] == "hypoparaq32") || 
              (f$x[2] == "hypoparaq22" && f$x[3] == "hypoparaq32")){ # 2/3
        score$hypopara <- 2
        sendSweetAlert(messageId = "successQuizhypopara", title = "Congratulations ...", 
                       text = paste("You have", score$hypopara,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "success")
      }
      else if(f$x[1] == "hypoparaq14" || f$x[2] == "hypoparaq22" || f$x[3] == "hypoparaq32"){ # 1/3
        
        score$hypopara <- 1
        sendSweetAlert(messageId = "successQuizhypopara", title = "Fail ...", 
                       text = paste("You have", score$hypopara,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "warning")
        
      }else{ # 0/3
        
        score$hypopara <- 0
        sendSweetAlert(messageId = "successQuizhypopara", title = "Fail ...", 
                       text = paste("You have", score$hypopara,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "error")
        
      }
      
    }
    
  })
  
  # Vitamin D3 deficiency start quiz
  # For the moment, the user can display it only once due to the counter
  
  observeEvent(input$run_hypoD3,{
    
    if(input$run_hypoD3 == "TRUE" && (score$hypoD3 != 3 || is.null(score$hypoD3)) && is.null(counter_quiz$hypoD3)){ # click on hypoD3 button 
      
      showModal(modalDialog( # generate the quiz
        title = "Vitamin D3 deficiency Start Quiz (maybe several answers per question)",
        "(1) What are the cause of vitamin D3 deficiency?",
        checkboxGroupInput(inputId = "hypoD3q1", label = "", choices = c( "Insufficiant intake of vitamin D and sun exposure" = "hypoD3q11", 
                                                                            "decreased conversion of 25(OH)D in kidneys due to a chronic kidney disease" = "hypoD3q12")),
        br(),
        "(2) What are the consequences on Ca/PO4 homeostasis?",
        checkboxGroupInput(inputId = "hypoD3q2", label = "", choices = c("high FGF23 concentrations; hypercaliuria, bone loss" = "hypoD3q21", 
                                                                           "hyperphosphatemia, hypocalcemia" = "hypoD3q22", 
                                                                           "hypocalcemia, secondary hyperparathyroidism, hypophosphatemia" = "hypoD3q23")),
        
        br(),
        "(3) What are the available treatments?",
        checkboxGroupInput(inputId = "hypoD3q3", label = "", choices = c("vitamin D3 daily or weekly injections" = "hypoD3q31", 
                                                                         "Ca supplementation/ PTH injections (minipump)" = "hypoD3q32",
                                                                           "parathyroidectomy" = "hypoD3q33")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("goQuizhypoD3", "Submit quiz!")
        )
        
      ))
      
    }
    else if (input$run_hypoD3 == "FALSE"){ # if run_hypopara is unckecked, reset the quiz so that graph
      
      counter_quiz$hypoD3 <- NULL
      
    }
    
  })
  
  # treat the HypoD3 quiz: display the score based on a score reactiveValues counter
  
  output$counter_quiz_hypoD3 <- reactive({ !is.null(counter_quiz$hypoD3) }) # these two lines are needed to hide the diagram while the quiz is not completed
  outputOptions(output, 'counter_quiz_hypoD3', suspendWhenHidden=FALSE) # these two lines are needed to hide the diagram while the quiz is not completed
  
  observeEvent(input$goQuizhypoD3,{
    
    if(is.null(input$hypoD3q1) || is.null(input$hypoD3q2) || is.null(input$hypoD3q3)){ # 
      sendSweetAlert(messageId = "successQuizhypoD3", title = "Oops ...", 
                     text = "Please answer all questions!", type = "error")
    }
    else{ # all fields are filled
      
      counter_quiz$hypoD3 <- 1 # set the counter equal to 1 so that quiz cannot be displayed again
      write.csv(x = c(input$hypoD3q1,input$hypoD3q2,input$hypoD3q3), file = "answer_hypoD3.csv") # write answers in a local file
      f <- read.csv("/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/answer_hypoD3.csv", 
                    stringsAsFactors = F) # do not forget to change this path if needed
      #f <- read.csv("/srv/shiny-server/capApp/case_studies_app/www/answer_hypoD3.csv", 
                    #stringsAsFactors = F) # do not forget to change this path if needed
      f$X <- NULL
      
      if((f$x[[2]] != "hypoparaq11") || ( f$x[[2]] != "hypoparaq12")){
        # if the second answer is not clicked, create a 0 value to avoid errors in the following code
        f$ind <- seq_len(nrow(f))
        f <- rbind(f,data.frame(x = "0",ind=1.1))
        f <- f[order(f$ind),]
        
      } 
      
      if(f$x[1] == "hypoD3q11" && f$x[2] == "hypoparaq12" && f$x[3] == "hypoparaq23" && f$x[4] == "hypoparaq31"){ # read the answer files 3/3: this is the perfect answer
        score$hypoD3 <- 3
        sendSweetAlert(messageId = "successQuizhypoD3", title = "Congratulations ...", 
                       text = paste("You have", score$hypoD3,"/3.", " You are already an expert of vitamin D3 deficiency!"), type = "success")
        
      }
      else if((f$x[1] == "hypoD3q11" && f$x[2] == "hypoD3q12" && f$x[3] == "hypoD3q23") || 
              (f$x[1] == "hypoD3q11" && f$x[2] == "hypoD3q12" && f$x[4] == "hypoD3q31") || 
              (f$x[3] == "hypoD3q23" && f$x[4] == "hypoD3q31")){ # 2/3
        score$hypoD3 <- 2
        sendSweetAlert(messageId = "successQuizhypoD3", title = "Congratulations ...", 
                       text = paste("You have", score$hypoD3,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "success")
      }
      else if((f$x[1] == "hypoD3q11" &&  f$x[2] == "hypoD3q12")|| f$x[3] == "hypoD3q23" || f$x[4] == "hypoD3q31"){ # 1/3
        
        score$hypoD3 <- 1
        sendSweetAlert(messageId = "successQuizhypoD3", title = "Fail ...", 
                       text = paste("You have", score$hypoD3,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "warning")
        
      }else{ # 0/3
        
        score$hypoD3 <- 0
        sendSweetAlert(messageId = "successQuizhypoD3", title = "Fail ...", 
                       text = paste("You have", score$hypoD3,"/3.", 
                                    " You can close the quiz and spend some time to use this application."), type = "error")
        
      }
      
    }
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Make interactive plot by loading tables of diseases
  #  be careful when put on webserver to change the path of table to avoid
  #  reading errors
  #-------------------------------------------------------------------------
  
  # php1
  
  path_to_php1 <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/www/php1.csv" 
  #path_to_php1 <- "/srv/shiny-server/capApp/case_studies_app/www/php1.csv" 
  php1_table <- read.csv(path_to_php1)
  
  php1_vec <- 4.192*seq(1,300,by = 10) # create the sequence of PTH production rate
  names(php1_vec) <- paste("k_prod_PTHg =", php1_vec)
  
  
  output$php1_plot <- renderPlotly({
    
    if(!is.null(score$php1) && !is.null(counter_quiz$php1)){ # only show the php1 plot if the quiz was filled
      
      input$run_php1
      
      xvar <- list(title = "k_prod_PTHg fold increase", range = c(min(php1_vec/php1_vec[1]), max(php1_vec)/php1_vec[1]))
      yvar1 <- list(title = "Normalized concentrations", range = c(0, 2))
      yvar2 <- list(title = "Normalized concentrations", range = c(0,7))
      yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,5))
      yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,3))
      
      plot_CaP_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], y = php1_table[,"Ca_p"]/php1_table[1,"Ca_p"],
                               type = "scatter", mode = "lines", line = list(color = 'rgb(27, 27, 244)', width = 2), showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"PO4_p"]/php1_table[1,"PO4_p"], line = list(color = 'rgb(244, 27, 27)', width = 2), showlegend = F) %>%
        add_annotations(x= 400, y= 2.6, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", showarrow = T) %>%
        add_annotations(x= 400, y= 0.3, xref = "x", yref = "y",text = "<b>[PO4]p</b>", showarrow = T) %>%
        layout(xaxis = NULL, yaxis = yvar1)
      
      plot_hormones_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], y = php1_table[,"PTH_p"]/php1_table[1,"PTH_p"],
                                    type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"), showlegend = F)%>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"D3_p"]/php1_table[1,"D3_p"], line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"FGF_p"]/php1_table[1,"FGF_p"], line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        add_annotations(x= 200, y= 5.2, xref = "x2", yref = "y2", text = "<b>[PTH]p</b>", showarrow = T, ax = 20, ay = 25) %>%
        add_annotations(x= 400, y= 10, xref = "x2", yref = "y2", text = "<b>[D3]p</b>", showarrow = T) %>%
        add_annotations(x= 600, y= 2, xref = "x2", yref = "y2", text = "<b>[FGF23]p</b>", showarrow = T) %>%
        layout(xaxis = NULL, yaxis = yvar2)
      
      plot_Ca_fluxes_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], y = php1_table[,"U_Ca"]/php1_table[1,"U_Ca"],
                                     type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"), showlegend = F)%>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Abs_int_Ca"]/php1_table[1,"Abs_int_Ca"], line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Res_Ca"]/php1_table[1,"Res_Ca"], line = list(color = 'black', width = 2, dash = "dashdot"), showlegend = F) %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Ac_Ca"]/php1_table[1,"Ac_Ca"], line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        layout(xaxis = xvar, yaxis = yvar3)
      
      plot_PO4_fluxes_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], y = php1_table[,"U_PO4"]/php1_table[1,"U_PO4"],
                                      type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"), name = "Urinary Excretion")%>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Abs_int_PO4"]/php1_table[1,"Abs_int_PO4"], line = list(color = 'black', width = 2, dash = "dot"), 
                  name = "Intestinal absorption") %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Res_PO4"]/php1_table[1,"Res_PO4"], line = list(color = 'black', width = 2, dash = "dashdot"), 
                  name = "Bone resorption") %>%
        add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Ac_PO4"]/php1_table[1,"Ac_PO4"], line = list(color = 'black', width = 2, dash = "solid"), 
                  name = "FLux into bone") %>%
        #add_annotations(x= 600, y= 4, xref = "x4", yref = "y4", text = "<b>Urinary excretion</b>", showarrow = T) %>%
        #add_annotations(x= 200, y= 3, xref = "x4", yref = "y4", text = "<b>Bone resorption</b>", showarrow = T) %>%
        #add_annotations(x= 400, y= 3, xref = "x4", yref = "y4", text = "<b>Intestinal absorption</b>", showarrow = T) %>%
        #add_annotations(x= 200, y= 2, xref = "x4", yref = "y4", text = "<b>Flux into bone</b>", showarrow = T) %>%
        layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'v', x = 0.05, y = 0.3))
      
      
      plot_php1 <- subplot(plot_CaP_php1, plot_hormones_php1, plot_Ca_fluxes_php1, plot_PO4_fluxes_php1, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.07, 
                           heights = c(0.5,0.5))
      
    }
    
  })
  
  # Vitamin D3 deficiency
  
  path_to_hypoD3 <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/www/hypoD3.csv"
  #path_to_hypoD3 <- "/srv/shiny-server/capApp/case_studies_app/www/hypoD3.csv"
  hypoD3_table <- read.csv(path_to_hypoD3)
  
  hypoD3_vec <- rev(2.5e-005*seq(0, 1, by = 0.01)) # create the sequence of D3 inact and reverse the vector
  names(hypoD3_vec) <- paste("D3_inact =", hypoD3_vec)
  
  output$hypoD3_plot <- renderPlotly({
    
    if(!is.null(score$hypoD3) && !is.null(counter_quiz$hypoD3)){ # only show the php1 plot if the quiz was filled
      
      input$run_hypoD3
      
      xvar <- list(title = "D3_inact fold decrease", range = c(max(hypoD3_vec/hypoD3_vec[1]), min(hypoD3_vec)/hypoD3_vec[1]),
                   autorange = F, autorange="reversed")
      xvar_bis <- list(title = "", range = c(max(hypopara_vec/hypopara_vec[1]), min(hypopara_vec)/hypopara_vec[1]),
                       autorange = F, autorange="reversed")
      yvar1 <- list(title = "Normalized concentrations", range = c(0, 1.1))
      yvar2 <- list(title = "Normalized concentrations", range = c(0,4))
      yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,1.2))
      yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,1.1))
      
      plot_CaP_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Ca_p"]/hypoD3_table[1,"Ca_p"],
                                 type = "scatter", mode = "lines", line = list(color = 'rgb(27, 27, 244)', width = 2), showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"PO4_p"]/hypoD3_table[1,"PO4_p"], line = list(color = 'rgb(244, 27, 27)', width = 2), showlegend = F) %>%
        add_annotations(x= 0.7, y= 0.95, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", showarrow = T, ax = -20, ay = 40) %>%
        add_annotations(x= 0.4, y= 1.1, xref = "x", yref = "y",text = "<b>[PO4]p</b>", showarrow = T, ax = -20, ay = -20) %>%
        layout(xaxis = xvar_bis, yaxis = yvar1)
      
      plot_hormones_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"PTH_p"]/hypoD3_table[1,"PTH_p"],
                                      type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"), showlegend = F)%>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"D3_p"]/hypoD3_table[1,"D3_p"], line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"FGF_p"]/hypoD3_table[1,"FGF_p"], line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        add_annotations(x= 0, y= 2.3, xref = "x2", yref = "y2", text = "<b>[PTH]p</b>", showarrow = T) %>%
        add_annotations(x= 0.9, y= 0.9, xref = "paper", yref = "y2", text = "<b>[D3]p</b>", showarrow = T) %>%
        add_annotations(x= 0.9, y= 0.4, xref = "paper", yref = "y2", text = "<b>[FGF23]p</b>", showarrow = T, ax = -20, ay = 30) %>%
        layout(xaxis = xvar_bis, yaxis = yvar2)
      
      plot_Ca_fluxes_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"U_Ca"]/hypoD3_table[1,"U_Ca"],
                                       type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"), showlegend = F)%>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Abs_int_Ca"]/hypoD3_table[1,"Abs_int_Ca"], line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Res_Ca"]/hypoD3_table[1,"Res_Ca"], line = list(color = 'black', width = 2, dash = "dashdot"), showlegend = F) %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Ac_Ca"]/hypoD3_table[1,"Ac_Ca"], line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        layout(xaxis = xvar, yaxis = yvar3)
      
      plot_PO4_fluxes_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"U_PO4"]/hypoD3_table[1,"U_PO4"],
                                        type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"),
                                        name = "Urinary Excretion")%>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Abs_int_PO4"]/hypoD3_table[1,"Abs_int_PO4"], line = list(color = 'black', width = 2, dash = "dot"), 
                  name = "Intestinal absorption") %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Res_PO4"]/hypoD3_table[1,"Res_PO4"], line = list(color = 'black', width = 2, dash = "dashdot"), 
                  name = "Bone resorption") %>%
        add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Ac_PO4"]/hypoD3_table[1,"Ac_PO4"], line = list(color = 'black', width = 2, dash = "solid"), 
                  name = "Flux into bone") %>%
        layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'v', x = 0.01, y = 0.05))
      
      
      plot_hypoD3 <- subplot(plot_CaP_hypoD3, plot_hormones_hypoD3, plot_Ca_fluxes_hypoD3, plot_PO4_fluxes_hypoD3, titleX = TRUE, titleY = TRUE,
                             nrows = 2, margin = 0.07, heights = c(0.5,0.5))
      
    }
    
  })
  
  # Hypoparathyroidism
  
  path_to_hypopara <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/www/hypopara.csv"
  #path_to_hypopara <- "/srv/shiny-server/capApp/case_studies_app/www/hypopara.csv"
  hypopara_table <- read.csv(path_to_hypopara)
  
  hypopara_vec <- rev(4.192*seq(0, 1, by = 0.01)) # create the sequence of PTH production rate
  names(hypopara_vec) <- paste("k_prod_PTHg =",  hypopara_vec)
  
  output$hypopara_plot <- renderPlotly({
    
    if(!is.null(score$hypopara) && !is.null(counter_quiz$hypopara)){ # only show the php1 plot if the quiz was filled
      
      input$run_hypopara
      
      xvar <- list(title = "k_prod_PTHg fold decrease", range = c(max(hypopara_vec/hypopara_vec[1]), min(hypopara_vec)/hypopara_vec[1]),
                   autorange = F, autorange="reversed")
      xvar_bis <- list(title = "", range = c(max(hypopara_vec/hypopara_vec[1]), min(hypopara_vec)/hypopara_vec[1]),
                       autorange = F, autorange="reversed")
      yvar1 <- list(title = "Normalized concentrations", range = c(0, 1.4))
      yvar2 <- list(title = "Normalized concentrations", range = c(0,1))
      yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,1))
      yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,1.4))
      
      plot_CaP_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"Ca_p"]/hypopara_table[1,"Ca_p"],
                                   type = "scatter", mode = "lines", line = list(color = 'rgb(27, 27, 244)', width = 2), showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"PO4_p"]/hypopara_table[1,"PO4_p"], 
                  line = list(color = 'rgb(244, 27, 27)', width = 2), showlegend = F) %>%
        add_annotations(x= 0.5, y= 0.85, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", showarrow = T, ax = -20, ay = 40) %>%
        add_annotations(x= 0.1, y= 1.7, xref = "x", yref = "y",text = "<b>[PO4]p</b>", showarrow = T) %>%
        layout(xaxis = xvar_bis, yaxis = yvar1)
      
      plot_hormones_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"PTH_p"]/hypopara_table[1,"PTH_p"],
                                        type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"), showlegend = F)%>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"D3_p"]/hypopara_table[1,"D3_p"], 
                  line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"FGF_p"]/hypopara_table[1,"FGF_p"], 
                  line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        add_annotations(x= 0.9, y= 0.2, xref = "paper", yref = "y2", text = "<b>[PTH]p</b>", showarrow = T, ax = -20, ay = 40) %>%
        add_annotations(x= 0.9, y= 0.67, xref = "paper", yref = "y2", text = "<b>[D3]p</b>", showarrow = T, ax = 15, ay = -30) %>%
        add_annotations(x= 0.88, y= 0.5, xref = "paper", yref = "y2", text = "<b>[FGF23]p</b>", showarrow = T, ax = -50, ay = 10) %>%
        layout(xaxis = xvar_bis, yaxis = yvar2)
      
      plot_Ca_fluxes_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"U_Ca"]/hypopara_table[1,"U_Ca"],
                                         type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"), showlegend = F)%>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"Abs_int_Ca"]/hypopara_table[1,"Abs_int_Ca"], 
                  line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"Res_Ca"]/hypopara_table[1,"Res_Ca"], 
                  line = list(color = 'black', width = 2, dash = "dashdot"), showlegend = F) %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"Ac_Ca"]/hypopara_table[1,"Ac_Ca"], 
                  line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
        layout(xaxis = xvar, yaxis = yvar3)
      
      plot_PO4_fluxes_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"U_PO4"]/hypopara_table[1,"U_PO4"],
                                          type = "scatter", mode = "lines", line = list(color = 'black', width = 2, dash = "dash"),
                                          name = "Urinary Excretion")%>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"Abs_int_PO4"]/hypopara_table[1,"Abs_int_PO4"], 
                  line = list(color = 'black', width = 2, dash = "dot"),
                  name = "Intestinal absorption") %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"Res_PO4"]/hypopara_table[1,"Res_PO4"], 
                  line = list(color = 'black', width = 2, dash = "dashdot"),
                  name = "Bone resorption") %>%
        add_lines(x = hypopara_vec/hypopara_vec[1], y = hypopara_table[,"Ac_PO4"]/hypopara_table[1,"Ac_PO4"], 
                  line = list(color = 'black', width = 2, dash = "solid"),
                  name = "Flux into bone") %>%
        layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'v', x = 0.01, y = 0.05))
      
      
      plot_hypopara <- subplot(plot_CaP_hypopara, plot_hormones_hypopara, plot_Ca_fluxes_hypopara, plot_PO4_fluxes_hypopara, titleX = TRUE, titleY = TRUE,
                               nrows = 2, margin = 0.07, heights = c(0.5,0.5))
      
    }
    
  })
  
  # Ca iv injection
  
  path_to_Ca_iv <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/www/iv_Ca.csv"
  #path_to_Ca_iv <- "/srv/shiny-server/capApp/case_studies_app/www/iv_Ca.csv"
  Ca_iv_table <- read.csv(path_to_Ca_iv)
  
  output$Ca_iv_plot <- renderPlotly({
    
    input$Ca_inject
    
    injectevents <- data.frame(times = c(0, 60, 65, 70, 80, 90, 100, 110, 120), Ca_val = 1/1.35*c(1.35, 1.45, 1.30, 1.20, 1.15, 1.10, 1.10, 1.00, 1.05),
                               PTH_val = 1/65*c(65, 10, 190, 260, 300, 260, 240, 290, 310), err_Ca = 1/1.35*2*c(0.02,0.04,0.04,0.06,0.04,0.06,0.06,0.07, 0.04),
                               err_PTH = 1/65*c(20, 0, 70, 100, 70, 70, 50, 70, 110))
    
    xvar <- list(title = "time (min)", range = c(0, max(Ca_iv_table[,1])+10))
    yvar1 <- list(title = "Normalized [Ca2+]p", range = c(0,2))
    yvar2 <- list(title = "Normalized [PTH]p", range = c(0,10))
    
    p1 <- plot_ly(Ca_iv_table, x = Ca_iv_table[,1], y = Ca_iv_table[,"Ca_p"]/Ca_iv_table[1,"Ca_p"], type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_markers(x=injectevents$times, y=injectevents$Ca_val, mode = 'markers', symbols = "o", marker = list(size = 10, color = 'black'),
                  error_y = list(array = injectevents$err_Ca, color = 'black'), line = list(color = 'white')) %>%
      add_lines(x = Ca_iv_table[,1], y = Ca_iv_table[,"Ca_p"]/Ca_iv_table[1,"Ca_p"], type = "scatter", mode = "lines", 
                line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_lines(x = input$tmax, y = c(0,2), line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      #add_annotations(x = input$tmax, y = 0, text = "Current Time") %>%
      layout(xaxis = xvar, yaxis = yvar1)
    
    p2 <- plot_ly(data = Ca_iv_table, x = Ca_iv_table[,1], y = Ca_iv_table[,"PTH_p"]/Ca_iv_table[1,"PTH_p"], type = "scatter", mode = "lines",
                  line = list(color = 'black', width = 2)) %>%
      add_trace(x=injectevents$times, y=injectevents$PTH_val, mode = 'markers', symbols = "o", marker = list(size = 10, color = 'black'),
                error_y = list(array = injectevents$err_PTH, color = 'black'), line = list(color = 'white')) %>%
      add_lines(x = Ca_iv_table[,1], y = Ca_iv_table[,"PTH_p"]/Ca_iv_table[1,"PTH_p"], type = "scatter", mode = "lines",
                line = list(color = 'black', width = 2)) %>%
      add_lines(x = input$tmax, y = c(0,10), line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      #add_annotations(x = input$tmax, y = 0, text = "Current Time") %>%
      layout(xaxis = xvar, yaxis = yvar2)
    
    p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE, nrows = 1, margin = 0.05)
    
    hide_legend(p)
    
  })
  
  # PO4 iv injection
  
  path_to_PO4_iv <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/www/iv_PO4.csv"
  #path_to_PO4_iv <- "/srv/shiny-server/capApp/case_studies_app/www/iv_PO4.csv"
  PO4_iv_table <- read.csv(path_to_PO4_iv)
  
  output$PO4_iv_plot  <- renderPlotly({
    
    input$PO4_inject
    
    injectevents <- data.frame(times = c(0,10,25,40,55,70,130,190,250), PO4_val = c(2.86, 5.84, 5.59, 4.69, 4.24, 3.83, 3.08, 3.27, 3.26),
                               Ca_val = 1/2.4*c(2.4,1.92,1.97,1.93,1.86,1.76,1.89,2.03,2.02),
                               PTH_val = c(1,8.72, 7.80, 7, 7.64, 9.34, 9.74, 7.12, 6), err_PO4 = c(0.3, 0.33, 0.33, 0.26, 0.24, 0.13, 0.15, 0.21, 0.39),
                               err_Ca = c(0.05, 0.04, 0.07, 0.04, 0.03, 0.04, 0.09, 0.04, 0.09),
                               err_PTH = c(0.53, 1.40, 1.34, 0.88, 0.97, 0.93, 1.47, 1.42, 0.87))
    
    xvar <- list(title = "time (min)", range = c(0, max(PO4_iv_table[,1])))
    yvar1 <- list(title = "[PO4]p (mM)", range = c(0,8))
    yvar2 <- list(title = "Normalized [Ca2+]p", range = c(0,2))
    yvar3 <- list(title = "Normalized [PTH]p", range = c(0,20))
    
    p1 <- plot_ly(PO4_iv_table, x = PO4_iv_table[,1], y = PO4_iv_table[,"PO4_tot"], type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
      add_markers(x=injectevents$times, y=injectevents$PO4_val, mode = 'markers', symbols = "o", marker = list(size = 10, color = 'black'),
                  error_y = list(array = injectevents$err_PO4, color = 'black'), line = list(color = 'white')) %>%
      add_lines(x = PO4_iv_table[,1], y = PO4_iv_table[,"PO4_tot"], type = "scatter", mode = "lines", line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
      add_lines(x = input$tmaxbis, y = c(0,8), line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = NULL, yaxis = yvar1)
    
    p2 <- plot_ly(PO4_iv_table, x = PO4_iv_table[,1], y = PO4_iv_table[,"Ca_tot"]/PO4_iv_table[1,"Ca_tot"], type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_markers(x=injectevents$times, y=injectevents$Ca_val, mode = 'markers', symbols = "o", marker = list(size = 10, color = 'black'),
                  error_y = list(array = injectevents$err_Ca, color = 'black'), line = list(color = 'white')) %>%
      add_lines(x = PO4_iv_table[,1], y = PO4_iv_table[,"Ca_tot"]/PO4_iv_table[1,"Ca_tot"], type = "scatter", mode = "lines", 
                line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_lines(x = input$tmaxbis, y = c(0,2), line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      
      layout(xaxis = xvar, yaxis = yvar2)
    
    p3 <- plot_ly(data = PO4_iv_table, x = PO4_iv_table[,1], y = PO4_iv_table[,"PTH_p"]/PO4_iv_table[1,"PTH_p"], type = "scatter", mode = "lines",
                  line = list(color = 'black', width = 2)) %>%
      add_trace(x=injectevents$times, y=injectevents$PTH_val, mode = 'markers', symbols = "o", marker = list(size = 10, color = 'black'),
                error_y = list(array = injectevents$err_PTH, color = '#000000'), line = list(color = 'white')) %>%
      add_lines(x = PO4_iv_table[,1], y = PO4_iv_table[,"PTH_p"]/PO4_iv_table[1,"PTH_p"], type = "scatter", mode = "lines", 
                line = list(color = 'black', width = 2)) %>%
      add_lines(x = input$tmaxbis, y = c(0,20), line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = xvar, yaxis = yvar3)
    
    p <- subplot(p1, p2, p3, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.05)
    
    hide_legend(p)
    
  })
  
  # PO4 gavage
  
  path_to_PO4_gav <- "/Users/macdavidgranjon/Dropbox/Post_Doc_Zurich_2017/WebApp_CaP_homeostasis/case_studies_app/www/gav_PO4.csv"
  #path_to_PO4_gav <- "/srv/shiny-server/capApp/case_studies_app/www/gav_PO4.csv"
  PO4_gav_table <- read.csv(path_to_PO4_gav)
  
  output$PO4_gav_plot  <- renderPlotly({
    
    gavevents <- data.frame(times = c(0,10,25,40,55,70,130,190,250), PO4_val = c(2.87, 2.91, 3.30, 3.20, 3.10, 3.23, 2.97, 2.72, 2.89),
                            Ca_val = 1/2.08*c(2.08,1.80,1.99,1.96,1.91,1.83,1.75,1.74,1.79),
                            PTH_val = c(1, 1.60, 2.17, 2.14, 2.12, 1.77, 1.95, 1.69, 1.80), err_PO4 = c(0.14, 0.1, 0.07, 0.2, 0.1, 0.09, 0.13, 0.1, 0.19),
                            err_Ca = c(0.03, 0.07, 0.03, 0.07, 0.04, 0.06, 0.03, 0.03, 0.04),
                            err_PTH = c(0.16, 0.34, 0.40, 0.20, 0.29, 0.12, 0.19, 0.19, 0.14))
    
    xvar <- list(title = "time (min)", range = c(0, max(PO4_gav_table[,1])))
    yvar1 <- list(title = "[PO4]p (mM)", range = c(0,8))
    yvar2 <- list(title = "Normalized [Ca2+]p", range = c(0,2))
    yvar3 <- list(title = "Normalized [PTH]p", range = c(0,20))
    
    p1 <- plot_ly(PO4_gav_table, x = PO4_gav_table[,1], y = PO4_gav_table[,"PO4_tot"], type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
      add_markers(x=gavevents$times, y=gavevents$PO4_val, mode = 'markers', symbols = "o", marker = list(size = 10, color = 'black'),
                  error_y = list(array = gavevents$err_PO4, color = 'black'), line = list(color = 'white')) %>%
      add_lines(x = PO4_gav_table[,1], y = PO4_gav_table[,"PO4_tot"], type = "scatter", mode = "lines", 
                line = list(color = 'rgb(244, 27, 27)', width = 2)) %>%
      add_lines(x = input$tmaxtris, y = c(0,8), line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = NULL, yaxis = yvar1)
    
    p2 <- plot_ly(PO4_gav_table, x = PO4_gav_table[,1], y = PO4_gav_table[,"Ca_tot"]/PO4_gav_table[1,"Ca_tot"], type = "scatter", mode = "lines", 
                  line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_markers(x=gavevents$times, y=gavevents$Ca_val, mode = 'markers', symbols = "o", marker = list(size = 10, color = 'black'),
                  error_y = list(array = gavevents$err_Ca, color = 'black'), line = list(color = 'white')) %>%
      add_lines(x = PO4_gav_table[,1], y = PO4_gav_table[,"Ca_tot"]/PO4_gav_table[1,"Ca_tot"], type = "scatter", mode = "lines", 
                line = list(color = 'rgb(27, 27, 244)', width = 2)) %>%
      add_lines(x = input$tmaxtris, y = c(0,2), line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = xvar, yaxis = yvar2)
    
    p3 <- plot_ly(data = PO4_gav_table, x = PO4_gav_table[,1], y = PO4_gav_table[,"PTH_p"]/PO4_gav_table[1,"PTH_p"], type = "scatter", mode = "lines",
                  line = list(color = 'black', width = 2)) %>%
      add_trace(x=gavevents$times, y=gavevents$PTH_val, mode = 'markers', symbols = "o", marker = list(size = 10, color = 'black'),
                error_y = list(array = gavevents$err_PTH, color = 'black'), line = list(color = 'white')) %>%
      add_lines(x = PO4_gav_table[,1], y = PO4_gav_table[,"PTH_p"]/PO4_gav_table[1,"PTH_p"], type = "scatter", mode = "lines", 
                line = list(color = 'black', width = 2)) %>%
      add_lines(x = input$tmaxtris, y = c(0,20), line = list(size = 6, color = 'orange', dashed = "dashdot")) %>%
      layout(xaxis = xvar, yaxis = yvar3)
    
    p <- subplot(p1, p2, p3, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.05)
    
    hide_legend(p)
    
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Update progress bar when the user triggers activities
  #  
  #-------------------------------------------------------------------------
  counter <- reactiveValues(pb = 0, php1 = 0, hypoD3 = 0, hypopara = 0, Ca_inject = 0, PO4_inject = 0, PO4_gav = 0)
  
  observeEvent(input$run_php1 | input$run_hypoD3 | input$run_hypopara 
               | input$Ca_inject | input$PO4_inject | input$PO4_gav,{ # each time a checkbox is clicked
                 
                 #if(!is.null(counter_quiz$php1) || !is.null(counter_quiz$hypopara) || !is.null(counter_quiz$hypoD3)){
                 
                 if (counter$pb == 99){ # to avoid to have 99% instead of 100%, since 100/6 is not an integer
                   
                   counter$pb <- 100
                   updateProgressBar(session = session, id = "progress_bar", value = counter$pb)
                   
                 }
                 
                 if(counter$pb <= 100){
                   
                   if(input$run_php1 == "TRUE" && counter$php1 == 0){ # if php1 is clicked
                     
                     counter$pb <- counter$pb + 16.5
                     counter$php1 <- 1 # counter set to 1 so that the next click on php1 does not update the whole progress again
                     updateProgressBar(session = session, id = "progress_bar", value = counter$pb)
                     
                   }
                   
                   if(input$run_hypoD3 == "TRUE" && counter$hypoD3 == 0){ # if hypoD3 is clicked
                     
                     counter$pb <- counter$pb + 16.5
                     counter$hypoD3 <- 1
                     updateProgressBar(session = session, id = "progress_bar", value = counter$pb)
                     
                   }
                   
                   if(input$run_hypopara == "TRUE" && counter$hypopara == 0){ # if hypopara is clicked
                     
                     counter$pb <- counter$pb + 16.5
                     counter$hypopara <- 1
                     updateProgressBar(session = session, id = "progress_bar", value = counter$pb)
                     
                   }
                   
                   if(input$Ca_inject == "TRUE" && counter$Ca_inject == 0){ # if Ca_inject is clicked
                     
                     counter$pb <- counter$pb + 16.5
                     counter$Ca_inject <- 1
                     updateProgressBar(session = session, id = "progress_bar", value = counter$pb)
                     
                   }
                   
                   if(input$PO4_inject == "TRUE" && counter$PO4_inject == 0){ # if PO4_inject is clicked
                     
                     counter$pb <- counter$pb + 16.5
                     counter$PO4_inject <- 1
                     updateProgressBar(session = session, id = "progress_bar", value = counter$pb)
                     
                   }
                   
                   if(input$PO4_gav == "TRUE" && counter$PO4_gav == 0){ # if PO4_gav is clicked
                     
                     counter$pb <- counter$pb + 16.5
                     counter$PO4_gav <- 1
                     updateProgressBar(session = session, id = "progress_bar", value = counter$pb)
                     
                   }
                   
                 }
                   
                 #}
                 
               })
  
  
  # send an alert to prevent that the activity is finished
  
  observe({
    
    if (counter$pb == 100){ # activity is finished
      
      sendSweetAlert(messageId = "successSw", title = "Congratulations ...", 
                     text = "You just finished this tutorial. You can then continue to explore!", type = "success")
    }
    
  })
  
  
  #------------------------------------------------------------------------- 
  #  
  #  Update the time navigation bar for Ca/EGTA inject, PO4 inject and 
  #  PO4 gavage by clicking on back or next buttons
  #  
  #-------------------------------------------------------------------------
  
  observeEvent(input$back1,{ # if the user clicks on back
    
    updateSliderInput(session, inputId = "tmax", "Current Time", value = input$tmax - 10, min = 1, max = 120, step = 1) # step decrease: 10
    updateSliderInput(session, inputId = "tmaxbis", "Current Time", value = input$tmaxbis - 10, min = 1, max = 250, step = 1) 
    updateSliderInput(session, inputId = "tmaxtris", "Current Time", value = input$tmaxtris - 10, min = 1, max = 250, step = 1) 
    
  })
  
  observeEvent(input$next1,{ # if the user clicks on next
    
    updateSliderInput(session, inputId = "tmax", "Current Time", value = input$tmax + 10, min = 1, max = 120, step = 1) # step increase: 10
    updateSliderInput(session, inputId = "tmaxbis", "Current Time", value = input$tmaxbis + 10, min = 1, max = 250, step = 1) 
    updateSliderInput(session, inputId = "tmaxtris", "Current Time", value = input$tmaxtris + 10, min = 1, max = 250, step = 1) 
    
  })
  
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
  #  Prevent user from selecting multiple boxes using shinyjs functions
  #  
  #-------------------------------------------------------------------------
  
  observeEvent(input$run_php1, {
    # Change the following line for more examples
    if (input$run_php1 == "TRUE"){
      disable("run_hypopara")
      disable("run_hypoD3")
      disable("Ca_inject")
      disable("PO4_inject")
      disable("PO4_gav")
    }else{
      enable("run_hypopara")
      enable("run_hypoD3")
      enable("Ca_inject")
      enable("PO4_inject")
      enable("PO4_gav")
    }
  })
  
  observeEvent(input$run_hypoD3, {
    # Change the following line for more examples
    if (input$run_hypoD3 == "TRUE"){
      disable("run_hypopara")
      disable("run_php1")
      disable("Ca_inject")
      disable("PO4_inject")
      disable("PO4_gav")
    }else{
      enable("run_hypopara")
      enable("run_php1")
      enable("Ca_inject")
      enable("PO4_inject")
      enable("PO4_gav")
    }
  })
  
  observeEvent(input$run_hypopara, {
    # Change the following line for more examples
    if (input$run_hypopara == "TRUE"){
      disable("run_hypoD3")
      disable("run_php1")
      disable("Ca_inject")
      disable("PO4_inject")
      disable("PO4_gav")
    }else{
      enable("run_hypoD3")
      enable("run_php1")
      enable("Ca_inject")
      enable("PO4_inject")
      enable("PO4_gav")
    }
  })
  
  observeEvent(input$Ca_inject, {
    # Change the following line for more examples
    if (input$Ca_inject == "TRUE"){
      disable("run_hypoD3")
      disable("run_php1")
      disable("run_hypopara")
      disable("PO4_inject")
      disable("PO4_gav")
    }else{
      enable("run_hypoD3")
      enable("run_php1")
      enable("run_hypopara")
      enable("PO4_inject")
      enable("PO4_gav")
    }
  })
  
  observeEvent(input$PO4_inject, {
    # Change the following line for more examples
    if (input$PO4_inject == "TRUE"){
      disable("run_hypoD3")
      disable("run_php1")
      disable("run_hypopara")
      disable("Ca_inject")
      disable("PO4_gav")
    }else{
      enable("run_hypoD3")
      enable("run_php1")
      enable("run_hypopara")
      enable("Ca_inject")
      enable("PO4_gav")
    }
  })
  
  observeEvent(input$PO4_gav, {
    # Change the following line for more examples
    if (input$PO4_gav == "TRUE"){
      disable("run_hypoD3")
      disable("run_php1")
      disable("run_hypopara")
      disable("Ca_inject")
      disable("PO4_inject")
    }else{
      enable("run_hypoD3")
      enable("run_php1")
      enable("run_hypopara")
      enable("Ca_inject")
      enable("PO4_inject")
    }
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Useful tasks such as save, reset, load ...
  #  
  #-------------------------------------------------------------------------
  
  # reset all the values of box inputs as well as graphs
  
  observeEvent(input$resetAll, {
    
    # reset checkbox input
    reset("boxinput")
    
    # reset all counters
    counter <- reactiveValues(pb = 0, php1 = 0, hypoD3 = 0, hypopara = 0, Ca_inject = 0, PO4_inject = 0, PO4_gav = 0)
    score <- reactiveValues(php1 = NULL, hypopara = NULL, hypoD3 = NULL)
    counter_quiz <- reactiveValues(php1 = NULL, hypopara = NULL, hypoD3 = NULL)
    counter_nav <- reactiveValues(diagram = 0)
    
    # reset diagram 
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
  
  
  # When the button is clicked, wrap the code in a call to
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