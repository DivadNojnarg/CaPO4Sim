#------------------------------------------------------------------------- 
#  This codes loads all packages needed by the application.
#  Moreover, it contains all mandatory UI and server elements. 
#  Some useful functions are defined here so as to lighten
#  the server code
#
#-------------------------------------------------------------------------

#loading packages
library(shiny)
library(plotly)
library(deSolve)
require(visNetwork)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjqui)
library(bsplus)
library(sweetalertR)

# Load the template components of UI
source("header.R")
source("sidebar.R")
source("body.R")

# JS code for closing shiny app with a button
#jscode <- "shinyjs.closeWindow = function() { window.close(); }"

#------------------------------------------------------------------------- 
#  
#  
#  Load server elements and useful functions
#  
#
#-------------------------------------------------------------------------

# Load usefull scripts
source("cap_fixed_parameters.R")
source("calcium_phosphate_core.R") # core model
source("calc_change.R")
source("box_close.R")

# initial conditions
state <- c("PTH_g" = 1288.19, "PTH_p" = 0.0687, 
           "D3_p" = 564.2664, "FGF_p" = 16.78112, 
           "Ca_p" = 1.2061,"Ca_f" = 1.8363, "Ca_b" = 250, 
           "PO4_p" = 1.4784, "PO4_f" = 0.7922, "PO4_b" = 90, 
           "PO4_c" = 2.7719,"CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, 
           "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, 
           "CaProt_p" = 1.4518, "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, 
           "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0)

# Function that allows to light the graph when an event occurs:
# arrows are in yellow to show perturbations
# take event argument, edges and network
# This function is then called by flux_lighting
# to update edges at the same time

arrow_lighting <- function(events, edges, network) {
  
  if (network == "network_Ca") {
    
    param_event <- list(values = events,
                        
                        text = list(Ca_intake = list("Calcium intake has been increased!",
                                                     "Calcium intake has been decreased!"),
                                    PO4_intake = list("Phosphate intake has been increased!",
                                                      "Phosphate intake has been decreased!"),
                                    Ca_ac = list("Calcium flux into bone has been increased!",
                                                 "Calcium flux into bone has been decreased!"),
                                    Ca_pf = list("Calcium storage in the bone rapid pool has 
                                                     been increased!",
                                                 "Calcium storage in the bone rapid pool has 
                                                     been decreased!"),
                                    PO4_pf = list("PO4 storage in the bone rapid pool has been 
                                                      increased!",
                                                  "PO4 storage in the bone rapid pool has been 
                                                      decreased!"),
                                    Ca_fp = list("Calcium release from the bone rapid pool has 
                                                     been increased!",
                                                 "Calcium release from the bone rapid pool has 
                                                     been decreased!"),
                                    PO4_fp = list("PO4 release from the bone rapid pool has 
                                                      been increased!",
                                                  "PO4 release from the bone rapid pool has 
                                                      been decreased!"),
                                    Res = list("Calcium/Phosphate release from the bone 
                                                   (resorption) has been increased!",
                                               "Calcium/Phosphate release from the bone 
                                                   (resorption) has been decreased!"),
                                    Res = list("Calcium/Phosphate release from the bone 
                                                   (resorption) has been increased!",
                                               "Calcium/Phosphate release from the bone 
                                                   (resorption) has been decreased!"),
                                    GFR = list("GFR has been increased!",
                                               "GFR has been decreased!"),
                                    PO4_pc = list("PO4 storage into cells has been increased!",
                                                  "PO4 storage into cells has been decreased!"),
                                    PO4_cp = list("PO4 release from cells to plasma has been 
                                                      increased!",
                                                  "PO4 release from cells to plasma has been 
                                                      decreased!"),
                                    PTH_synth = list("PTH synthesis has been increased!",
                                                     "PTH synthesis has been decreased!"),
                                    D3_synth = list("25(OH)D stock has been increased!",
                                                    "25(OH)D stock has been decreased!"),
                                    D3_deg = list("Vitamin D3 degradation has been increased!",
                                                  "Vitamin D3 degradation has been decreased!"),
                                    FGF_synth = list("FGF23 synthesis has been increased!",
                                                     "FGF23 synthesis has been decreased!")
                        ),
                        
                        edges_id = list(1,1,6,4,31,5,30,7,7,9,22,21,c(11,12,15),
                                        c(13,14,16,17),c(13,14,16,17), c(18,19))
                        
    )
  } else {
    
    param_event <- list(values = events,
                            
                            text = list(PTH_synth = list(),
                                        PTHg_exo = list("PTH exocytosis has been increased",
                                                        "PTH exocytosis has been decreased"),
                                        PTHg_exoinhib  = list("CaSR inhibition on PTH synthesis 
                                                            has been increased",
                                                              "CaSR inhibition on PTH synthesis 
                                                            has been decreased")
                            ),
                            
                            edges_id = list(1,3,4)
    )
  }

  # search for events which value are different of 1
  event_id <- which(param_event$value != 1)
  edges_id_network <- as.numeric(unlist(param_event$edges_id[event_id]))
 
  # for notification display
  message <- ifelse(param_event$value[event_id] > 1, 
                    param_event$text[[event_id]][1],
                    param_event$text[[event_id]][2])
  
  if (!is.null(message)) 
         ifelse(param_event$value[event_id] != 1,
                showNotification(paste(message), type = "warning", duration = 2), NULL)
  
  return(list(edges_id_network, event_id, param_event$values))
  
}


# Function that allows to light the graph when fluxes change:
# arrows are in green when fluxes are increased and in
# red when fluxes are decreased
# takes edges, network (by default set to network_Ca), out and
# events as arguments

flux_lighting <- function(edges, network = "network_Ca", out, events){ 
  
  # round by default, otherwise to much precision can cause problems
  calc_change_t <- round(calc_change(out)) 
  calc_change_t$X <- NULL # remove column X
  
  # calculate the difference between live fluxes and base-case values
  if (network == "network_Ca") {
    # index of arrows in the Ca network (which are fluxes and not regulations)
    index <- c(3,10,29,7,6,32,8,23,4,31,5,30,22,21) 
  } else { # should use else if when other graphs will be added
    index <- c(1,2,3) # index arrows in the PTH network
  }
  
  calc_change_t <- rbind(calc_change_t, index)
  
  # calculate which element in the sum table is different of 0 and store the index
  flux_changed_index <- which(calc_change_t[1,] != 0) 
  # convert to arrow index in the interactive diagramm
  arrow_index <- as.numeric(t(calc_change_t[2,flux_changed_index])) 
  
  if (!is.null(flux_changed_index)) {
    for (i in (1:ncol(calc_change_t))) {
      arrow_index_i <- arrow_index[i] 
      # change edge color according to an increase or decrease of the flux
      ifelse(calc_change_t[[i]][1] > 0, 
             edges$color.color[arrow_index_i] <- "green", 
             edges$color.color[arrow_index_i] <- "red")
    }
    
  }
  
  # proceed to perturbation highlithing
  selected_edges <- arrow_lighting(events, edges, network)
  # increase/decrease the size of the corresponding edge
  edges$width[selected_edges[[1]]] <- ifelse(selected_edges[[3]][selected_edges[[2]]] > 1, 12, 2)
  
  # update the network
  visNetworkProxy(network) %>% # update the network: two choices for the moment
    visSetSelection(edgesId = selected_edges[[1]]) %>%
    visUpdateEdges(edges = edges)
}


# plot_node function will plot the concentrations or
# quantities related to the latest selected node
# nodes can be input$current_node_id and out is out()
# Finally, also needs parameters_bis

plot_node <- function(node, out, parameters_bis) {
  
  if (!is.null(node) && sum(node ==  c(1:3,7:8,10:12,14:15)) != 1) {
    
    # set the x/y-axis ranges
    time <- out[,1]
    xvar <- list(title = "time (min)", 
                 range = c(0, max(time)))
    #yvar <- list(title = "Concentrations (mM)", 
    #             range = c(min(out[,node])*0.8,
    #                       max(out[,node])*1.2))
    
    # plasma compartment
    if (node == 4) {
      p <- plot_ly(out, 
                   x = time, 
                   mode = "lines") %>%
        add_lines(y = out[,"Ca_p"],
                  name = "Cap",
                  line = list(color = 'rgb(27, 102, 244)', width = 2), 
                  visible = TRUE) %>%
        add_lines(y = out[,"PO4_p"], 
                  name = "PO4p",
                  line = list(color = 'rgb(244, 27, 27)', width = 2), 
                  visible = FALSE) %>%
        add_lines(y = out[,"PTH_p"]/parameters_bis["Vp"],
                  name = "PTHp",
                  line = list(color = 'black', width = 2),
                  visible = FALSE) %>%
        add_lines(y = out[,"D3_p"],
                  name = "D3p",
                  line = list(color = 'black', width = 2),
                  visible = FALSE) %>%
        add_lines(y = out[,"FGF_p"],
                  name = "FGFp",
                  line = list(color = 'black', width = 2),
                  visible = FALSE) %>%
        layout(
          title = "Plasma compartment concentrations",
          xaxis = xvar,
          yaxis = list(title = "y"),
          updatemenus = list(
            list(
              y = 0.7,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE)),
                     label = "Cap"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE)),
                     label = "PO4p"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE)),
                     label = "PTHp"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE)),
                     label = "D3p"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE)),
                     label = "FGFp")))
          )
        )
    } else if (node == 5) {
      
      # rapid bone compartment
      p <- plot_ly(out, 
                   x = time, 
                   mode = "lines") %>%
        add_lines(y = out[,"Ca_f"],
                  name = "Caf",
                  line = list(color = 'rgb(27, 102, 244)', width = 2), 
                  visible = TRUE) %>%
        add_lines(y = out[,"PO4_f"], 
                  name = "PO4f",
                  line = list(color = 'rgb(244, 27, 27)', width = 2), 
                  visible = TRUE) %>%
        layout(
          title = "Rapid bone pool Ca and PO4 quantities (mmol)",
          xaxis = xvar,
          yaxis = list(title = "y"),
          updatemenus = list(
            list(
              y = 0.7,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE)),
                     label = "Ca fast bone"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE)),
                     label = "PO4 fast bone"),
                
                list(method = "restyle",
                     args = list("visible", list(TRUE, TRUE, FALSE)),
                     label = "Both")))
          )
        )
      
    } else if (node == 6) {
      
      # deep bone compartment
      p <- plot_ly(out, 
                   x = time, 
                   mode = "lines") %>%
        add_lines(y = out[,"Ca_b"],
                  name = "Cab",
                  line = list(color = 'rgb(27, 102, 244)', width = 2), 
                  visible = TRUE) %>%
        add_lines(y = out[,"PO4_b"], 
                  name = "PO4b",
                  line = list(color = 'rgb(244, 27, 27)', width = 2), 
                  visible = TRUE) %>%
        layout(
          title = "Deep bone pool Ca and PO4 quantities (mmol)",
          xaxis = xvar,
          yaxis = list(title = "y"),
          updatemenus = list(
            list(
              y = 0.7,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE)),
                     label = "Ca deep bone"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE)),
                     label = "PO4 deep bone"),
                
                list(method = "restyle",
                     args = list("visible", list(TRUE, TRUE, FALSE)),
                     label = "Both")))
          )
        )
    } else {
      
      # other cases: need to convert graph indexes to the solver indexes
      # which are totally different (and is a big problem!!!). 
      # 0 correspond to nodes in previous cases or not interesting
      node_Ca_list <- data.frame(id = c(rep(0,8), 2, rep(0,3),12),
                                 names = c(rep("",8),"PTH quantity in parathyroid glands",
                                           rep("",3),"PO4 quantity in cells"),
                                 units = c(rep("",8),"pmol",
                                           rep("",3),"mmol"))
      #names(node_Ca_list) <- c(rep("",8),"PTH quantity in parathyroid glands",
      #                         rep("",3),"PO4 quantity in cells")
      
      yvar <- list(title = paste("Quantity", "(", node_Ca_list$units[node], ")"), 
                   range = c(min(out[,node_Ca_list$id[node]]*0.8),
                             max(out[,node_Ca_list$id[node]]*1.2)))
      
      p <- plot_ly(out, 
                   x = time, 
                   y = out[,node_Ca_list$id[node]],
                   type = "scatter",
                   mode = "lines",
                   line = list(color = 'black', width = 2)) %>%
        layout(title = paste(node_Ca_list$names[node]),
               xaxis = xvar,
               yaxis = yvar)
      
    }
    
  } else {
    
    # node not allowed to plot
    p <- plot_ly() %>%
      add_annotations("Please select another node!", 
                      showarrow = FALSE, 
                      font = list(color = "red", size = 20))
  }
}


# plot_edge function will plot the flux
# related to the last selected edge
# edge can be input$current_edge_id and out is out()

plot_edge <- function(edge, out) {
  
  time <- out[,1]
  xvar <- list(title = "time (min)", 
               range = c(0, max(time)))
  
  # avoid edges that are not fluxes in the network
  if (edge == 1 | edge == 2 | edge == 9 |
      # sum counts the number of true, only one is enough
      sum(edge ==  11:20) == 1 | sum(edge == 24:28) == 1) {
    p <- plot_ly() %>%
      add_annotations("Please select another edge!", 
                      showarrow = FALSE, 
                      font = list(color = "red", size = 20))
  } else {
    
    # select edges where Ca and PO4 fluxes
    # have the same regulation
    if (edge == 3) {
      
      yvar <- list(title = "Flux (µmol/min)", 
                   range = c(min(out[,"Abs_int_Ca"]*1000*0.8),
                             max(out[,"Abs_int_Ca"]*1000*1.2)))
      
      p <- plot_ly(out, 
                   x = time, 
                   mode = "lines") %>%
        add_lines(y = out[,"Abs_int_Ca"]*1000,
                  name = "Ca abs",
                  line = list(color = 'rgb(27, 102, 244)', width = 2), 
                  visible = TRUE) %>%
        add_lines(y = out[,"Abs_int_PO4"]*1000, 
                  name = "PO4 abs",
                  line = list(color = 'rgb(244, 27, 27)', width = 2), 
                  visible = FALSE) %>%
        layout(
          title = "Intestinal absorption fluxes",
          xaxis = xvar,
          yaxis = yvar,
          updatemenus = list(
            list(
              y = 0.7,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE)),
                     label = "Ca"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE)),
                     label = "PO4"),
                
                list(method = "restyle",
                     args = list("visible", list(TRUE, TRUE, FALSE)),
                     label = "Both")))
          )
        )
      
    } else if (edge == 7) {
      
      # resorption case
      yvar <- list(title = "Flux (µmol/min)", 
                   range = c(min(out[,"Res_Ca"]*1000*0.8,
                                 out[,"Res_PO4"]*1000*0.8),
                             max(out[,"Res_Ca"]*1000*1.2,
                                 out[,"Res_Ca"]*1000*1.2)))
      
      p <- plot_ly(out, 
                   x = time,
                   mode = "lines") %>%
        add_lines(y = out[,"Res_Ca"]*1000,
                  name = "Ca resorption",
                  line = list(color = 'rgb(27, 102, 244)', width = 2), 
                  visible = TRUE) %>%
        add_lines(y = out[,"Res_PO4"]*1000, 
                  name = "PO4 resorption",
                  line = list(color = 'rgb(244, 27, 27)', width = 2), 
                  visible = FALSE) %>%
        layout(
          title = "Resorption fluxes",
          xaxis = xvar,
          yaxis = yvar,
          updatemenus = list(
            list(
              y = 0.7,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE)),
                     label = "Ca"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE)),
                     label = "PO4"),
                
                list(method = "restyle",
                     args = list("visible", list(TRUE, TRUE, FALSE)),
                     label = "Both")))
          )
        )
      
    } else {
      
      # other cases: need to convert graph indexes to the solver indexes
      # which are totally different (and is a big problem!!!). 
      # 0 correspond to arrows in previous cases or not interesting
      edge_Ca_list <- c(rep(0,3),34,35,30,0,32,33,24,rep(0,10),
                        39,38,33,rep(0,5),25,37,36,31)
      names(edge_Ca_list) <- c(rep("",3),"Rapid Ca storage in bone",
                               "Rapid Ca release from bone",
                               "Ca flux into bone",
                               "","Ca renal reabsorption",
                               "PO4 renal reabsorption",
                               "Urinary Ca excretion",
                               rep("",10),
                               "PO4 Cell release",
                               "PO4 Cell storage",
                               "PO4 renal reabsorption",
                               rep("",5),
                               "Urinary PO4 excretion",
                               "Rapid PO4 release from bone",
                               "Rapid PO4 storage in bone",
                               "PO4 flux into bone")
      
      yvar <- list(title = "Flux (µmol/min)", 
                   range = c(min(out[,edge_Ca_list[edge]]*1000*0.8),
                             max(out[,edge_Ca_list[edge]]*1000*1.2)))
      
      p <- plot_ly(out, 
                   x = time, 
                   y = out[,edge_Ca_list[edge]]*1000,
                   type = "scatter",
                   mode = "lines",
                   line = list(color = 'black', width = 2)) %>%
        layout(title = paste(names(edge_Ca_list)[edge]),
               xaxis = xvar,
               yaxis = yvar)
      
    }
    
  }
}


# Function to reset sliders input to their original values
# Takes a reset_table, network and edges as arguments
# reset_table contains the state of reset button (0 if
# not used) as well as the related sliders_id

sliders_reset <- function(reset_table, network, edges) {
  
  # select which reset buttons are selected
  reset_target <- which(reset_table$sliders$button_state != 0)
  if (length(reset_target) > 1) {
    # only keep the last selected element
    reset_target <- reset_target[-c(1:length(reset_target)-1)] 
  }

  # reset the corresponding target(s) in the table
  shinyjs::reset(reset_table$sliders$slider_id[reset_target])
  
  #update the network
  visNetworkProxy(network) %>%
    visUpdateEdges(edges = edges)
  
  #print(reset_target)
  #print(reset_table$sliders$button_id[reset_target])
}

#------------------------------------------------------------------------- 
#  
#  
#  Other elements: bookmarking, config,...
#  
#
#-------------------------------------------------------------------------

# Bookmarking
enableBookmarking(store = "server") # save to the disk
