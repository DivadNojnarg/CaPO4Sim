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

# css style for visNetwork export

css_export <- paste0("float: left;
              -webkit-border-radius: 10;
              -moz-border-radius: 10;
              border-radius: 10px;
              font-family: Arial;
              color: #ffffff;
              font-size: 12px;
              background: #090a0a;
              padding: 4px 8px 4px 4px;
              text-decoration: none;
              position: absolute;
              top: -800px;")

css_export_zoom <- paste0("float: left;
                          -webkit-border-radius: 10;
                          -moz-border-radius: 10;
                          border-radius: 10px;
                          font-family: Arial;
                          color: #ffffff;
                          font-size: 12px;
                          background: #090a0a;
                          padding: 4px 8px 4px 4px;
                          text-decoration: none;
                          position: absolute;
                          top: -400px;")

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
    
    param_event <- list(
      values = events,
      
      text = list(list("Calcium intake has been increased!",
                       "Calcium intake has been decreased!"),
                  list("Phosphate intake has been increased!",
                       "Phosphate intake has been decreased!"),
                  list("Calcium flux into bone has been increased!",
                       "Calcium flux into bone has been decreased!"),
                  list("Calcium storage in the bone rapid pool has 
                        been increased!",
                       "Calcium storage in the bone rapid pool has 
                        been decreased!"),
                  list("PO4 storage in the bone rapid pool has been 
                        increased!",
                       "PO4 storage in the bone rapid pool has been 
                        decreased!"),
                  list("Calcium release from the bone rapid pool has 
                        been increased!",
                       "Calcium release from the bone rapid pool has 
                        been decreased!"),
                  list("PO4 release from the bone rapid pool has 
                        been increased!",
                       "PO4 release from the bone rapid pool has 
                        been decreased!"),
                  list("Calcium/Phosphate release from the bone 
                       (resorption) has been increased!",
                       "Calcium/Phosphate release from the bone 
                       (resorption) has been decreased!"),
                  list("Calcium/Phosphate release from the bone 
                       (resorption) has been increased!",
                       "Calcium/Phosphate release from the bone 
                        (resorption) has been decreased!"),
                  list("GFR has been increased!",
                       "GFR has been decreased!"),
                  list("PO4 storage into cells has been increased!",
                       "PO4 storage into cells has been decreased!"),
                  list("PO4 release from cells to plasma has been 
                        increased!",
                       "PO4 release from cells to plasma has been 
                       decreased!"),
                  list("PTH synthesis has been increased!",
                       "PTH synthesis has been decreased!"),
                  list("25(OH)D stock has been increased!",
                       "25(OH)D stock has been decreased!"),
                  list("Vitamin D3 degradation has been increased!",
                       "Vitamin D3 degradation has been decreased!"),
                  list("FGF23 synthesis has been increased!",
                       "FGF23 synthesis has been decreased!")
      ),
      
      edges_id = list(1,1,8,4,5,6,7,10,10,11,16,17,c(24,25,26),
                      c(27,28,29,30,31,32),c(27,28,29,30,31,32), c(33,34))
      
    )
    
  } else {
    
    param_event <- list(
      values = events,
      
      text = list(PTH_synth = list("",
                                   ""),
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
  event_id <- which(param_event$values != 1)
  # if a previous event is already active
  # only select the last new event
  ifelse(length(event_id)  > 1,
         event_target <- event_id[[length(event_id)]],
         event_target <- event_id)

  # select the related edges on the network
  edges_id_network <- as.numeric(unlist(param_event$edges_id[event_target]))
  
  # for notification display
  # split notif into 2 parts: one for increase, other for decrease
  notif_increase <- unlist(lapply(param_event$text, function(x, ind = 1) x[ind]))
  notif_decrease <- unlist(lapply(param_event$text, function(x, ind = 2) x[ind]))

  message <- ifelse(param_event$values[event_target] > 1, 
                    notif_increase[event_target],
                    notif_decrease[event_target])
  
  if (!is.null(message)) 
    ifelse(param_event$value[event_target] != 1,
           showNotification(paste(message[length(message)]), # does not work totally
                            type = "warning", 
                            duration = 2), NULL)
  
  return(list(edges_id_network, event_target, 
              param_event$values, param_event$edges_id))
  
}


# Function that allows to light the graph when fluxes change:
# arrows are in green when fluxes are increased and in
# red when fluxes are decreased
# takes edges, network (by default set to network_Ca), out and
# events as arguments

flux_lighting <- function(edges, network = "network_Ca", out, events){ 
  
  # calculate the difference between live fluxes and base-case values
  # depending on the graph selection
  if (network == "network_Ca") {
    # round by 0.1, otherwise to much precision can cause problems
    # low precision also
    calc_change_t <- round(calc_change(out)[1:14],1)
    # index of arrows in the Ca network (which are fluxes and not regulations)
    index <- c(3,10,29,7,6,32,8,23,4,31,5,30,22,21)
    calc_change_t <- rbind(calc_change_t, index)
  } else { # should use else if when other graphs will be added
    calc_change_t <- round(calc_change(out)[c(15,17:18)])
    index <- c(1,2,3) # index arrows in the PTH network
    calc_change_t <- rbind(calc_change_t, index)
  }
  
  # calculate which element in the sum table is different of 0 and store the index
  flux_changed_index <- which(calc_change_t[1,] != 0) 
  # convert to arrow index in the interactive diagramm
  arrow_index <- as.numeric(t(calc_change_t[2,flux_changed_index])) 
  
  if (!is.null(flux_changed_index)) {
    for (i in (seq_along(calc_change_t))) {
      arrow_index_i <- arrow_index[i] 
      # change edge color according to an increase or decrease of the flux
      ifelse(calc_change_t[[i]][1] > 0, 
             edges$color.color[arrow_index_i] <- "green", 
             edges$color.color[arrow_index_i] <- "red")
    }
    
  }
  
  # proceed to perturbation highlithing
  selected_edges <- arrow_lighting(events, edges, network)
  edges_id_network <- selected_edges[[1]]
  event_target <- selected_edges[[2]]
  param_event_values <- selected_edges[[3]]
  
  # increase/decrease the size of the corresponding edge
  if (network == "network_Ca") {
    # need to take care when parameters correspond to
    # degradation rate (edge$width is thus inverted) 
    ifelse(param_event_values[15] == 1,
         edges$width[edges_id_network] <- ifelse(param_event_values[event_target] > 1, 12, 2),
         edges$width[edges_id_network] <- ifelse(param_event_values[event_target] < 1, 12, 2))
    
  } else {
    edges$width[edges_id_network] <- ifelse(param_event_values[event_target] > 1, 12, 2)
  }
  
  
  # update the network
  visNetworkProxy(network) %>% 
    visSetSelection(edgesId = edges_id_network) %>%
    visUpdateEdges(edges = edges)
}


# plot_node function will plot the concentrations or
# quantities related to the latest selected node
# nodes can be input$current_node_id and out is out()
# Finally, also needs parameters_bis

title_size <- list(size = 10)

plot_node <- function(node, out, parameters_bis) {
  
  if (!is.null(node) && sum(node ==  c(1:3,7:12,14:15,17:21)) != 1) {
    
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
          font = title_size,
          xaxis = xvar,
          yaxis = list(title = "y"),
          updatemenus = list(
            list(
              type = "buttons",
              direction = "right",
              xanchor = 'center',
              yanchor = "bottom",
              #pad = list('r'= 0, 't'= 10, 'b' = 10),
              x = 0.5,
              y = -0.4,
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
        ) %>%
        config(displayModeBar = FALSE)
      
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
          title = "Rapid bone pool Ca and PO4 content",
          font = title_size,
          xaxis = xvar,
          yaxis = list(title = "Quantities (mmol)"),
          updatemenus = list(
            list(
              type = "buttons",
              direction = "right",
              xanchor = 'center',
              yanchor = "bottom",
              #pad = list('r'= 0, 't'= 10, 'b' = 10),
              x = 0.5,
              y = -0.4,
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
        ) %>%
        config(displayModeBar = FALSE)
      
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
          title = "Deep bone pool Ca and PO4 content",
          font = title_size,
          xaxis = xvar,
          yaxis = list(title = "Quantities (mmol"),
          updatemenus = list(
            list(
              type = "buttons",
              direction = "right",
              xanchor = 'center',
              yanchor = "bottom",
              #pad = list('r'= 0, 't'= 10, 'b' = 10),
              x = 0.5,
              y = -0.4,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE)),
                     label = "Ca bone"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE)),
                     label = "PO4 bone"),
                
                list(method = "restyle",
                     args = list("visible", list(TRUE, TRUE, FALSE)),
                     label = "Both")))
          )
        ) %>%
        config(displayModeBar = FALSE)
      
    } else {
      
      # other cases: need to convert graph indexes to the solver indexes
      # which are totally different (and is a big problem!!!). 
      # 0 correspond to nodes in previous cases or not interesting
      node_Ca_list <- data.frame(id = c(rep(0,12),12,rep(0,2),2),
                                 names = c(rep("",12),"PO4 quantity in cells",
                                           rep("",2),"PTH quantity in parathyroid glands"),
                                 units = c(rep("",12),"mmol",
                                           rep("",2),"pmol"))
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
               font = title_size,
               xaxis = xvar,
               yaxis = yvar) %>%
        config(displayModeBar = FALSE)
      
    }
    
  } else {
    
    # node not allowed to plot
    p <- plot_ly() %>%
      add_annotations("Please select another node!", 
                      showarrow = FALSE, 
                      font = list(color = "red", size = 10)) %>%
      config(displayModeBar = FALSE)
  }
}


# plot_edge function will plot the flux
# related to the last selected edge
# edge can be input$current_edge_id and out 
# contains all the variables returned by the 
# solver

plot_edge <- function(edge, out) {
  
  time <- out[,1]
  xvar <- list(title = "time (min)", 
               range = c(0, max(time)))
  
  # avoid edges that are not fluxes in the network
  if (edge == 1 | edge == 3 | edge == 11 |
      # sum counts the number of true, only one is enough
      sum(edge ==  18:34) == 1) {
    p <- plot_ly() %>%
      add_annotations("Please select another edge!", 
                      showarrow = FALSE, 
                      font = list(color = "red", size = 10)) %>%
      config(displayModeBar = FALSE)
    
  } else {
    
    # select edges where Ca and PO4 fluxes
    # have the same regulation
    if (edge == 2) {
      
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
          font = title_size,
          xaxis = xvar,
          yaxis = yvar,
          updatemenus = list(
            list(
              type = "buttons",
              direction = "right",
              xanchor = 'center',
              yanchor = "bottom",
              #pad = list('r'= 0, 't'= 10, 'b' = 10),
              x = 0.5,
              y = -0.4,
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
        ) %>%
        config(displayModeBar = FALSE)
      
    } else if (edge == 10) {
      
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
          font = title_size,
          xaxis = xvar,
          yaxis = yvar,
          updatemenus = list(
            list(
              type = "buttons",
              direction = "right",
              xanchor = 'center',
              yanchor = "bottom",
              #pad = list('r'= 0, 't'= 10, 'b' = 10),
              x = 0.5,
              y = -0.4,
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
        ) %>%
        config(displayModeBar = FALSE)
      
    } else {
      
      # other cases: need to convert graph indexes to the solver indexes
      # which are totally different (and is a big problem!!!). 
      # 0 correspond to arrows in previous cases or not interesting
      edge_Ca_list <- c(rep(0,3),4,6,8,0,12,13,14,rep(0,10),
                        17,16,33,rep(0,5),15,7,5,9)
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
               font = title_size,
               xaxis = xvar,
               yaxis = yvar) %>%
        config(displayModeBar = F)
      
    }
    
  }
}


# Function to reset sliders input to their original values
# Takes a reset_table, network and edges as arguments
# reset_table contains the state of reset button (0 if
# not used) as well as the related sliders_id

sliders_reset <- function(button_states, input) {
  
  # stock the previous state of buttons in
  # reactiveValues so as to compare with
  # the current state
  button_states$values <- append(button_states$values, 
                                 list(c(input$resetPTHsynthesis[1],
                                        input$resetPTHexocytosis[1],
                                        input$resetPTHexocytosisinhib[1],
                                        input$resetD3inact[1],
                                        input$resetD3deg[1],
                                        input$resetFGFsynth[1],
                                        input$resetCaintake[1],
                                        input$resetPintake[1],
                                        input$resetkpCa[1],
                                        input$resetkfCa[1],
                                        input$resetkpP[1],
                                        input$resetkfP[1],
                                        input$resetacCa[1],
                                        input$resetresmin[1],
                                        input$resetresmax[1],
                                        input$resetkpc[1],
                                        input$resetkcp[1])))         
  
  # associate each reset button to its related slider
  reset_vector <- c("k_prod_PTHg", 
                  "beta_exo_PTHg",
                  "gamma_exo_PTHg",
                  "D3_inact",
                  "k_deg_D3",
                  "k_prod_FGF",
                  "I_Ca",
                  "I_P",
                  "k_p_Ca",
                  "k_f_Ca",
                  "k_p_P",
                  "k_f_P",
                  "Lambda_ac_Ca",
                  "Lambda_res_min",
                  "delta_res_max",
                  "k_pc",
                  "k_cp")
  
  # store the temp state of buttons
  states <- button_states$values
  last_state <- states[[length(states)]]
  
  # select which reset buttons are selected
  if (length(states) <= 1) {
    # compare the current state with 0
    reset_target <- which(unlist(states) != 0)
  } else {
    # compare the current state with the previous one
    penultimate_state <- states[[length(states) - 1]]
    reset_target <- which(penultimate_state != last_state)
  }
  
  # reset the corresponding target(s) in the table
  shinyjs::reset(reset_vector[reset_target])
  
  #update the network
  #visNetworkProxy(network) %>%
  #  visUpdateEdges(edges = edges)
  
}


# Function graphs_reset remove all changes
# of color/size of arrows/nodes
# It takes graph id as argument as well as
# edges 

graphs_reset <- function(network, edges) {
  
  visNetworkProxy(network) %>%
    visUpdateEdges(edges = edges)
}


# Functions that handle help text generation
# help_text_generator will generate notifications
# whereas help_text_destructor will remove these
# notifications

help_text <- data.frame(
  id = c("menu_notif",
         "graph_notif",
         "diagram_notif",
         "control_notif"),
  
  text = c("In this panel you can enable/disable notifications, 
           bookmark the state of your app to share it with colleagues, 
           save it, load the last state you saved and change
           the global theme.",
           
           "In this panel are displayed the graph of CaPO4 homeostasis. 
           To see results, start by clicking on a node and/or an edge on 
           the interactive diagram. The value of tmax which is the maximum 
           time of simulation can be increased or decreased as required 
           (but higher than 0).",
           
           "In this panel is displayed the interactive diagram (see legend). 
           Basically, when a parameter is changed in the control center, 
           initial perturbations are shown in yellow. The arrow size increases 
           if it is a stimulatory effect and inversely. Fluxes are shown 
           in red if they decrease or in green if they are enhanced. 
           Colors correspond to the final state of the system
           (which is the value of tmax in minutes).",
           
           "In this panel you can select several parameters 
           and change their value using sliders.")
)

# notification builder
# do not need any argument
help_text_generator <- function(){
  
  for (i in seq_along(help_text$id)) { 
    showNotification(id = help_text$id[[i]],
                     paste(help_text$text[[i]]),
                     duration = 9999, # sufficient amount of time
                     closeButton = TRUE,
                     type = "error")
  }
  
}

# notification eraser
# needs the session argument !!!
help_text_destructor <- function(session){
  
  for (i in seq_along(help_text$id)) { 
    removeNotification(id = help_text$id[[i]], session)
  }
  
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
