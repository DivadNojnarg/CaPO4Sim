#------------------------------------------------------------------------- 
#  This codes load all packages needed by the application.
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
#  Load server elements and usefull functions
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



# function that allows to light the graph when event occurs:
# arrows are in green when fluxes are increased and in
# red when fluxes are decreased

network_lighting <- function(edges, network = "network_Ca", out){ # takes edge and network (by default set to network_Ca)
  
  # round by default, otherwise to much precision can cause problems
  calc_change_t <- round(calc_change(out)) 
  calc_change_t$X <- NULL # remove column X
  
  # calculate the difference between live fluxes and base-case values
  if (network == "network_Ca") {
    index <- c(3,10,29,7,6,32,8,23,4,31,5,30,22,21) # index of arrows in the Ca network (which are fluxes and not regulations)
  } else { # should use else if when other graphs will be added
    index <- c(1,2,3) # index arrows in the PTH network
  }
  
  calc_change_t <- rbind(calc_change_t, index)
  
  flux_changed_index <- which(calc_change_t[1,] != 0) # calculate which element in the sum table is different of 0 and store the index
  arrow_index <- as.numeric(t(calc_change_t[2,flux_changed_index])) # convert to arrow index in the interactive diagramm
  
  if (!is.null(flux_changed_index)) {
    for (i in (1:ncol(calc_change_t))) {
      arrow_index_i <- arrow_index[i] # change edge color according to an increase or decrease of the flux
      ifelse(calc_change_t[[i]][1] > 0, 
             edges$color.color[arrow_index_i] <- "green", 
             edges$color.color[arrow_index_i] <- "red")
    }
    
  }
  
  visNetworkProxy(network) %>% # update the network: two choices for the moment
    visUpdateEdges(edges = edges)
}


# plot_node function will plot the concentrations or
# quantities related to the latest selected node
# nodes can be input$current_node_id and out is out()
plot_node <- function(node, out, parameters_bis) {
  
  if (!is.null(node) | node != 4) {
    
    # set the x/y-axis ranges
    time <- out[,1]
    xvar <- list(title = "time (min)", 
                 range = c(0, max(time)))
    #yvar <- list(title = "Concentrations (mM)", 
    #             range = c(min(out[,node])*0.8,
    #                       max(out[,node])*1.2))
    
    # plot the current selected node
    if (node == 4) {
      p <- plot_ly(out, 
                   x = time, 
                   mode = "lines") %>%
        add_lines(y = out[,"Ca_p"],
                  name = "Cap",
                  line = list(color = 'rgb(27, 102, 244)', width = 2), 
                  visible = F) %>%
        add_lines(y = out[,"PO4_p"], 
                  name = "PO4p",
                  line = list(color = 'rgb(244, 27, 27)', width = 2), 
                  visible = F) %>%
        add_lines(y = out[,"PTH_p"]/parameters_bis["Vp"],
                  name = "PTHp",
                  line = list(color = 'black', width = 2),
                  visible = F) %>%
        add_lines(y = out[,"D3_p"],
                  name = "D3p",
                  line = list(color = 'black', width = 2),
                  visible = F) %>%
        add_lines(y = out[,"FGF_p"],
                  name = "FGFp",
                  line = list(color = 'black', width = 2),
                  visible = F) %>%
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
    }
    
  } else {
    
    p <- plot_ly() %>%
      add_annotations("Please select another node!", 
                      showarrow = FALSE, 
                      font = list(color = "red", size = 20))
    
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
