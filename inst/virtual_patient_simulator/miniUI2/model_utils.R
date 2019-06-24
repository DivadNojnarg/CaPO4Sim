# Function that allows to light the graph when an event occurs:
# arrows are in yellow to show perturbations
# take event argument, edges and network
# This function is then called by flux_lighting
# to update edges at the same time
arrow_lighting <- function(events, edges, network) {

  if (network == "network_Ca") {
    param_event <- list(
      values = events,
      # do not use rep(6,2) to have 6,6//12,12 because of a bug
      edges_id = list(
        4,2,3,2,3,6,6,7,12,12,
        c(19,20,21),
        c(22,23,24,25,26,27),
        c(22,23,24,25,26,27),
        c(28,29)
      )
    )
  } else if (network == "network_PTH") {
    param_event <- list(
      values = events,
      edges_id = list(1,3,4,6,6)
    )
  } else if (network == "network_kidney_PT") {
    param_event <- list(
      values = events,
      edges_id = 1
    )
  } else if (network == "network_kidney_PT_PO4") {
    param_event <- list(
      values = events,
      edges_id = list(c(3,4), c(1,2))
    )
  } else if (network == "network_kidney_TAL") {
    param_event <- list(
      values = events,
      edges_id = 1
    )
  } else if (network == "network_kidney_DCT") {
    param_event <- list(
      values = events,
      edges_id = list(1, c(5,6), c(5,6))
    )
  } else if (network == "network_intestine") {
    param_event <- list(
      values = events,
      edges_id = list(c(1,2),c(1,2))
    )
  } else {
    param_event <- list(
      values = events,
      edges_id = list(1,5,5)
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

  return(list(edges_id_network, event_target,
              param_event$values, param_event$edges_id))

}



# highlitght arrows for dynamic events
# take out (result of integration by ode solver),
# edges and session as arguments. Nothing special
# is returned, except that the network is updated
arrow_lighting_live <- function(out, edges, session, t_target) {
  # restricted to the first 11 fluxes
  calc_change_t <- round(calc_change(out, t_target)[1:11])
  calc_change_t$X <- NULL # remove column X

  # calculate the difference between live fluxes and base-case values
  # index of arrows in the graph (which are fluxes and not regulations)
  index <- c(1,10,11,6,4,5,8,9,2,3,12)
  calc_change_t <- rbind(calc_change_t, index)

  # calculate which element in the sum table is different of 0 and store the index
  flux_changed_index <- which(calc_change_t[1,] != 0)
  # convert to arrow index in the interactive diagramm
  arrow_index <- as.numeric(t(calc_change_t[2, flux_changed_index]))

  if (!is.null(flux_changed_index)) {
    for (i in (1:ncol(calc_change_t))) {
      # change edge color according to an increase or decrease of the flux
      arrow_index_i <- arrow_index[i]
      ifelse(calc_change_t[[i]][1] > 0,
             edges$color.color[arrow_index_i] <- "green",
             edges$color.color[arrow_index_i] <- "red")
    }
  }
  visNetworkProxy("network_Ca") %>%
    visUpdateEdges(edges = edges)
}



# Function that allows to light the graph when fluxes change:
# arrows are in green when fluxes are increased and in
# red when fluxes are decreased
# takes edges, network (by default set to network_Ca), out and
# events as arguments
flux_lighting <- function(edges, network = "network_Ca", events, out, t_target){

  # calculate the difference between live fluxes and base-case values
  # depending on the graph selection
  if (network == "network_Ca") {
    # round by 0.1, otherwise too much precision might cause problems
    # low precision also
    calc_change_t <- round(calc_change(out, t_target)[1:11],1)
    # index of arrows in the Ca network (which are fluxes and not regulations)
    # except filtration which is not included
    index <- c(1,10,11,6,4,5,8,9,2,3,12)
    calc_change_t <- rbind(calc_change_t, index)
    # change arrowhead orientation for Ca flux between plasma and rapid bone
    edges$from[2] <- ifelse(calc_change_t[1,"Net_Ca_pf_change"] > 0, 2, 3)
    edges$to[2] <- ifelse(calc_change_t[1,"Net_Ca_pf_change"] > 0, 3, 2)
    # change arrowhead orientation for PO4 flux between plasma and rapid bone
    edges$from[3] <- ifelse(calc_change_t[1,"Net_PO4_pf_change"] > 0, 2, 3)
    edges$to[3] <- ifelse(calc_change_t[1,"Net_PO4_pf_change"] > 0, 3, 2)
    # change arrowhead orientation for PO4 flux between plasma and cells
    edges$from[12] <- ifelse(calc_change_t[1,"Net_PO4_pc_change"] > 0, 2, 8)
    edges$to[12] <- ifelse(calc_change_t[1,"Net_PO4_pc_change"] > 0, 8, 2)

    edges$arrows.to.enabled[c(2,3,12)] <- TRUE

  } else if (network == "network_PTH") {# should use else if when other graphs will be added
    calc_change_t <- round(calc_change(out, t_target)[c(12:17)])
    index <- c(1,6,5,4,3,2) # index arrows in the PTH network
    calc_change_t <- rbind(calc_change_t, index)
  } else if (network == "network_kidney_PT") {# PT network
    # the second arrow of PT is not part of calc_change so need
    # to do as if it is the same as for the first arrow
    calc_change_t <- as.data.frame(rep(round(calc_change(out, t_target)[19]),2))
    index <- c(1,2)
    calc_change_t <- rbind(calc_change_t, index)
  } else if (network == "network_kidney_PT_PO4") {
    # PTH and FGF23 have the same qualitative effect
    calc_change_t <- round(calc_change(out, t_target)[c(rep(27,2), rep(28,2))], 2)
    index <- c(4,3,2,1)
    calc_change_t <- rbind(calc_change_t, index)
  } else if (network == "network_kidney_TAL") {
    calc_change_t <- round(calc_change(out, t_target)[20:21])
    index <- c(2,3)
    calc_change_t <- rbind(calc_change_t, index)
  } else if (network == "network_kidney_DCT") {
    calc_change_t <- round(calc_change(out, t_target)[c(rep(22,4), rep(23,3))])
    index <- c(1:7)
    calc_change_t <- rbind(calc_change_t, index)
  } else if (network == "network_intestine") {
    calc_change_t <- round(calc_change(out, t_target)[rep(24,7)])
    index <- c(1:7)
    calc_change_t <- rbind(calc_change_t, index)
  } else {
    calc_change_t <- round(calc_change(out, t_target)[c(rep(25,4), rep(26,3))])
    index <- c(1:7)
    calc_change_t <- rbind(calc_change_t, index)
  }

  # calculate which element in the sum table is different of 0 and store the index
  flux_changed_index <- which(calc_change_t[1,] != 0)
  # convert to arrow index in the interactive diagramm
  arrow_index <- t(calc_change_t[2,flux_changed_index])


  # proceed to perturbation highlithing
  selected_edges <- arrow_lighting(events, edges, network)
  edges_id_network <- selected_edges[[1]]
  event_target <- selected_edges[[2]]
  param_event_values <- selected_edges[[3]]

  # edge color engine
  if (!is.null(flux_changed_index)) {
    for (i in (seq_along(calc_change_t))) {
      arrow_index_i <- arrow_index[i]
      if (is.element(arrow_index_i, edges_id_network)) {
        # if the edge is part of the selection
        edges$color.color[arrow_index_i] <- "yellow"
      } else {
        # change edge color according to an increase or decrease of the flux
        ifelse(calc_change_t[[i]][1] > 0,
               edges$color.color[arrow_index_i] <- "green",
               edges$color.color[arrow_index_i] <- "red")
      }
    }
  }

  # increase/decrease the size of the corresponding edge
  if (network == "network_Ca") {
    # need to take care when parameters correspond to
    # degradation rate (edge$width is thus inverted)
    # such as for vitamin D3 degradation
    ifelse(param_event_values[13] == 1,
           edges$width[edges_id_network] <- ifelse(param_event_values[event_target] > 1, 12, 2),
           edges$width[edges_id_network] <- ifelse(param_event_values[event_target] < 1, 12, 2))

  } else if (network == "network_kidney_DCT" | network == "network_bone") {
    ifelse(param_event_values[3] == 1,
           edges$width[edges_id_network] <- ifelse(param_event_values[event_target] > 1, 12, 2),
           edges$width[edges_id_network] <- ifelse(param_event_values[event_target] < 1, 12, 2))
  } else if (network == "network_intestine") {
    ifelse(param_event_values[2] == 1,
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
plot_node <- function(input, node, out, parms) {

  if (node == "null") {
    p <- plot_ly() %>%
      add_annotations(
        "Please select a node!",
        showarrow = FALSE,
        font = list(color = "red", size = 10)
      ) %>%
      config(displayModeBar = FALSE)
  } else {
    if (sum(node ==  c(1,5:7,9:10,12:16)) != 1) {

      # set the x/y-axis ranges
      time <- out[,1]
      xvar <- list(
        title = "time (min)",
        range = c(0, max(time))
      )

      # plasma compartment
      if (node == 2) {
        p <- plot_ly(out,
                     x = time) %>%
          add_lines(y = round(out[,"Ca_p"], 3),
                    ymin = 0.5 * min(out[,"Ca_p"]),
                    ymax = 1.5 * max(out[,"Ca_p"]),
                    name = "Ca2+p (mM)",
                    line = list(color = 'rgb(27, 102, 244)', width = 2),
                    visible = TRUE) %>%
          add_lines(y = round(out[,"PO4_p"], 3),
                    ymin = 0.5 * min(out[,"PO4_p"]),
                    ymax = 1.5 * max(out[,"PO4_p"]),
                    name = "PO4p (mM)",
                    line = list(color = 'rgb(244, 27, 27)', width = 2),
                    visible = FALSE) %>%
          add_lines(y = round(out[,"PTH_p"]/parms["Vp"], 1),
                    ymin = 0.5 * min(out[,"PTH_p"]),
                    ymax = 1.5 * max(out[,"PTH_p"]),
                    name = "PTHp (pM)",
                    line = list(color = 'black', width = 2),
                    visible = FALSE) %>%
          # rescale D3
          add_lines(y = out[,"D3_p"] / 4,
                    ymin = 0.5 * min(out[,"D3_p"] / 4),
                    ymax = 1.5 * max(out[,"D3_p"] / 4),
                    name = "1,25D3p (pM)",
                    line = list(color = 'black', width = 2),
                    visible = FALSE) %>%
          # rescale FGF23
          add_lines(y = round(out[,"FGF_p"] / 16.8 * 32, 1),
                    ymin = 0.5 * min(out[,"FGF_p"] / 16.8 * 32),
                    ymax = 1.5 * max(out[,"FGF_p"] / 16.8 * 32),
                    name = "FGF23p (pg/mL)",
                    line = list(color = 'black', width = 2),
                    visible = FALSE) %>%
          layout(
            title = "Plasma concentrations",
            font = title_size,
            xaxis = xvar,
            updatemenus = list(
              list(
                #type = "buttons",
                direction = "right",
                #xanchor = 'left',
                yanchor = "bottom",
                x = 0,
                y = -0.45,
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
                       label = "FGFp")
                )
              )
            )
          ) %>%
          config(displayModeBar = FALSE)

      } else if (node == 3) {

        # rapid bone compartment
        p <- plot_ly(out,
                     x = time,
                     mode = "lines") %>%
          add_lines(y = out[,"Ca_f"],
                    ymin = 0.5 * min(out[,"Ca_f"]),
                    ymax = 1.5 * max(out[,"Ca_f"]),
                    name = "Caf (mmol)",
                    line = list(color = 'rgb(27, 102, 244)', width = 2),
                    visible = TRUE) %>%
          add_lines(y = out[,"PO4_f"],
                    ymin = 0.5 * min(out[,"PO4_f"]),
                    ymax = 1.5 * max(out[,"PO4_f"]),
                    name = "PO4f (mmol)",
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
                y = -0.45,
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

      } else if (node == 4) {

        # deep bone compartment
        p <- plot_ly(out,
                     x = time,
                     mode = "lines") %>%
          add_lines(y = out[,"Ca_b"],
                    ymin = 0.5 * min(out[,"Ca_b"]),
                    ymax = 1.5 * max(out[,"Ca_b"]),
                    name = "Cab (mmol)",
                    line = list(color = 'rgb(27, 102, 244)', width = 2),
                    visible = TRUE) %>%
          add_lines(y = out[,"PO4_b"],
                    ymin = 0.5 * min(out[,"PO4_b"]),
                    ymax = 1.5 * max(out[,"PO4_b"]),
                    name = "PO4b (mmol)",
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
                y = -0.45,
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
        node_Ca_list <- data.frame(id = c(rep(0,7),11,rep(0,2),2),
                                   names = c(rep("",7),"PO4 quantity in cells",
                                             rep("",2),"PTH quantity in parathyroid glands"),
                                   units = c(rep("",7),"mmol",
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

  # avoid edges that are not fluxes in the network 13:29
  # as well as filtration process
  if (edge == 7 |
      # sum counts the number of true, only one is enough
      sum(edge ==  13:29) == 1) {
    p <- plot_ly() %>%
      add_annotations("Please select another edge!",
                      showarrow = FALSE,
                      font = list(color = "red", size = 10)) %>%
      config(displayModeBar = FALSE)

  } else {

    # select edges where Ca and PO4 fluxes
    # have the same regulation
    if (edge == "Abs_int" | edge == "Res") {

      yvar <- list(title = "Flux (µmol/min)",
                   range = c(min(out[,paste0(edge,"_Ca")]*1000*0.8,
                                 out[,paste0(edge,"_PO4")]*1000*0.8),
                             max(out[,paste0(edge,"_Ca")]*1000*1.2,
                                 out[,paste0(edge,"_PO4")]*1000*1.2)))

      p <- plot_ly(out,
                   x = time,
                   mode = "lines") %>%
        add_lines(y = out[,paste0(edge,"_Ca")]*1000,
                  name = paste0("Ca ",edge),
                  line = list(color = 'rgb(27, 102, 244)', width = 2),
                  visible = TRUE) %>%
        add_lines(y = out[,paste0(edge,"_PO4")]*1000,
                  name = paste0("PO4 ",edge),
                  line = list(color = 'rgb(244, 27, 27)', width = 2),
                  visible = FALSE) %>%
        layout(
          title = paste(edge),
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
              y = -0.45,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE)),
                     label = "Ca"),

                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE)),
                     label = "PO4"),

                list(method = "restyle",
                     args = list("visible", list(TRUE, TRUE, FALSE)),
                     label = "Both"))))) %>%
        config(displayModeBar = FALSE)

    } else if (edge == "Net_Ca_pf" | edge == "Net_PO4_pf") {

      # extract the pattern Ca or PO4 from the edge name
      elem <- unlist(strsplit(edge,"_"))[[2]]

      # Ca and PO4 exchanges between plasma and rapid pool
      yvar <- list(title = "Flux (µmol/min)",
                   range = c(min(out[,paste0(elem,"_pf")]*1000*0.8),
                             max(out[,paste0(elem,"_pf")]*1000*1.2)))

      p <- plot_ly(out,
                   x = time,
                   mode = "lines") %>%
        add_lines(y = out[,paste0(elem,"_pf")]*1000,
                  name = paste(elem, "flux between plasma and fast bone pool"),
                  line = list(color = 'rgb(27, 102, 244)', width = 2),
                  visible = TRUE) %>%
        add_lines(y = out[,paste0(elem,"_fp")]*1000,
                  name = paste(elem, "flux between fast bone pool and plasma"),
                  line = list(color = 'rgb(244, 27, 27)', width = 2),
                  visible = FALSE) %>%
        layout(
          title = paste("Plasma/fast bone pool",elem,"exchanges"),
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
              y = -0.45,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE)),
                     label = "Plasma -> bone"),

                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE)),
                     label = "Bone -> plasma"),

                list(method = "restyle",
                     args = list("visible", list(TRUE, TRUE, FALSE)),
                     label = "Both"))))) %>%
        config(displayModeBar = FALSE)

    } else if (edge == "Net_PO4_cells") {

      # PO4 exchanges between cells and plasma
      yvar <- list(title = "Flux (µmol/min)",
                   range = c(min(out[,"PO4_pc"]*1000*0.8,
                                 out[,"PO4_cp"]*1000*0.8),
                             max(out[,"PO4_cp"]*1000*1.2,
                                 out[,"PO4_pc"]*1000*1.2)))

      p <- plot_ly(out,
                   x = time,
                   mode = "lines") %>%
        add_lines(y = out[,"PO4_pc"]*1000,
                  name = "PO4 flux into cell",
                  line = list(color = 'rgb(27, 102, 244)', width = 2),
                  visible = TRUE) %>%
        add_lines(y = out[,"PO4_cp"]*1000,
                  name = "PO4 release from cells",
                  line = list(color = 'rgb(244, 27, 27)', width = 2),
                  visible = FALSE) %>%
        layout(
          title = "Plasma/Cells PO4 exchanges",
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
              y = -0.45,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE, FALSE)),
                     label = "Plasma -> Cells"),

                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE, FALSE)),
                     label = "Cells -> Plasma"),

                list(method = "restyle",
                     args = list("visible", list(TRUE, TRUE, FALSE)),
                     label = "Both"))))) %>%
        config(displayModeBar = FALSE)

    } else {

      # other cases
      yvar <- list(title = "Flux (µmol/min)",
                   range = c(min(out[,edge]*1000*0.8),
                             max(out[,edge]*1000*1.2)))

      p <- plot_ly(out,
                   x = time,
                   y = out[,edge]*1000,
                   type = "scatter",
                   mode = "lines",
                   line = list(color = 'black', width = 2)) %>%
        layout(title = paste(edge),
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
  button_states$values <- append(button_states$values, input$reset_t_now)

  # associate each reset button to its related slider
  reset_vector <- "t_now"

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

}


# Function that determines which parameter is changed or not
# Takes parameters values as argument and returns a list
# of all parameters that are currently different from their
# base case value
find_parameter_change <- function(parms) {
  param_base_case <- c(4.192, 5.9e-002, 5.8e-002, 2.5e-005, 1e-003, 6.902e-011,
                       2.2e-003, 5.5e-004, 2.75e-004, 1e-004, 6e-004, 0.44,
                       2.34e-003, 1.55e-003, 0.1875, 1e-003, 13.5,
                       0.25165, 0.3, 3, 142, 0.6, 0.01, 2e-003)
  # determines which param val is different of 1 (normalized value)
  id <- which(parms/param_base_case != 1)
  if (is_empty(id)) {
    NULL
  } else {
    param_name <- names(parms[id])
    parms <- unname(parms)
    param_val <- parms[id]
    param_ratio <- parms[id]/param_base_case[id]
    param_col <- ifelse(param_ratio > 1, "success", "danger")
    param_variation <- ifelse(param_ratio > 1, "increased", "decreased")

    list("value" = param_val, "color" = param_col,
                "text" = paste(param_name, "is", param_variation, "by",
                               param_ratio, sep = " "))
  }
}



# Recover to_start and t_stop time when reading the event_table
# Takes event table as argument and returns a vector of the
# corresponding event parameters (t_start, t_stop and the rate of injection/gavage)
generate_event_parms <- function(event_table) {
  #take event table row by row
  if (nrow(event_table) > 0) {
    name <- event_table[1, "event"]
    rate <- event_table[1, "rate"]
    t_start <- event_table[1, "start_time"]
    t_stop <- event_table[1, "stop_time"]

    Ca_inject <- if (name == "Ca_inject") rate else 0
    Ca_food <- if (name == "Ca_food") rate else 0
    D3_inject <- if (name == "D3_inject") rate else 0
    P_inject <- if (name == "P_inject") rate else 0
    P_food <- if (name == "P_food") rate else 0
    D3_intake_reduction <- if (name == "D3_intake_reduction") rate else 1
    Bispho <- if (name == "bisphosphonate") 0.3 else 1
    Furo <- if (name == "furosemide") 6 else 1
    Cinacal <- if (name == "cinacalcet") 1 else 0

    return(
      c(
        "t_start" = t_start,
        "t_stop" = t_stop,
        "Ca_inject" = Ca_inject,
        "Ca_food" = Ca_food,
        "D3_inject" = D3_inject,
        "P_inject" = P_inject,
        "P_food" = P_food,
        "D3_intake_reduction" = D3_intake_reduction,
        "Bispho" = Bispho,
        "Furo" = Furo,
        "Cinacal" = Cinacal
      )
    )
  } else {
    return(
      c(
        "t_start" = 0,
        "t_stop" = 0,
        "Ca_inject" = 0,
        "Ca_food" = 0,
        "D3_inject" = 0,
        "P_inject" = 0,
        "P_food" = 0,
        "D3_intake_reduction" = 1,
        "Bispho" = 1,
        "Furo" = 1,
        "Cinacal" = 0
      )
    )
  }
}

# Function needed to produce cumulative plots
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


# custom bootstrap 4 panels
bs4TabSetPanel <- function(..., id, side, status = NULL, tabStatus = NULL, .list = NULL) {

  # to make tab ids in the namespace of the tabSetPanel
  ns <- shiny::NS(id)

  tabs <- c(list(...), .list)
  found_active <- FALSE
  selected <- NULL
  tabStatus <- if (!is.null(tabStatus)) rep(tabStatus, length.out = length(tabs))
  # handle tabs
  tabSetPanelItem <- lapply(seq_along(tabs), FUN = function(i) {

    tabName <- tabs[[i]][[1]]
    tabsTag <- tabs[[i]][[2]]

    tabClass <- tabsTag$attribs$class

    # make sure that if the user set 2 tabs active at the same time,
    # only the first one is selected
    active <- sum(grep(x = tabClass, pattern = "active")) == 1
    if (!found_active) {
      if (active) {
        found_active <<- TRUE
        selected <<- i - 1
        # if no items are selected, we select the first
      } else {
        selected <<- 0
      }
      # do not allow more than 1 active item
    } else {
      if (active) {
        stop("Cannot set 2 active tabs at the same time.")
      }
    }

    id <- tabsTag$attribs$id

    shiny::tags$li(
      class = if (!is.null(status) & is.null(tabStatus[i])) {
        "nav-item bg-light"
      } else if (!is.null(tabStatus[i])) {
        paste0("nav-item bg-", tabStatus[i])
      } else {
        "nav-item"
      },
      shiny::tags$a(
        class = if (active) "nav-link active show" else "nav-link",
        href = paste0("#", ns(id)),
        `data-toggle` = "tab",
        tabName
      )
    )
  })

  tabSetMenu <- shiny::tags$ul(
    id = id,
    class = if (side == "right") {
      "nav nav-pills ml-auto p-2"
    } else {
      "nav nav-pills p-2"
    }
  )
  tabSetMenu <- shiny::tagAppendChildren(tabSetMenu, tabSetPanelItem)

  # content
  tabSetContent <- shiny::tags$div(
    class = "tab-content",
    lapply(seq_along(tabs), FUN = function(i) {

      # put the correct namespace on ids
      tabs[[i]][[2]]$attribs$id <- ns(tabs[[i]][[2]]$attribs$id)
      tabs[[i]][[2]]
    })
  )

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(
          paste0(
            "$(function () {
              $('#", id," li:eq(", selected,") a').tab('show');
            })
            "
          )
        )
      )
    ),
    tabSetMenu, tabSetContent
  )

}
