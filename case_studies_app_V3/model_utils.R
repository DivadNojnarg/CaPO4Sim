# Notification function. Takes counter_nav diagram as well 
# as the simulation event as arguments and also the switch 
# to allow notifications or not.
generate_notification <- function(simulation, counter, allowed) {
  idx <- counter
  # only take the part after the "_"
  sim <- str_split(simulation, pattern = "_")[[1]][2]
  # print only if notifications are allowed
  if (allowed == TRUE) {
    showNotification(
      id = "notifid",
      # need to eval and deparse so as to paste message
      # need also to use HTML to handle html tags in the text
      # such as <b> >/b>, ...
      HTML(eval(parse(text = paste("notification_list$", sim, "[idx+1]", sep = "")))),
      type = "message",
      duration = 9999
    )
    
    notif <- eval(parse(text = paste("graph_notification_list$", sim, "[idx+1]", sep = "")))
    # this part ensures that the graph notif is only displayed
    # a the first iteration of the animation. 
    if (!is.na(notif)) {
      showNotification(
        id = "graph_notif",
        # need to eval and deparse so as to paste message
        withMathJax(HTML(notif)),
        type = "warning",
        duration = 9999
      )
    } else {
      removeNotification(id = "graph_notif")
    }
    
    # toastr is interesting but need to be improved!
    # toastr_info(
    #   message = eval(parse(text = paste("notification_list$", sim, "[idx+1]", sep = ""))),
    #   title = "",
    #   closeButton = TRUE,
    #   preventDuplicates = TRUE,
    #   position = "top-full-width",
    #   timeOut = 0,
    #   showEasing = "swing",
    #   showMethod = "fadeIn",
    #   hideMethod = "fadeOut",
    #   progressBar = TRUE,
    #   newestOnTop = TRUE
    # )
    
  } else {
    removeNotification(id = "notifid")
    removeNotification(id = "graph_notif")
    #toastr_clear_all(with_animation = TRUE)
  }
}



# Extract the current runing simulation among php1, hypoD3, hypopara
# Ca_inject, PO4_inject and PO4 gavage. Takes input as argument
# returns the names of the current simulation (string).
extract_running_sim <- function(input) {
  # extract all simulations
  sim <- str_extract(names(input), pattern = "^run_\\w+")
  # remove NAs
  sim <- sim[!is.na(sim)]
  # converting each string to the corresponding object
  sim_obj <- lapply(sim, function(x) {eval(parse(text = paste("input$", x)))})
  # which simulation is set to true? (php1, hypopara, ...)
  sim_idx <- which(sim_obj == TRUE)
  current_simulation <- sim[sim_idx]
  return(list(current_simulation, sim))
}



# Lighting events for php1, hypopara and hypoD3
# As fluxes are not calculated by the model
# we have to make something different of what
# was done in the global app.


# extract the proper table of animations
extract_animation <- function(input) {
  # returns php1, hypopara, hypoD3
  current_sim <- extract_running_sim(input)[[1]]
  current_animation <- paste("animation", str_extract(current_sim, 
                                                      "_\\w+"), sep = "")
  return(current_animation)
}

# highlight arrows for steady state events.
# Takes edges id, current simulation, the counter value 
# (which represents the current step f-of the animation), 
# some input values as well as session. Nothing is returned
# except that the network is updated. This function calls
# extract_animation() to get the current animation
arrow_lighting <- function(edges, simulation, counter, input, session) {
  # store the current animation
  current_anim <- eval(parse(text = extract_animation(input)))
  
  # if the counter is 1 or higher
  if (counter > 0) {
    # selected arrows
    sel <- unlist(current_anim[[counter]])
    #set new edge properties
    if (counter != 6) edges$color.color[sel] <- "yellow" # perturbation
    
    # edge size might depend on the event
    if (simulation == "run_php1") {
      ifelse(sum(is.element(c(1:4, 6), counter)) == 1,
             edges$width[sel] <- 8,
             edges$width[sel] <- 3)
      if (counter == 6) {
        edges$color.color[sel] <- c(rep("red", 4), rep("green", 7))
      }
    } else if (simulation == "run_hypopara") {
      ifelse(sum(is.element(c(1:4), counter)) == 1,
             edges$width[sel] <- 3,
             edges$width[sel] <- 8)
      if (counter == 6) {
        edges$color.color[sel] <- c(rep("green", 4), rep("red", 7))
      }
    } else if (simulation == "run_hypoD3") {
      ifelse(sum(is.element(c(1, 3, 4, 5), counter)) == 1,
             edges$width[sel] <- 3,
             edges$width[sel] <- 8)
      if (counter == 6) {
        edges$color.color[sel] <- "red"
      }
    }
    
  } else {
    # no selection when the counter equals 0
    sel <- NULL
  }
  
  # update the network
  visNetworkProxy("network_Ca", session) %>%
    visUpdateEdges(edges = edges)
}



# highlitght arrows for dynamic events
# take out (result of integration by ode solver),
# edges and session as arguments. Nothing special
# is returned, except that the network is updated
arrow_lighting_live <- function(out, edges, session) {
  calc_change_t <- calc_change(out)
  calc_change_t$X <- NULL # remove column X
  
  # calculate the difference between live fluxes and base-case values
  # index of arrows in the graph (which are fluxes and not regulations)
  index <- c(1,10,11,6,4,5,8,9,2,3,12) 
  calc_change_t <- rbind(calc_change_t, index)
  
  # calculate which element in the sum table is different of 0 and store the index
  flux_changed_index <- which(calc_change_t[1,] != 0) 
  # convert to arrow index in the interactive diagramm
  arrow_index <- as.numeric(t(calc_change_t[2,flux_changed_index])) 
  
  if (!is.null(flux_changed_index)) {
    for (i in (1:ncol(calc_change_t))) {
      # change edge color according to an increase or decrease of the flux
      arrow_index_i <- arrow_index[i] 
      ifelse(calc_change_t[[i]][1] > 0,
             edges$color.color[arrow_index_i] <- "green",
             edges$color.color[arrow_index_i] <- "red")
    }
    
  }
  
  # need to add elements related to PO4 and Ca events
  # if (input$run_PO4_gav) { # PO4 gavage
  # 
  #   edges_Ca$color.color[1] <- "yellow" # perturbation
  #   edges_Ca$width[1] <- 8
  # 
  # }
  # if (input$run_Ca_inject) {
  #   if (input$tmax < 60) { # Ca infusion
  # 
  #     edges_Ca$color.color[c(20,24,25,33)] <- "yellow" # perturbation
  #     edges_Ca$width[c(20,24,25,33)] <- 8
  # 
  #   } else {# EGTA infusion
  # 
  #     edges_Ca$color.color[c(20,24,25,35)] <- "yellow" # perturbation
  #     edges_Ca$width[c(20,24,25)] <- 2
  #     edges_Ca$width[35] <- 8
  # 
  #   }
  # }
  # if (input$run_PO4_inject) { # PO4 injection
  #   edges_Ca$color.color[c(26,27,28)] <- "yellow" # perturbation
  #   edges_Ca$width[c(26,27,28)] <- 8
  # 
  #   if (input$tmaxbis <= 3) {
  # 
  #     edges_Ca$color.color[34] <- "yellow" # perturbation
  #     edges_Ca$width[34] <- 8
  # 
  #   }
  # 
  # }
  
  visNetworkProxy("network_Ca") %>%
    visUpdateEdges(edges = edges)
}



# function needed to generate the slider for php1, 
# hypopara and hypoD3. Takes input as argument and
# returns the slider_value as output. It is used by
# all_plots.R script to draw the vertical orange line
# during php1, hypopara and hypoD3
generate_slidersteady <- function(input){
  current_sim <- extract_running_sim(input)[[1]] %>%
    str_extract("_\\w+") %>%
    str_replace("_", "")
  
  slider_name <- paste("slider_", current_sim, sep = "")
  slider_value <- eval(parse(text = paste("input$", slider_name, sep = "")))
  return(slider_value)
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
                                 list(c(input$reset_tmaxCainj[1],
                                        input$reset_tmaxPO4inj[1],
                                        input$reset_tmaxPO4gav[1])))         
  
  # associate each reset button to its related slider
  reset_vector <- c("tmaxCainj",
                    "tmaxPO4inj",
                    "tmaxPO4gav")
  
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


# Function that helps in generating 4 users fields,
# image, stat1, stat2 and stat3, so as to reinject
# them in the header userMenu. Takes input as arguments.
# Returns a list.
generate_userFields <- function(input) {
  if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
    if (input$run_php1) {
      req(input$slider_php1)
      if (input$slider_php1 == 20) {
        stat1 <- withMathJax(p("$$[Ca^{2+}]_p$$ = 1.5 mM"))
        stat2 <- withMathJax(p("$$[P_i]_p$$ = 1.2 mM"))
        image <- "sad.png"
        state <- "sick"
      } else if (input$slider_php1 == 100) {
        stat1 <- withMathJax(p("$$[Ca^{2+}]_p$$ = 2 mM"))
        stat2 <- withMathJax(p("$$[P_i]_p$$ = 1 mM"))
        image <- "suffer.png"
        state <- "sick"
      } else {
        stat1 <- withMathJax(p("$$[Ca^{2+}]_p$$ = 2 mM"))
        stat2 <- withMathJax(p("$$[P_i]_p$$ = 1 mM"))
        image <- "dead.png"
        state <- "dead"
      }
    } else if (input$run_hypopara) {
      req(input$slider_hypopara)
      if (input$slider_hypopara == 0.5) {
        stat1 <- withMathJax(p("$$[Ca^{2+}]_p$$ = 1.2 mM"))
        stat2 <- withMathJax(p("$$[P_i]_p$$ = 1.5 mM"))
        image <- "happy.png"
        state <- "sick"
      } else if (input$slider_hypopara == 0.1) {
        stat1 <- withMathJax(p("$$[Ca^{2+}]_p$$ = 1 mM"))
        stat2 <- withMathJax(p("$$[P_i]_p$$ = 1.8 mM"))
        image <- "sad.png"
        state <- "sick"
      } else {
        stat1 <- withMathJax(p("$$[Ca^{2+}]_p$$ = 0.6 mM"))
        stat2 <- withMathJax(p("$$[P_i]_p$$ = 1.9 mM"))
        image <- "suffer.png"
        state <- "sick"
      }
    } else {
      req(input$slider_hypoD3)
      if (input$slider_hypoD3 %in% c(0.1, 0.5)) {
        stat1 <- withMathJax(p("$$[Ca^{2+}]_p$$ = 1.2 mM"))
        stat2 <- withMathJax(p("$$[P_i]_p$$ = 1.5 mM"))
        image <- "happy.png"
        state <- "sick"
      } else {
        stat1 <- withMathJax(p("$$[Ca^{2+}]_p$$ = 0.8 mM"))
        stat2 <- withMathJax(p("$$[P_i]_p$$ = 1.3 mM"))
        image <- "bad.png"
        state <- "sick"
      }
    }
  }
  return(list(image = image, stat1 = stat1, 
              stat2 = stat2, description = state))
}