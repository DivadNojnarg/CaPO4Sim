# Notification function. Takes counter_nav diagram as well
# as the simulation event as arguments and also the switch
# to allow notifications or not.
generate_notification <- function(simulation, counter, allowed) {
  idx <- counter
  # only take the part after the "_"
  sim <- str_split(simulation, pattern = "_")[[1]][2]
  # print only if notifications are allowed
  if (allowed == TRUE) {
    if (counter > 0) {
      showNotification(
        id = "notifid",
        # need to eval and deparse so as to paste message
        # need also to use HTML to handle html tags in the text
        # such as <b> >/b>, ...
        HTML(eval(parse(text = paste0("notification_list$", sim, "[idx+1]")))),
        type = "message",
        duration = 9999
      )
    } else {
      removeNotification(id = "notifid")
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
  current_animation <- paste0("animation", str_extract(current_sim, "_\\w+"))
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
      if (sum(is.element(c(1:4, 6), counter)) == 1) {
        edges$width[sel] <- 8
        # make these edges blink
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          }
          Sys.sleep(0.5)
        })
        # make these edges blink
      } else {
        edges$width[sel] <- 3
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          }
          Sys.sleep(0.5)
        })
      }
      if (counter == 6) {
        edges$color.color[sel] <- c(rep("red", 4), rep("green", 7))
      }
    } else if (simulation == "run_hypopara") {
      if (sum(is.element(c(1:4, 6), counter)) == 1) {
        edges$width[sel] <- 3
        # make these edges blink
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          }
          Sys.sleep(0.5)
        })
        # make these edges blink
      } else {
        edges$width[sel] <- 8
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          }
          Sys.sleep(0.5)
        })
      }
      if (counter == 6) {
        edges$color.color[sel] <- c(rep("green", 4), rep("red", 7))
      }
    } else if (simulation == "run_hypoD3") {
      if (sum(is.element(c(1, 3, 4, 5), counter)) == 1) {
        edges$width[sel] <- 3
        # make these edges blink
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          }
          Sys.sleep(0.5)
        })
        # make these edges blink
      } else {
        edges$width[sel] <- 8
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy("network_Ca") %>%
              visUpdateEdges(edges = edges)
          }
          Sys.sleep(0.5)
        })
      }
      if (counter == 6) {
        edges$color.color[sel] <- "red"
      }
    }
  } else {
    # no selection when the counter equals 0
    sel <- NULL
  }

  # update the network
  visNetworkProxy("network_CaPO4", session) %>%
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

  if (is_empty(current_sim)) {
    slider_value <- eval(parse(text = "input$slider_help"))
  } else {
    slider_name <- paste("slider_", current_sim, sep = "")
    slider_value <- eval(parse(text = paste0("input$", slider_name)))
  }
  return(slider_value)
}



# Function that helps in generating 4 users fields,
# image, stat1, stat2 and stat3, so as to reinject
# them in the header userMenu. Takes input as arguments.
# Returns a list.
generate_userFields <- function() {
  if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
    if (input$run_php1) {
      req(input$slider_php1)
      if (input$slider_php1 == 20) {
        stat1 <- HTML(
          paste(withMathJax(p("$$[Ca^{2+}]_p$$")), "<font color=\"#008000\"><b>",
                "1.5 mM", "</b></font>", "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(
          paste(withMathJax(p("$$[P_i]_p$$")), "<font color=\"FF0000\"><b>",
                "2.3 mM", "</b></font>", "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(
          paste(withMathJax(p("$$[PTH]_p$$")), "<font color=\"#008000\"><b>",
                "107 ng/l", "</b></font>", "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/sad.png"
        state <- "sick"
      } else if (input$slider_php1 == 100) {
        stat1 <- HTML(
          paste(withMathJax(p("$$[Ca^{2+}]_p$$")), "<font color=\"#008000\"><b>",
                "2 mM", "</b></font>", "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(
          paste(withMathJax(p("$$[P_i]_p$$")), "<font color=\"FF0000\"><b>",
                "2 mM", "</b></font>", "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(
          paste(withMathJax(p("$$[PTH]_p$$")), "<font color=\"#008000\"><b>",
                "214 ng/l", "</b></font>", "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/suffer.png"
        state <- "sick"
      } else {
        stat1 <- HTML(
          paste(withMathJax(p("$$[Ca^{2+}]_p$$")), "<font color=\"#008000\"><b>",
                "2 mM", "</b></font>", "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(
          paste(withMathJax(p("$$[P_i]_p$$")), "<font color=\"FF0000\"><b>",
                "1.9 mM", "</b></font>", "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(
          paste(withMathJax(p("$$[PTH]_p$$")), "<font color=\"#008000\"><b>",
                "303 ng/l", "</b></font>", "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/dead.png"
        state <- "dead"
      }
    } else if (input$run_hypopara) {
      req(input$slider_hypopara)
      if (input$slider_hypopara == 0.5) {
        stat1 <- HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(paste(withMathJax(p("$$[P_i]_p$$ 3 mM")), "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(paste(withMathJax(p("$$[PTH]_p$$ 63 ng/l")), "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/happy.png"
        state <- "sick"
      } else if (input$slider_hypopara == 0.1) {
        stat1 <- HTML(
          paste(withMathJax(p("$$[Ca^{2+}]_p$$")), "<font color=\"#FF0000\"><b>",
                "1 mM", "</b></font>", "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(
          paste(withMathJax(p("$$[P_i]_p$$")), "<font color=\"#008000\"><b>",
                "3.6 mM", "</b></font>", "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(paste(withMathJax(p("$$[PTH]_p$$ 41 ng/l")), "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/sad.png"
        state <- "sick"
      } else {
        stat1 <- HTML(
          paste(withMathJax(p("$$[Ca^{2+}]_p$$")), "<font color=\"#FF0000\"><b>",
                "0.6 mM", "</b></font>", "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(
          paste(withMathJax(p("$$[P_i]_p$$")), "<font color=\"#008000\"><b>",
                "3.8 mM", "</b></font>", "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(
          paste(withMathJax(p("$$[PTH]_p$$")), "<font color=\"#FF0000\"><b>",
                "0 ng/l", "</b></font>", "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/suffer.png"
        state <- "sick"
      }
    } else {
      req(input$slider_hypoD3)
      if (input$slider_hypoD3 == 0.5) {
        stat1 <- HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(paste(withMathJax(p("$$[P_i]_p$$ 3 mM")), "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(
          paste(withMathJax(p("$$[PTH]_p$$")), "<font color=\"#008000\"><b>",
                "106 ng/l", "</b></font>", "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/sad.png"
        state <- "sick"
      } else if (input$slider_hypoD3 == 0.1) {
        stat1 <- HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(paste(withMathJax(p("$$[P_i]_p$$ 3 mM")), "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(
          paste(withMathJax(p("$$[PTH]_p$$")), "<font color=\"#008000\"><b>",
                "180 ng/l", "</b></font>", "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/sad.png"
        state <- "sick"
      } else {
        stat1 <- HTML(
          paste(withMathJax(p("$$[Ca^{2+}]_p$$")), "<font color=\"#FF0000\"><b>",
                "0.8 mM", "</b></font>", "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(paste(withMathJax(p("$$[P_i]_p$$ 2.5 mM")), "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(
          paste(withMathJax(p("$$[PTH]_p$$")), "<font color=\"#008000\"><b>",
                "231 ng/l", "</b></font>", "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/sad.png"
        state <- "sick"
      }
    }
  }
  return(list(image = image, stat1 = stat1, stat2 = stat2, stat3 = stat3, description = state))
}