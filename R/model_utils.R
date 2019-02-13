#' @title Notifications Generator for CaPO4 animations
#'
#' @description Generate sequential notification as a function of the
#' selected diseases. All notifications are in the notifications.R file in the
#' inst/entry_level app folder. Used in the \link{infos} module.
#'
#' @param simulation Which disease is currently selected. See \link{extract_running_sim}.
#' @param counter To determine which notification to display. We expect a counter
#' returned by the \link{networkCaPO4} module.
#' @param allowed Whether to allow simulations. Expect logical value. See \link{infos} module.
#'
#' @export
generate_notification <- function(simulation, counter, allowed) {
  idx <- counter
  # print only if notifications are allowed
  if (allowed == TRUE) {
    if (counter > 0) {
      showNotification(
        id = "notifid",
        # need to eval and deparse so as to paste message
        # need also to use HTML to handle html tags in the text
        # such as <b> >/b>, ...
        HTML(eval(parse(text = paste0("notification_list$", simulation, "[idx+1]")))),
        type = "message",
        duration = 9999
      )
    } else {
      removeNotification(id = "notifid")
    }

    # toastr is interesting but need to be improved!
    # toastr_info(
    #   message = eval(parse(text = paste("notification_list$", simulation, "[idx+1]", sep = ""))),
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



# dropNulls
dropNulls <- function (x) x[!vapply(x, is.null, FUN.VALUE = logical(1))]


#' @title Extract the current running simulation
#'
#' @description Simulations are currently php1, hypoD3 and hypopara. Takes
#' diseases as input given by the \link{diseaseSelect} module.
#'
#' @param diseases Shiny input disease selector. See \link{diseaseSelect}.
#'
#' @export
extract_running_sim <- function(diseases) {
  sim <- unlist(
    lapply(seq_along(diseases), FUN = function(i) {
      if (diseases[[i]]()) names(diseases)[[i]]
    })
  )
  sim <- dropNulls(sim)
  return(sim)
}




#' @title Highlight arrows for steady state events
#'
#' @description Use inside in the \link{networkCaPO4}. Nothing is returned
#' except that the network is updated via \link[visNetwork]{visNetworkProxy}.
#'
#' @param edges A dataframe of edges provided by \link{generate_edges}.
#' @param simulation Which disease is currently selected. See \link{extract_running_sim}.
#' @param counter To determine which notification to display. We expect a counter
#' returned by the \link{networkCaPO4} module.
#' @param session Session object.
#'
#' @export
arrow_lighting <- function(edges, simulation, counter, session) {

  ns <- session$ns

  # store the current animation
  current_anim <- eval(parse(text = paste0("animation_", simulation)))

  # if the counter is 1 or higher
  if (counter > 0) {
    # selected arrows
    sel <- unlist(current_anim[[counter]])
    #set new edge properties
    if (counter != 6) edges$color.color[sel] <- "yellow" # perturbation

    # edge size might depend on the event
    if (simulation == "php1") {
      if (sum(is.element(c(1:4, 6), counter)) == 1) {
        edges$width[sel] <- 8
        # make these edges blink
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy(ns("network_CaPO4")) %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy(ns("network_CaPO4")) %>%
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
            visNetworkProxy(ns("network_CaPO4")) %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy(ns("network_CaPO4")) %>%
              visUpdateEdges(edges = edges)
          }
          Sys.sleep(0.5)
        })
      }
      if (counter == 6) {
        edges$color.color[sel] <- c(rep("red", 4), rep("green", 7))
      }
    } else if (simulation == "hypopara") {
      if (sum(is.element(c(1:4, 6), counter)) == 1) {
        edges$width[sel] <- 3
        # make these edges blink
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy(ns("network_CaPO4")) %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy(ns("network_CaPO4")) %>%
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
            visNetworkProxy(ns("network_CaPO4")) %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy(ns("network_CaPO4")) %>%
              visUpdateEdges(edges = edges)
          }
          Sys.sleep(0.5)
        })
      }
      if (counter == 6) {
        edges$color.color[sel] <- c(rep("green", 4), rep("red", 7))
      }
    } else if (simulation == "hypoD3") {
      if (sum(is.element(c(1, 3, 4, 5), counter)) == 1) {
        edges$width[sel] <- 3
        # make these edges blink
        lapply(1:2, FUN = function(i){
          if ((i %% 2) != 0) {
            edges$hidden[sel] <- TRUE
            visNetworkProxy(ns("network_CaPO4")) %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy(ns("network_CaPO4")) %>%
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
            visNetworkProxy(ns("network_CaPO4")) %>%
              visUpdateEdges(edges = edges)
          } else {
            edges$hidden[sel] <- FALSE
            visNetworkProxy(ns("network_CaPO4")) %>%
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
  visNetworkProxy(ns("network_CaPO4"), session) %>%
    visUpdateEdges(edges = edges)
}



#' @title Generate user fields
#'
#' @description Use inside in the \link{userInfo}. Function that helps in generating 4 users fields,
#' image, stat1, stat2 and stat3, so as to reinject them in the header userMenu
#'
#' @param diseases Shiny input disease selector. See \link{diseaseSelect}.
#' @param sliderDisease Shiny slider input related to the current disease severity.
#' See \link{plotBox}.
#'
#' @export
generate_userFields <- function(diseases, sliderDisease) {
  if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) {
    if (diseases$php1()) {
      req(sliderDisease())
      if (sliderDisease() == 20) {
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
      } else if (sliderDisease() == 100) {
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
    } else if (diseases$hypopara()) {
      req(sliderDisease())
      if (sliderDisease() == 0.5) {
        stat1 <- HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(paste(withMathJax(p("$$[P_i]_p$$ 3 mM")), "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(paste(withMathJax(p("$$[PTH]_p$$ 63 ng/l")), "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/happy.png"
        state <- "sick"
      } else if (sliderDisease() == 0.1) {
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
      req(diseases$hypoD3())
      if (sliderDisease() == 0.5) {
        stat1 <- HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
        stat2 <- HTML(paste(withMathJax(p("$$[P_i]_p$$ 3 mM")), "<br/>", "(2.2-3.5 mM)"))
        stat3 <- HTML(
          paste(withMathJax(p("$$[PTH]_p$$")), "<font color=\"#008000\"><b>",
                "106 ng/l", "</b></font>", "<br/>", "(20-70 ng/l)"))
        image <- "images_patient_info/sad.png"
        state <- "sick"
      } else if (sliderDisease() == 0.1) {
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
