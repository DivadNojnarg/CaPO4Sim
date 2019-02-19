#' @title CaPO4 Network UI module
#'
#' @description Create a CaPO4 network
#'
#' @param id module id.
#'
#' @export
networkCaPO4Ui <- function(id) {

  ns <- NS(id)


  boxTag <- box(
    width = 12,
    solidHeader = TRUE,
    # back button for case studies
    uiOutput(ns("back_button")),
    # slider input for dynamic case studies
    uiOutput(ns("counter_progress")),
    # next button for case studies
    uiOutput(ns("next_button")),
    br(),
    # Main network
    introBox(
      div(
        id = "network_cap", # to insert a background image if needed
        withSpinner(
          visNetworkOutput(
            outputId = ns("network_CaPO4"),
            height = "900px"),
          size = 2,
          type = 8,
          color = "#000000"
        )
      ),
      data.step = 2,
      data.intro = help_text[2],
      data.position = "right"
    )
  )

  boxTag$children[[1]] <- tagAppendAttributes(boxTag$children[[1]], id = "boxNetwork")

  column(
    width = 6,
    offset = 0,
    style = 'padding:0px;',
    boxTag
  )
}




#' @title CaPO4 Network server module
#'
#' @description Create a CaPO4 network
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#' @param isMobile Shiny input checking if the app is running on a cellphone/tablet.
#' @param components Shiny input CaPO4 component selector. See \link{networkOptions}.
#' @param organs Shiny input to toggle organs display. See \link{networkOptions}.
#' @param regulations Shiny input to toggle hormone display. See \link{networkOptions}.
#' @param background Shiny input background selector. See \link{networkOptions}.
#' @param diseases Shiny input disease selector. See \link{diseaseSelect}.
#' @param organs_nodes_size Shiny input for organs node size. See \link{networkOptions}.
#' @param hormones_nodes_size Shiny input for hormones node size. See \link{networkOptions}.
#' @param organs_edges_size Shiny input for organs edges size. See \link{networkOptions}.
#' @param hormones_edges_size Shiny input for hormones edges size. See \link{networkOptions}.
#' @param help Help input.
#'
#' @export
networkCaPO4 <- function(input, output, session, isMobile, components,
                         organs, regulations, background, diseases,
                         organs_nodes_size, hormones_nodes_size,
                         organs_edges_size, hormones_edges_size, help) {

  ns <- session$ns

  observe({
    #print(regulations())
    #print(background())
    #print(components())
    #print(organs_nodes_size())
    #print(organs_edges_size())
  })

  #-------------------------------------------------------------------------
  #  Generate the patient overview network
  #-------------------------------------------------------------------------

  nodes <- reactive({
    generate_nodes(
      components,
      organs,
      regulations,
      background,
      diseases,
      organs_nodes_size,
      hormones_nodes_size
    )
  })

  edges <- reactive({
    generate_edges(
      components,
      organs,
      regulations,
      diseases,
      organs_edges_size,
      hormones_edges_size
    )
  })

  # Generate the output of the Ca graph to be used in body
  output$network_CaPO4 <- renderVisNetwork({

    nodes<- nodes()
    edges <- edges()
    regulations()

    generate_network(nodes, edges, usephysics = TRUE, isMobile) %>%
      # simple click event to select a node
      visEvents(selectNode = paste0("function(nodes) { Shiny.setInputValue('", ns("current_node_id"), "', nodes.nodes); }")) %>%
      # unselect node event
      visEvents(deselectNode = paste0(
        "function(nodes) {
          Shiny.setInputValue('", ns("current_node_id"), "', 'null');
          Shiny.setInputValue('", ns("current_node_id_zoom"), "', 'null');
         }
        "
        )
      ) %>%
      # add the doubleclick for nodes (zoom view)
      visEvents(doubleClick = paste0("function(nodes) { Shiny.setInputValue('", ns("current_node_id_zoom"), "', nodes.nodes); }")) %>%
      # simple click event for selecting edges
      visEvents(selectEdge = paste0("function(edges) { Shiny.setInputValue('", ns("current_edge_id"), "', edges.edges); }")) %>%
      # unselect edge event
      visEvents(deselectEdge = paste0("function(edges) { Shiny.setInputValue('", ns("current_edge_id"), "', 'null'); }")) %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", stabilized = "function() { this.moveTo({ position: {x:0, y:-13.43}, offset: {x: 0, y:0} }); }") %>%
      # scale for cellphones and tablets
      visEvents(type = "on", initRedraw = paste0("function() { this.moveTo({scale:", if (isMobile()) 0.3 else 0.6, "}); }"))
  })


  #-------------------------------------------------------------------------
  # Network UI: next and back button, progress bar, ...
  #-------------------------------------------------------------------------

  # back button
  output$back_button <- renderUI({

    if (diseases$php1() | diseases$hypopara() |
        diseases$hypoD3() | help()) {
      column(
        width = 4,
        align = "left",
        actionBttn(
          inputId = ns("previousStep"),
          label = "Back",
          style = "simple",
          color = "danger",
          size = "md",
          icon = icon("step-backward")
        )
      )
    }
  })

  # next button
  output$next_button <- renderUI({

    if (diseases$php1() | diseases$hypopara() |
        diseases$hypoD3() | help()) {
      column(
        width = 4,
        align = "right",
        # manually add an extra class for shinyjs
        tagAppendAttributes(
          actionBttn(
            inputId = ns("nextStep"),
            label = "Next",
            style = "simple",
            color = "danger",
            size = "md",
            icon = icon("step-forward")
          ),
          class = "nextStep"
        )
      )
    }
  })

  # create a navigation counter to trigger sequential graph animation
  counter_nav <- reactiveValues(diagram = 0)

  # progress
  output$counter_progress <- renderUI({
    if (diseases$php1() | diseases$hypopara() |
        diseases$hypoD3() | help()) {

      column(
        width = 4,
        align = "center",
        progressBar(
          id = ns("progress"),
          value = 0,
          total = 6,
          title = "Progress",
          size = "s",
          striped = TRUE,
          status = NULL,
          display_pct = FALSE
        )
      )
    }
  })

  # update the progress bar
  observeEvent(counter_nav$diagram, {
    updateProgressBar(
      session,
      id = ns("progress"),
      value = counter_nav$diagram,
      total = 6,
      status = if (counter_nav$diagram <= 1) {
        "danger"
      } else if (counter_nav$diagram >= 2 & counter_nav$diagram <= 5) {
        "warning"
      } else {
        "success"
      }
    )
  })


  # counter decrease
  observeEvent(input$previousStep, {
    if (counter_nav$diagram == 0) {
      NULL
    } else {
      counter_nav$diagram <- counter_nav$diagram - 1
    }
  })

  # counter incrementation
  observeEvent(input$nextStep,{
    counter_nav$diagram <- counter_nav$diagram + 1
  })

  # reset the counter if higher than 5
  observeEvent(input$nextStep, {
    if (counter_nav$diagram > 6) {
      counter_nav$diagram <- 0
    }
  })


  # add the blinking button class to the next button in animations
  observe({
    req(!is.null(input$nextStep))
    if (input$nextStep == 0) {
      shinyjs::runjs("$('.nextStep').addClass('blinking-button')")
    } else {
      shinyjs::runjs("$('.nextStep').removeClass('blinking-button')")
    }
  })


  #-------------------------------------------------------------------------
  # Network animations
  #-------------------------------------------------------------------------

  # change the selected node size to
  # better highlight it
  last <- reactiveValues(selected_node = NULL, selected_edge = NULL)

  observeEvent(input$current_node_id, {
    selected_node <- input$current_node_id
    nodes <- nodes()
    # javascript return null instead of NULL
    # cannot use is.null
    if (!identical(selected_node, "null")) {
      last$selected_node <- selected_node
      # organ nodes
      if (selected_node %in% c(1:5, 7:8, 11)) {
        nodes$size[selected_node] <- 100
        # Kidney zoom node
      } else if (selected_node == 6) {
        nodes$size[selected_node] <- 214
        # regulation nodes
      } else {
        nodes$size[selected_node] <- 57
      }
      visNetworkProxy(ns("network_CaPO4")) %>%
        visUpdateNodes(nodes = nodes)
      # reset the node size when unselected
    } else {
      if (last$selected_node %in% c(1:5, 7:8, 11)) {
        nodes$size[last$selected_node] <- 70
      } else if (last$selected_node == 6) {
        nodes$size[last$selected_node] <- 150
      } else {
        nodes$size[last$selected_node] <- 40
      }
      visNetworkProxy(ns("network_CaPO4")) %>%
        visUpdateNodes(nodes = nodes)
    }
  })

  # change the selected edge size to
  # better highlight it
  observeEvent(input$current_edge_id,{
    req(input$current_edge_id)
    selected_edge <- input$current_edge_id
    edges <- edges()
    edge_id <- match(selected_edge, edges$id)
    if (!identical(selected_edge, "null")) {
      last$selected_edge <- edge_id
      # organs edges
      if (edge_id %in% c(1:12)) {
        edges$width[edge_id] <- 24
        # regulations edges
      } else {
        edges$width[edge_id] <- 12
      }
      visNetworkProxy(ns("network_CaPO4")) %>%
        visUpdateEdges(edges = edges)
      # reset the edge size when unselected
    } else {
      if (edge_id %in% c(1:12)) {
        edges$width[edge_id] <- 8
      } else {
        edges$width[edge_id] <- 4
      }
      visNetworkProxy(ns("network_CaPO4")) %>%
        visUpdateEdges(edges = edges)
    }
  })

  # reset also if another simulation is choosen
  observeEvent(c(diseases$php1(), diseases$hypopara(), diseases$hypoD3()), {
    counter_nav$diagram <- 0
    edges<- edges()
    edges$color <- "black"
    edges$witdh <- 4
    visNetworkProxy(ns("network_CaPO4"), session) %>%  # then reset the graph
      visUpdateEdges(edges = edges)
  })

  # Animations of arrows when event occurs (php1, hypopara, hypoD3)
  observeEvent(input$nextStep | input$previousStep , {

    edges <- edges()
    current_sim <- extract_running_sim(diseases)
    # only if a simulation is selected
    # dynamics simulations are excluded since calculations
    # are performed live contrary to steady-state simulations
    if (!is_empty(current_sim)) {
      if (eval(parse(text = paste0("diseases$", current_sim, "()")))) {

        # the code below ensures that nodes related to
        # perturbations, ie PTHg for php1 and hypopara
        # D3 nodes for hypoD3, blink when the counter equals 1
        if (counter_nav$diagram == 1) {
          nodes <- nodes()
          if (diseases$php1() | diseases$hypopara()) {
            lapply(1:2, FUN = function(i){
              if ((i %% 2) != 0) {
                nodes$hidden[11] <- TRUE
                visNetworkProxy(ns("network_CaPO4")) %>%
                  visUpdateNodes(nodes = nodes)
              } else {
                nodes$hidden[11] <- FALSE
                visNetworkProxy(ns("network_CaPO4")) %>%
                  visUpdateNodes(nodes = nodes)
              }
              Sys.sleep(0.5)
            })
          } else if (diseases$hypoD3()) {
            lapply(1:2, FUN = function(i){
              if ((i %% 2) != 0) {
                nodes$hidden[c(13:15)] <- TRUE
                visNetworkProxy(ns("network_CaPO4")) %>%
                  visUpdateNodes(nodes = nodes)
              } else {
                nodes$hidden[c(13:15)] <- FALSE
                visNetworkProxy(ns("network_CaPO4")) %>%
                  visUpdateNodes(nodes = nodes)
              }
              Sys.sleep(0.5)
            })
          }
        }

        # make arrow yellow and blink
        # (see model_utils.R)
        arrow_lighting(
          edges = edges,
          simulation = current_sim,
          counter = counter_nav$diagram,
          session
        )
      }
    }
  })


  #-------------------------------------------------------------------------
  # Zoom events
  #-------------------------------------------------------------------------

  observeEvent(input$current_node_id_zoom, {

    node_id_zoom <- switch (as.character(input$current_node_id_zoom),
      NULL = NULL,
      "1" = "intestine",
      "4" = "bones",
      "6" = "kidneys",
      "11" = "PTHg"
    )

    ## show the modal related to the current running simulation
    current_sim <- extract_running_sim(diseases)
    if (is.null(current_sim)) {
      if (!is.null(node_id_zoom)) {
        showModal(eval(parse(text = paste("modal_zoom", node_id_zoom, sep = "_"))))
      }
    } else {
      if (!is.null(node_id_zoom)) {
        showModal(eval(parse(text = paste("modal_zoom", node_id_zoom, current_sim, sep = "_"))))
      }
    }
  })

  #-------------------------------------------------------------------------
  # Get node position, for debug only
  #-------------------------------------------------------------------------

  vals <- reactiveValues(coords = NULL, viewposition = NULL, scale = NULL)

  # Node position
  # useful to set a proper layout
  output$position <- renderPrint(vals$position)
  observe({
    invalidateLater(1000)
    visNetworkProxy(ns("network_CaPO4")) %>% visGetPositions()
    vals$coords <- if (!is.null(input$network_CaPO4_positions))
      do.call(rbind, input$network_CaPO4_positions)
  })

  # view position (of the camera)
  # useful to set a proper view
  output$viewposition <- renderPrint(vals$viewposition)
  observe({
    invalidateLater(1000)
    visNetworkProxy(ns("network_CaPO4")) %>% visGetViewPosition()
    vals$viewposition <- if (!is.null(input$network_CaPO4_viewPosition))
      do.call(rbind, input$network_CaPO4_viewPosition)
  })

  # scale (get the zoomView...)
  output$scale <- renderPrint(vals$scale)
  observe({
    invalidateLater(1000)
    visNetworkProxy(ns("network_CaPO4")) %>% visGetScale()
    vals$scale <- if (!is.null(input$network_CaPO4_scale))
      do.call(rbind, list(input$network_CaPO4_scale))
  })

  network_debug <- reactive({
    list(
      position = vals$coords,
      view = vals$viewposition,
      scale = vals$scale
    )
  })

  return(
    list(
      debug = network_debug,
      counter = reactive(counter_nav$diagram)
    )
  )

}
