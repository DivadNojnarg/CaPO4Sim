#' @title CardioRenal Network Generator
#'
#' @description Create a CardioRenal network taking nodes and edges as inputs
#'
#' @param nodes A dataframe of nodes provided by \link{generate_nodes}.
#' @param edges A dataframe of edges provided by \link{generate_edges}.
#' @param usephysics Whether to use physic. FALSE by default. A visNetwork API parameter.
#' @param isMobile Shiny input checking if the app is running on a cellphone/tablet.
#'
#' @export
generate_network <- function(nodes, edges, usephysics = FALSE, isMobile) {

  visNetwork::visNetwork(
    nodes,
    edges,
    width = "100%",
    height = "100%") %>%
    visNetwork::visNodes(
      shapeProperties =
        list(
          useBorderWithImage = FALSE,
          interpolation = FALSE
        )
    ) %>%
    # put shadow on false
    visNetwork::visEdges(
      shadow = FALSE,
      font = list(align = "horizontal")
    ) %>%
    # add group selection option
    visNetwork::visOptions(
      highlightNearest = FALSE,
      clickToUse = FALSE,
      manipulation = FALSE,
      collapse = FALSE,
      autoResize = if (isMobile()) FALSE else TRUE
    ) %>%
    # prevent edge from being selected when a node is selected
    visNetwork::visInteraction(
      hover = TRUE,
      hoverConnectedEdges = FALSE,
      selectConnectedEdges = FALSE,
      multiselect = FALSE,
      dragNodes = FALSE,
      dragView = FALSE,
      zoomView = FALSE,
      navigationButtons = FALSE,
      selectable = TRUE,
      tooltipStyle = '
        position: fixed;
        top: 50px;
        left: 50%;
        width: 50%;
        transform: translate(-50%, -50%);
        visibility: hidden;
        padding: 5px;
        padding-right: 10px;
        padding-bottom: 10px;
        white-space: nowrap;
        font-family: verdana;
        font-size: 14px;
        font-color: #000000;
        background-color: #FFFFFF;
        -moz-border-radius: 3px;
        -webkit-border-radius: 3px;
        border-radius: 3px;
        border: 1px solid #808074;
        box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
        z-index: 100;'
    ) %>%
    # stabilization prevents arrows from bouncing
    visNetwork::visPhysics(stabilization = TRUE, enabled = usephysics)
}


#' @title CardioRenal Nodes Generator
#'
#' @description Generate nodes for the CardioRenal network
#'
#' @param components Shiny input CardioRenal component selector. See \link{networkOptions}.
#' @param organs Shiny input to toggle organs display. See \link{networkOptions}.
#' @param regulations Shiny input to toggle hormone display. See \link{networkOptions}.
#' @param background Shiny input background selector. See \link{networkOptions}.
#' @param diseases Shiny input disease selector. See \link{diseaseSelect}.
#' @param organs_nodes_size Shiny input for organs node size. See \link{networkOptions}.
#' @param hormones_nodes_size Shiny input for hormones node size. See \link{networkOptions}..
#'
#' @export
generate_nodes <- function(components, organs, regulations, background, diseases,
                           organs_nodes_size, hormones_nodes_size) {

  req(organs_nodes_size(), hormones_nodes_size())

  data.frame(
    id = 1:8,
    shape = c(
      "image",
      "image",
      "image",
      "image",
      "image",
      "image",
      "image",
      "image"
    ),
    image = c(
      "CardioRenal_network/heart.svg", "CardioRenal_network/arteries.svg",
      "CardioRenal_network/veins.svg", "CardioRenal_network/lungs.svg",
      "CardioRenal_network/kidney.svg", "CardioRenal_network/kidney_zoom1.svg",
      "CardioRenal_network/urine.svg", "CardioRenal_network/brain.svg"
    ),
    label = c(rep("", 8)),
    fixed = list("x" = TRUE, "y" = TRUE),
    # node position tighlty depends on the selected background
    x = if (is.null(background())) {
      c(100, 150, 50, -100, -100, -333, 7, 5)
    } else if (background() == "rat") {
      c(100, 150, 50, -100, -100, -333, 7, 5)
    } else {
      c(100, 150, -50, -100, -100, -300, 7, 0)
    },

    y = if (is.null(background())) {
      c(-125, 90, 62, -175, 220, 475, 500, -550)
    } else if (background() == "rat") {
      c(-125, 90, 62, -175, 220, 475, 500, -550)
    } else {
      c(-125, 120, 30, -200, 270, 450, 500, -550)
    },

    color = list(
      background = "#97C2FC",
      border = "#97C2FC",
      highlight = list(background = "orange", border = "orange")
    ),
    size = c(
      organs_nodes_size(),
      1.15*organs_nodes_size(),
      rep(organs_nodes_size(), 3),
      150,
      organs_nodes_size(),
      1.2*organs_nodes_size()
    ),
    #fixed = list("x" = TRUE, "y" = TRUE),
    physics = rep(FALSE, 8),
    hidden = c(
      ## organs ##
      if (organs()) {
        rep(FALSE, 8)
      } else {
        rep(TRUE, 8)
      }
    ),
    stringsAsFactors = FALSE
  )
}



#' @title CardioRenal Edges Generator
#'
#' @description Generate edges for the CardioRenal network
#'
#' @param components Shiny input CardioRenal component selector. See \link{networkOptions}.
#' @param organs Shiny input to toggle organs display. See \link{networkOptions}.
#' @param regulations Shiny input to toggle hormone display. See \link{networkOptions}.
#' @param diseases Shiny input disease selector. See \link{diseaseSelect}.
#' @param organs_edges_size Shiny input for organs edges size. See \link{networkOptions}.
#' @param hormones_edges_size Shiny input for hormones edges size. See \link{networkOptions}.
#'
#' @export
generate_edges <- function(components, organs, regulations, diseases,
                           organs_edges_size, hormones_edges_size) {

  req(organs_edges_size(), hormones_edges_size())
  N_edges = 8

  data.frame(
    from = c(
      1, 2, 3, 1, 4, 2, 5, 5
    ),

    to = c(
      2, 3, 1, 4, 1, 5, 3, 7
    ),

    arrows = list(
      to = list(
        enabled = c(
          rep(TRUE,N_edges)
        ),
        scaleFactor = 1,
        type = "arrow"
      )
    ),

    label = c(
      rep("",N_edges)
    ),
    id = c(
      "1-2",
      "2-3",
      "3-1",
      "1-4",
      "4-1",
      "2-5",
      "5-3",
      "5-7"
    ),
    width = c(rep(organs_edges_size(), N_edges)),
    font.size = c(rep(25, N_edges)),
    font.align = c(  # "bottom", "top" or ""
      rep("", N_edges)
    ),
    color = list(color = c(rep("black", N_edges)), highlight = "yellow"),
    dashes = c(rep(FALSE, N_edges)),
    title = c(
      rep(NA, N_edges)
    ),
    smooth = c(rep(TRUE, N_edges)),
    length = c(
      rep(200, N_edges)
    ),
    # display option to show/hide specific species or regulatory network arrows
    hidden = c(
      ## organ arrows ##
      if (organs()) {
        c(
          rep(ifelse(is.element("Ca", components()) | is.element("PO4", components()), FALSE, TRUE),N_edges)
        )
      } else {
        rep(TRUE, N_edges)
      }
    )
  )
}
