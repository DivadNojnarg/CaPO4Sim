#' @title CaPO4 Network Generator
#'
#' @description Create a CaPO4 network taking nodes and edges as inputs
#'
#' @param nodes A dataframe of nodes provided by \link{generate_nodes}.
#' @param edges A dataframe of edges provided by \link{generate_edges}.
#' @param usephysics Whether to use physic. FALSE by default. A visNetwork API parameter.
#' @param isMobile Shiny input checking if the app is running on a cellphone/tablet.
#'
#' @export
generate_network <- function(nodes, edges, usephysics = FALSE, isMobile) {

  visNetwork(
    nodes,
    edges,
    width = "100%",
    height = "100%") %>%
    visNodes(
      shapeProperties =
        list(
          useBorderWithImage = FALSE,
          interpolation = FALSE
        )
    ) %>%
    # put shadow on false
    visEdges(
      shadow = FALSE,
      font = list(align = "horizontal")
    ) %>%
    # add group selection option
    visOptions(
      highlightNearest = FALSE,
      clickToUse = FALSE,
      manipulation = FALSE,
      collapse = FALSE,
      autoResize = if (isMobile()) FALSE else TRUE
    ) %>%
    # prevent edge from being selected when a node is selected
    visInteraction(
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
        visibility:hidden;
        padding: 5px;
        padding-right: 10px;
        padding-bottom: 10px;
        white-space: nowrap;
        font-family: verdana;
        font-size:14px;
        font-color:#000000;
        background-color: #FFFFFF;
        -moz-border-radius: 3px;
        -webkit-border-radius: 3px;
        border-radius: 3px;
        border: 1px solid #808074;
        box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
        z-index: 100;'
    ) %>%
    # stabilization prevents arrows from bouncing
    visPhysics(stabilization = TRUE, enabled = usephysics)
}


#' @title CaPO4 Nodes Generator
#'
#' @description Generate nodes for the CaPO4 network
#'
#' @param components Shiny input CaPO4 component selector. See \link{networkOptions}.
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
    id = 1:16,
    shape = c(
      "image",
      "image",
      "image",
      "image",
      "image",
      "image",
      "image",
      "image",
      ifelse(regulations(),"image","text"),
      ifelse(regulations(),"image","text"),
      "image",
      ifelse(regulations(),"image","text"),
      ifelse(regulations(),"image","text"),
      ifelse(regulations(),"image","text"),
      ifelse(regulations(),"image","text"),
      ifelse(regulations(),"image","text")
    ),
    image = c(
      "CaPO4_network/intestine.svg", "CaPO4_network/plasma.svg",
      "CaPO4_network/rapid-bone.svg", "CaPO4_network/bone.svg",
      "CaPO4_network/kidney.svg", "CaPO4_network/kidney_zoom1.svg",
      "CaPO4_network/urine.svg", "CaPO4_network/cells.svg",
      "CaPO4_network/Cap.svg", "CaPO4_network/PO4.svg",
      if (is.null(background())) {
        "CaPO4_network/parathyroid_gland.svg"
      } else if (background() == "rat") {
        "CaPO4_network/parathyroid_gland.svg"
      } else {
        "CaPO4_network/parathyroid_gland_human.svg"
      }
      ,"CaPO4_network/PTH.svg", "CaPO4_network/D3.svg",
      "CaPO4_network/D3.svg", "CaPO4_network/D3.svg",
      "CaPO4_network/FGF23.svg"
    ),
    label = c(rep("", 6), rep("", 10)),
    fixed = list("x" = TRUE, "y" = TRUE),

    # tooltip to display an image
    title = c(
      # intestinal absorption tooltip
      if (diseases$php1()) {
        HTML(
          paste("Detailed Ca intestinal absorption <hr>
                <a href=\"php1_zoom/intestine/php1_notif_intestine.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"php1_zoom/intestine/php1_notif_intestine.svg\"/></a>"
          )
        )
      } else if (diseases$hypopara()) {
        HTML(
          paste("Detailed Ca intestinal absorption <hr>
                <a href=\"hypopara_zoom/intestine/hypopara_notif_intestine.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"hypopara_zoom/intestine/hypopara_notif_intestine.svg\"/></a>"
          )
        )
      } else if (diseases$hypoD3()) {
        HTML(
          paste("Detailed Ca intestinal absorption <hr>
                <a href=\"hypoD3_zoom/intestine/hypoD3_notif_intestine.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"hypoD3_zoom/intestine/hypoD3_notif_intestine.svg\"/></a>"
          )
        )
      } else {
        HTML(
          paste("Detailed Ca intestinal absorption <hr>
                <a href=\"base_case_zoom/intestine/base_case_notif_intestine.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"base_case_zoom/intestine/base_case_notif_intestine.svg\"/></a>"
          )
        )
      },
      NA,

      # rapid bone pool tooltip
      NA,

      # deep bone zoom tooltip
      if (diseases$php1()) {
        HTML(
          paste("<div class=\"row\"> <div class=\"col-sm-6\">
                 Effect of PTH on bone <hr>
                 <a href=\"php1_zoom/bone/php1_notif_bone1.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                 align=\"center\"  src=\"php1_zoom/bone/php1_notif_bone1.svg\"/></a>
                 </div><div class=\"col-sm-6\">
                 Effect of D3 on bone <hr>
                 <a href=\"php1_zoom/bone/php1_notif_bone2.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                 align=\"center\"  src=\"php1_zoom/bone/php1_notif_bone2.svg\"/></a>
                 </div></div>"
          )
        )
      } else if (diseases$hypopara()) {
        HTML(
          paste("<div class=\"row\"> <div class=\"col-sm-6\">
                 Effect of PTH on bone <hr>
                 <a href=\"hypopara_zoom/bone/hypopara_notif_bone1.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                 align=\"center\"  src=\"hypopara_zoom/bone/hypopara_notif_bone1.svg\"/></a>
                 </div><div class=\"col-sm-6\">
                 Effect of D3 on bone <hr>
                 <a href=\"hypopara_zoom/bone/hypopara_notif_bone2.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                 align=\"center\"  src=\"hypopara_zoom/bone/hypopara_notif_bone2.svg\"/></a>
                 </div></div>"
          )
        )
      } else if (diseases$hypoD3()) {
        HTML(
          paste("<div class=\"row\"> <div class=\"col-sm-6\">
                 Effect of PTH on bone <hr>
                 <a href=\"hypoD3_zoom/bone/hypoD3_notif_bone1.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                 align=\"center\"  src=\"hypoD3_zoom/bone/hypoD3_notif_bone1.svg\"/></a>
                 </div><div class=\"col-sm-6\">
                 Effect of D3 on bone <hr>
                 <a href=\"hypoD3_zoom/bone/hypoD3_notif_bone2.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                 align=\"center\"  src=\"hypoD3_zoom/bone/hypoD3_notif_bone2.svg\"/></a>
                 </div></div>"
          )
        )
      } else {
        HTML(
          paste("<div class=\"row\"> <div class=\"col-sm-6\">
                 Effect of PTH on bone <hr>
                 <a href=\"base_case_zoom/bone/base_case_notif_bone1.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                 align=\"center\"  src=\"base_case_zoom/bone/base_case_notif_bone1.svg\"/></a>
                 </div><div class=\"col-sm-6\">
                 Effect of D3 on bone <hr>
                 <a href=\"base_case_zoom/bone/base_case_notif_bone2.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                 align=\"center\"  src=\"base_case_zoom/bone/base_case_notif_bone2.svg\"/></a>
                 </div></div>"
          )
        )
      },

      # kidney tooltip
      NA,

      # kidney_zoom tooltip
      if (diseases$php1()) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca PT reabsorption", "<hr>",
                "<a href=\"php1_zoom/kidney/php1_notif_kidney1.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"php1_zoom/kidney/php1_notif_kidney1.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Pi PT reabsorption", "<hr>",
                "<a href=\"php1_zoom/kidney/php1_notif_kidney2.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"php1_zoom/kidney/php1_notif_kidney2.svg\"/></a>",
                "</div>", "</div>", "<br>",
                "<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca TAL reabsorption", "<hr>",
                "<a href=\"php1_zoom/kidney/php1_notif_kidney3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"php1_zoom/kidney/php1_notif_kidney3.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Ca DCT reabsorption", "<hr>",
                "<a href=\"php1_zoom/kidney/php1_notif_kidney4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"php1_zoom/kidney/php1_notif_kidney4.svg\"/></a>",
                "</div>", "</div>"
          )
        )
      } else if (diseases$hypopara()) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca PT reabsorption", "<hr>",
                "<a href=\"hypopara_zoom/kidney/hypopara_notif_kidney1.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypopara_zoom/kidney/hypopara_notif_kidney1.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Pi PT reabsorption", "<hr>",
                "<a href=\"hypopara_zoom/kidney/hypopara_notif_kidney2.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypopara_zoom/kidney/hypopara_notif_kidney2.svg\"/></a>",
                "</div>", "</div>", "<br>",
                "<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca TAL reabsorption", "<hr>",
                "<a href=\"hypopara_zoom/kidney/hypopara_notif_kidney3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypopara_zoom/kidney/hypopara_notif_kidney3.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Ca DCT reabsorption", "<hr>",
                "<a href=\"hypopara_zoom/kidney/hypopara_notif_kidney4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypopara_zoom/kidney/hypopara_notif_kidney4.svg\"/></a>",
                "</div>", "</div>"
          )
        )
      } else if (diseases$hypoD3()) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca PT reabsorption", "<hr>",
                "<a href=\"hypoD3_zoom/kidney/hypoD3_notif_kidney1.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypoD3_zoom/kidney/hypoD3_notif_kidney1.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Pi PT reabsorption", "<hr>",
                "<a href=\"hypoD3_zoom/kidney/hypoD3_notif_kidney2.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypoD3_zoom/kidney/hypoD3_notif_kidney2.svg\"/></a>",
                "</div>", "</div>", "<br>",
                "<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca TAL reabsorption", "<hr>",
                "<a href=\"hypoD3_zoom/kidney/hypoD3_notif_kidney3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypoD3_zoom/kidney/hypoD3_notif_kidney3.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Ca DCT reabsorption", "<hr>",
                "<a href=\"hypoD3_zoom/kidney/hypoD3_notif_kidney4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypoD3_zoom/kidney/hypoD3_notif_kidney4.svg\"/></a>",
                "</div>", "</div>"
          )
        )
      } else {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca PT reabsorption", "<hr>",
                "<a href=\"base_case_zoom/kidney/base_case_notif_kidney1.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"base_case_zoom/kidney/base_case_notif_kidney1.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Pi PT reabsorption", "<hr>",
                "<a href=\"base_case_zoom/kidney/base_case_notif_kidney2.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"base_case_zoom/kidney/base_case_notif_kidney2.svg\"/></a>",
                "</div>", "</div>", "<br>",
                "<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca TAL reabsorption", "<hr>",
                "<a href=\"base_case_zoom/kidney/base_case_notif_kidney3.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"base_case_zoom/kidney/base_case_notif_kidney3.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Ca DCT reabsorption", "<hr>",
                "<a href=\"base_case_zoom/kidney/base_case_notif_kidney4.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"base_case_zoom/kidney/base_case_notif_kidney4.svg\"/></a>",
                "</div>", "</div>"
          )
        )
      },
      rep(NA, 2),

      # calcium tooltip
      NA,
      # PO4 tooltip
      NA,

      # PTH synthesis zoom
      if (diseases$php1()) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed PTH mechanisms", "<hr>",
                "<a href=\"php1_zoom/PTHg/php1_notif_PTHg1.svg\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                    align=\"center\"  src=\"php1_zoom/PTHg/php1_notif_PTHg1.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Effect of D3 on PTH synthesis", "<hr>",
                "<a href=\"php1_zoom/PTHg/php1_notif_PTHg2.svg\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"php1_zoom/PTHg/php1_notif_PTHg2.svg\"/></a>",
                "</div>", "</div>", "<br>","<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Effect of Pi on PTH synthesis", "<hr>",
                "<a href=\"php1_zoom/PTHg/php1_notif_PTHg3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"php1_zoom/PTHg/php1_notif_PTHg3.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">", "Effect of Ca on PTH secretion", "<hr>",
                "<a href=\"php1_zoom/PTHg/php1_notif_PTHg4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"php1_zoom/PTHg/php1_notif_PTHg4.svg\"/></a>",
                "</div>", "</div>"
          )
        )
      } else if (diseases$hypopara()) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed PTH mechanisms", "<hr>",
                "<a href=\"hypopara_zoom/PTHg/hypopara_notif_PTHg1.svg\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                    align=\"center\"  src=\"hypopara_zoom/PTHg/hypopara_notif_PTHg1.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Effect of D3 on PTH synthesis", "<hr>",
                "<a href=\"hypopara_zoom/PTHg/hypopara_notif_PTHg2.svg\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypopara_zoom/PTHg/hypopara_notif_PTHg2.svg\"/></a>",
                "</div>", "</div>", "<br>","<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Effect of Pi on PTH synthesis", "<hr>",
                "<a href=\"hypopara_zoom/PTHg/hypopara_notif_PTHg3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypopara_zoom/PTHg/hypopara_notif_PTHg3.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">", "Effect of Ca on PTH secretion", "<hr>",
                "<a href=\"hypopara_zoom/PTHg/hypopara_notif_PTHg4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypopara_zoom/PTHg/hypopara_notif_PTHg4.svg\"/></a>",
                "</div>", "</div>"
          )
        )
      } else if (diseases$hypoD3()) {
        HTML(
          paste("Detailed PTH mechanisms <hr>
                   <a href=\"hypoD3_zoom/PTHg/hypoD3_notif_PTHg1.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypoD3_zoom/PTHg/hypoD3_notif_PTHg1.svg\"/></a>"
          )
        )
      } else {
        HTML(
          paste("Detailed PTH mechanisms <hr>
                <a href=\"base_case_zoom/PTHg/base_case_notif_PTHg.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                align=\"center\"  src=\"base_case_zoom/PTHg/base_case_notif_PTHg.svg\"/></a>"
          )
        )
      },
      # PTH tooltip
      NA,
      # D3 tooltip
      NA,
      # D3 tooltip
      NA,
      # D3 tooltip
      NA,
      # FGF23 tooltip
      NA
    ),

    # node position tighlty depends on the selected background
    x = if (is.null(background())) {
      c(38, -65, -65, -256, 180, 360, 170, -190, 290, 320, 41, -418, 330, 385, -386, 481)
    } else if (background() == "rat") {
      c(38, -65, -65, -256, 180, 360, 170, -190, 290, 320, 41, -418, 330, 385, -386, 481)
    } else {
      c(13, -80, -185, -322, 157, 333, 7, -175, 290, 320, 9, -466, 330, 385, -386, 481)
    },

    y = if (is.null(background())) {
      c(-150, 195, 472, 460, 0, 230, 506, 0, -317, -633, -452, 240, -452, 0, -106, -452)
    } else if (background() == "rat") {
      c(-150, 195, 472, 460, 0, 230, 506, 0, -317, -633, -452, 240, -452, 0, -106, -452)
    } else {
      c(23, 320, 524, 214, 189, 439, 581, 88, -317, -633, -449, 400, -452, 0, -106, -452)
    },

    color = list(
      background = "#97C2FC",
      border = "#97C2FC",
      highlight = list(background = "orange", border = "orange")
    ),
    size = c(
      rep(organs_nodes_size(), 5),
      150,
      rep(organs_nodes_size(), 2),
      rep(hormones_nodes_size(), 2),
      organs_nodes_size(),
      rep(hormones_nodes_size(), 5)
    ),
    #fixed = list("x" = TRUE, "y" = TRUE),
    physics = rep(FALSE, 16),
    hidden = c(
      ## organs ##
      if (organs()) {
        c(rep(FALSE, 7),
          # PO4 Cells
          ifelse(is.element("PO4", components()), ifelse(is.element("Ca", components()), FALSE, FALSE), TRUE))
      } else {
        rep(TRUE, 8)
      },
      ## Hormones ##
      # Ca plasma
      ifelse(regulations(), ifelse(is.element("Ca", components()), FALSE, TRUE), TRUE),
      # PO4 plasma
      ifelse(regulations(),
             ifelse(is.element("PO4", components()) &
                      (is.element("D3", components()) |
                         is.element("PTH", components()) |
                         is.element("FGF23", components())),
                    FALSE, TRUE), TRUE),
      # PTHg
      ifelse(regulations(), ifelse(is.element("PTH", components()), FALSE, TRUE), TRUE),
      # PTH plasma
      TRUE,
      # ifelse(organs(),
      #        ifelse(regulations(),
      #               ifelse(is.element("PTH", components()), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation
      ifelse(regulations(),
             ifelse(is.element("D3", components()) &
                      (is.element("PO4", components()) |
                         is.element("Ca", components()) |
                         is.element("PTH", components()) |
                         is.element("FGF23", components())),
                    FALSE, TRUE), TRUE),
      # D3 plasma
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("D3", components()), FALSE, TRUE), TRUE), TRUE),
      # D3 plasma
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("D3", components()), FALSE, TRUE), TRUE), TRUE),
      # FGF23
      ifelse(regulations(), ifelse(is.element("FGF23", components()), FALSE, TRUE), TRUE)
    ),
    stringsAsFactors = FALSE
  )
}



#' @title CaPO4 Edges Generator
#'
#' @description Generate edges for the CaPO4 network
#'
#' @param components Shiny input CaPO4 component selector. See \link{networkOptions}.
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
  data.frame(
    from = c(
      1,
      # change the from direction depending on the net flux result
      if (diseases$php1()) {
        2
      } else if (diseases$hypopara()) {
        3
      } else if (diseases$hypoD3()) {
        3
      } else {
        2
      },
      if (diseases$php1()) {
        2
      } else if (diseases$hypopara()) {
        3
      } else if (diseases$hypoD3()) {
        3
      } else {
        2
      },
      rep(3, 2), 4, 2, rep(5, 2), rep(5, 2),
      if (diseases$php1()) {
        2
      } else if (diseases$hypopara()) {
        8
      } else if (diseases$hypoD3()) {
        8
      } else {
        2
      },
      rep(9, 3), rep(10, 3), rep(11, 2), 11, rep(13, 2), rep(14, 2), rep(15, 2), rep(16, 2)
    ),

    to = c(
      2,
      if (diseases$php1()) {
        3
      } else if (diseases$hypopara()) {
        2
      } else if (diseases$hypoD3()) {
        2
      } else {
        3
      },
      if (diseases$php1()) {
        3
      } else if (diseases$hypopara()) {
        2
      } else if (diseases$hypoD3()) {
        2
      } else {
        3
      },
      rep(4, 2), 2, 5, rep(2, 2), rep(7, 2),
      if (diseases$php1()) {
        8
      } else if (diseases$hypopara()) {
        2
      } else if (diseases$hypoD3()) {
        2
      } else {
        8
      },
      11, 5,
      13, 11, 13, 16, 5, 13, 4, 11, 16, 14, 5, 4, 1, 13, 5
    ),

    arrows = list(
      to = list(
        enabled = c(
          TRUE,
          # show or hode arrow symbol depending on the net flux result
          rep(if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) TRUE else FALSE, 2),
          rep(TRUE, 8),
          if (diseases$php1() | diseases$hypopara() | diseases$hypoD3()) TRUE else FALSE,
          rep(TRUE, 17)
        ),
        scaleFactor = 1,
        type = "arrow"
      )
    ),

    label = c(
      "",
      "Net Ca",
      "Net PO4",
      "Ca",
      "PO4",
      rep("", 2),
      "Ca",
      "PO4",
      "Ca",
      "PO4",
      "Net PO4",
      rep("-", 3),
      "+",
      "-",
      "+",
      "",
      "+",
      "+",
      "-",
      "+",
      "-",
      "+",
      "+",
      "+",
      "-",
      "-"
    ),
    id = c(
      "Abs_int",
      "Net_Ca_pf",
      "Net_PO4_pf",
      "Ac_Ca",
      "Ac_PO4",
      "Res",
      7,
      "Reabs_Ca",
      "Reabs_PO4",
      "U_Ca",
      "U_PO4",
      "Net_PO4_cells",
      13:29
    ),
    width = c(rep(organs_edges_size(), 12), rep(hormones_edges_size(), 17)),
    font.size = c(rep(25, 12), rep(60, 17)),
    font.align = c(
      "",
      if (!diseases$hypopara() | !diseases$hypoD3()) "bottom" else "top",
      if (!diseases$hypopara() | !diseases$hypoD3()) "bottom" else "top",
      "top",
      "bottom",
      rep("", 4),
      "bottom",
      "top",
      "bottom",
      "bottom",
      rep("top", 2),
      "top",
      "top",
      "top",
      "",
      "bottom",
      "top",
      "top",
      "bottom",
      "bottom",
      "top",
      "top",
      rep("top", 2),
      "bottom"
    ),
    color = list(color = c(rep("black", 29)), highlight = "yellow"),
    dashes = c(rep(FALSE, 12), rep(TRUE, 17)),
    title = c(
      rep(NA, 3),
      rep(NA, 9),
      rep(NA, 2),
      rep(NA, 15)
    ),
    smooth = c(rep(TRUE, 29)),
    length = c(
      200,
      rep(300, 2),
      rep(300, 2),
      200,
      300,
      200,
      rep(300, 4),
      rep(200, 8),
      1800,
      rep(200, 8)
    ),
    # to show either Ca or PO4 or CaPO4 network arrows
    hidden = c(
      ## organ arrows ##
      if (organs()) {
        c(
          ifelse(is.element("Ca", components()) | is.element("PO4", components()), FALSE, TRUE),
          ifelse(is.element("Ca", components()), ifelse(is.element("PO4", components()), FALSE, FALSE), TRUE),
          ifelse(is.element("PO4", components()), ifelse(is.element("Ca", components()), FALSE, FALSE), TRUE),
          ifelse(is.element("Ca", components()), ifelse(is.element("PO4", components()), FALSE, FALSE), TRUE),
          ifelse(is.element("PO4", components()), ifelse(is.element("Ca", components()), FALSE, FALSE), TRUE),
          rep(ifelse(is.element("Ca", components()) | is.element("PO4", components()), FALSE, TRUE), 2),
          ifelse(is.element("Ca", components()), ifelse(is.element("PO4", components()), FALSE, FALSE), TRUE),
          ifelse(is.element("PO4", components()), ifelse(is.element("Ca", components()), FALSE, FALSE), TRUE),
          ifelse(is.element("Ca", components()), ifelse(is.element("PO4", components()), FALSE, FALSE), TRUE),
          rep(ifelse(is.element("PO4", components()), ifelse(is.element("Ca", components()), FALSE, FALSE), TRUE), 2)
        )
      } else {
        rep(TRUE, 12)
      },

      ## hormonal regulations arrows ##
      # Ca regulation to PTH
      ifelse(regulations(), ifelse(is.element("PTH", components()) & is.element("Ca", components()), FALSE, TRUE), TRUE),
      # Ca to Kidney
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("Ca", components()), FALSE, TRUE), TRUE), TRUE),
      # Ca regulation to D3
      ifelse(regulations(), ifelse(is.element("D3", components()) & is.element("Ca", components()), FALSE, TRUE), TRUE),
      # PO4 regulation to PTH
      ifelse(regulations(), ifelse(is.element("PTH", components()) & is.element("PO4", components()), FALSE, TRUE), TRUE),
      # PO4 regulation to D3
      ifelse(regulations(), ifelse(is.element("D3", components()) & is.element("PO4", components()), FALSE, TRUE), TRUE),
      # PO4 regulation to FGF23
      ifelse(regulations(), ifelse(is.element("FGF23", components()) & is.element("PO4", components()), FALSE, TRUE), TRUE),
      # PTH regulation to kidney
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("PTH", components()), FALSE, TRUE), TRUE), TRUE),
      # PTH regulation to D3
      ifelse(regulations(), ifelse(is.element("D3", components()) & is.element("PTH", components()), FALSE, TRUE), TRUE),
      # PTH regulation to bone
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("PTH", components()), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation to PTH
      ifelse(regulations(), ifelse(is.element("PTH", components()) & is.element("D3", components()), FALSE, TRUE), TRUE),
      # D3 regulation to FGF23
      ifelse(regulations(), ifelse(is.element("FGF23", components()) & is.element("D3", components()), FALSE, TRUE), TRUE),
      # D3 regulation to D3
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("D3", components()), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation to kidney
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("D3", components()), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation to bone
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("D3", components()), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation to intestine
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("D3", components()), FALSE, TRUE), TRUE), TRUE),
      # FGF regulation to D3
      ifelse(regulations(), ifelse(is.element("D3", components()) & is.element("FGF23", components()), FALSE, TRUE), TRUE),
      # FGF regulation to kidney
      ifelse(organs(), ifelse(regulations(), ifelse(is.element("FGF23", components()), FALSE, TRUE), TRUE), TRUE)
    ),
    stringsAsFactors = FALSE
  )
}
