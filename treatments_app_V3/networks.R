#------------------------------------------------------------------------- 
#  This codes contains all network skeletons for CaPO4, PTH, ...
#  For each network, we define two dataframe: node contains all informations
#  related to nodes and edges to edges...
#
#-------------------------------------------------------------------------


# This function is used to generate a network as well
# as basic options such as physics, manipulations,
# selection
generate_network <- function(input, nodes, edges, usephysics = FALSE) {
  
  visNetwork(
    nodes, 
    edges, 
    width = "100%", 
    height = "100%"
  ) %>%
    visNodes(
      shapeProperties = list(
        interpolation = TRUE
      )
    ) %>%
    # put shadow on false
    visEdges(
      shadow = FALSE, 
      smooth = TRUE,
      font = list(align = "horizontal")
    ) %>%
    # add group selection option
    visOptions(
      highlightNearest = FALSE, 
      clickToUse = FALSE, 
      manipulation = FALSE, 
      collapse = FALSE,
      autoResize = if (input$isMobile) FALSE else TRUE
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
      tooltipStyle = 
        'position: fixed;
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
         z-index: 100;
        ' 
    ) %>% 
    # stabilization prevents arrows from bouncing
    visPhysics(
      stabilization = TRUE, 
      enabled = usephysics
    )
}

# % % % % #
#         #
#  CaPO4  #
#         #
# % % % % #

# Generate nodes for the CaPO4 network
generate_nodes_Ca <- function(input) {
  
  data.frame(
    id = 1:16,
    shape = rep("image", 16), 
    image = c(
      "CaPO4_network/intestine.svg", "CaPO4_network/plasma.svg",
      "CaPO4_network/rapid-bone.svg", "CaPO4_network/bone.svg",
      "CaPO4_network/kidney.svg", "CaPO4_network/kidney_zoom1.svg",
      "CaPO4_network/urine.svg", "CaPO4_network/cells.svg", 
      "CaPO4_network/Cap.svg", "CaPO4_network/PO4.svg",
      if (is.null(input$background_choice)) {
        "CaPO4_network/parathyroid_gland.svg"
      } else if (input$background_choice == "rat") {
        "CaPO4_network/parathyroid_gland.svg"
      } else {
        "CaPO4_network/parathyroid_gland_human.svg"
      }
      , "CaPO4_network/PTH.svg", "CaPO4_network/D3.svg",
      "CaPO4_network/D3.svg", "CaPO4_network/D3.svg", 
      "CaPO4_network/FGF23.svg"
    ),
    label = c(rep("", 6), rep("",10)),
    # tooltip to display an image
    title = rep(NA, 16),
    
    x = if (is.null(input$background_choice)) {
      c(38,-65,-65,-256,180,360,170,-190,290,320,41,-418,330,385,-386,481)
    } else if (input$background_choice == "rat") {
      c(38,-65,-65,-256,180,360,170,-190,290,320,41,-418,330,385,-386,481)
      # for human background
    } else {
      c(13,-80,-185,-322,157,333,7,-175,290,320,9,-466,330,385,-386,481)
    },
    
    y = if (is.null(input$background_choice)) {
      c(-150,195,472,460,0,230,506,0,-317,-633,-452,240,-452,0,-106,-452)
    } else if (input$background_choice == "rat") {
      c(-150,195,472,460,0,230,506,0,-317,-633,-452,240,-452,0,-106,-452)
      # for human background
    } else {
      c(23,320,524,214,189,439,581,88,-317,-633,-449,400,-452,0,-106,-452)
    },
    
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(rep(input$size_organs,5), 150, rep(input$size_organs,2), 
             rep(input$size_hormones,2), input$size_organs, rep(input$size_hormones,5)),
    #fixed = list("x" = TRUE, "y" = TRUE),
    physics = rep(FALSE,16),
    hidden = c(
      ## organs ##
      if (input$network_organ_choice == TRUE) {
        c(rep(FALSE, 7),
          # PO4 Cells
          ifelse(is.element("PO4", input$network_Ca_choice), 
                 ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE))
      } else {
        rep(TRUE, 8)
      },
      ## Hormones ##
      # Ca plasma
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("Ca", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PO4 plasma
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("PO4", input$network_Ca_choice) & 
                      (is.element("D3", input$network_Ca_choice) | 
                         is.element("PTH", input$network_Ca_choice) | 
                         is.element("FGF23", input$network_Ca_choice)), 
                    FALSE, TRUE), TRUE),
      # PTHg
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PTH plasma
      TRUE,
      # ifelse(input$network_organ_choice, 
      #        ifelse(input$network_hormonal_choice,
      #               ifelse(is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("D3", input$network_Ca_choice) & 
                      (is.element("PO4", input$network_Ca_choice) |
                         is.element("Ca", input$network_Ca_choice) | 
                         is.element("PTH", input$network_Ca_choice) | 
                         is.element("FGF23", input$network_Ca_choice)), 
                    FALSE, TRUE), TRUE),
      # D3 plasma
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice,
                    ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # D3 plasma
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice,
                    ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # FGF23
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("FGF23", input$network_Ca_choice), FALSE, TRUE), TRUE)
    ),
    stringsAsFactors = FALSE
  )
}

# Generate edges for the CaPO4 network
generate_edges_Ca <- function(input) {
  req(input$width_organs, input$width_hormones)
  data.frame(
    from = c(
      1, 2, 3, rep(3, 2), 4, 2, rep(5, 2), rep(5, 2), 8,
      rep(9, 3), rep(10, 3), rep(11, 2), 11, rep(13, 2), 
      rep(14, 2), rep(15, 2), rep(16, 2)
    ), 
    to = c(
      2, 3, 2, rep(4, 2), 2, 5, rep(2, 2), rep(7, 2), 
      2, 11, 5, 13, 11, 13, 16, 5, 13, 4, 11, 16, 
      14, 5, 4, 1, 13, 5
    ),
    arrows = list(
      to = list(
        enabled = c(
          TRUE, 
          rep(FALSE,2), 
          rep(TRUE,8), 
          FALSE, 
          rep(TRUE,17)
        ), 
        scaleFactor = 1, 
        type = "arrow"
      )
    ),
    label = c(
      "", "Net Ca", "Net PO4", "Ca", "PO4", rep("", 2), "Ca", "PO4",
      "Ca", "PO4", "Net PO4", rep("-", 3), "+", "-", "+",
      "", "+", "+", "-", "+", "-", "+", "+", "+", "-", "-"
    ),
    id = c(
      "Abs_int", "Net_Ca_pf", "Net_PO4_pf",
      "Ac_Ca", "Ac_PO4", "Res", 7, "Reabs_Ca",
      "Reabs_PO4", "U_Ca", "U_PO4", "Net_PO4_cells", 
      13:29
    ),
    width = c(rep(input$width_organs,12), rep(input$width_hormones,17)),
    font.size = c(rep(25,12),rep(60,17)),
    font.align = c(
      "","top","bottom","top","bottom",rep("",4),"bottom",
      "top","bottom","bottom",rep("top",2),"top","top",
      "top","","bottom","top","top","bottom","bottom","top","top",
      rep("top",2),"bottom"
    ),
    color = list(color = c(rep("black", 29)), highlight = "yellow"),
    dashes = c(rep(FALSE,12), rep(TRUE,17)),
    title = c(rep(NA,3), rep(NA, 9), rep(NA, 2), rep(NA,15)),
    smooth = c(rep(TRUE,29)),
    length = c(200,rep(300,2),rep(300,2),200,300,
               200,rep(300,4),rep(200,8), 1700, rep(200,8)),
    # to show either Ca or PO4 or CaPO4 network arrows
    hidden = c(
      ## organ arrows ##
      if (input$network_organ_choice == TRUE) {
        c(ifelse(is.element("Ca", input$network_Ca_choice) | 
                   is.element("PO4", input$network_Ca_choice), FALSE, TRUE), 
          ifelse(is.element("Ca", input$network_Ca_choice), 
                 ifelse(is.element("PO4", input$network_Ca_choice),FALSE, FALSE), TRUE), 
          ifelse(is.element("PO4", input$network_Ca_choice), 
                 ifelse(is.element("Ca", input$network_Ca_choice),FALSE, FALSE), TRUE),
          ifelse(is.element("Ca", input$network_Ca_choice), 
                 ifelse(is.element("PO4", input$network_Ca_choice),FALSE, FALSE), TRUE),
          ifelse(is.element("PO4", input$network_Ca_choice), 
                 ifelse(is.element("Ca", input$network_Ca_choice),FALSE, FALSE), TRUE),
          rep(ifelse(is.element("Ca", input$network_Ca_choice) | 
                       is.element("PO4", input$network_Ca_choice), FALSE, TRUE), 2),
          ifelse(is.element("Ca", input$network_Ca_choice), 
                 ifelse(is.element("PO4", input$network_Ca_choice),FALSE, FALSE), TRUE),
          ifelse(is.element("PO4", input$network_Ca_choice), 
                 ifelse(is.element("Ca", input$network_Ca_choice),FALSE, FALSE), TRUE),
          ifelse(is.element("Ca", input$network_Ca_choice), 
                 ifelse(is.element("PO4", input$network_Ca_choice),FALSE, FALSE), TRUE),
          rep(ifelse(is.element("PO4", input$network_Ca_choice), 
                     ifelse(is.element("Ca", input$network_Ca_choice),FALSE, FALSE), TRUE), 2))
      } else {
        rep(TRUE, 12)
      },
      
      ## hormonal regulations arrows ##
      # Ca regulation to PTH
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("PTH", input$network_Ca_choice) & 
                      is.element("Ca", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # Ca to Kidney
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice, 
                    ifelse(is.element("Ca", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # Ca regulation to D3
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("D3", input$network_Ca_choice) & 
                      is.element("Ca", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PO4 regulation to PTH
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("PTH", input$network_Ca_choice) & 
                      is.element("PO4", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PO4 regulation to D3
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("D3", input$network_Ca_choice) & 
                      is.element("PO4", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PO4 regulation to FGF23
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("FGF23", input$network_Ca_choice) & 
                      is.element("PO4", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PTH regulation to kidney
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice, 
                    ifelse(is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # PTH regulation to D3
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("D3", input$network_Ca_choice) & 
                      is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PTH regulation to bone
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice, 
                    ifelse(is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation to PTH
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("PTH", input$network_Ca_choice) & 
                      is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # D3 regulation to FGF23
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("FGF23", input$network_Ca_choice) & 
                      is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # D3 regulation to D3
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice, 
                    ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation to kidney
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice, 
                    ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation to bone
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice, 
                    ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # D3 regulation to intestine
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice, 
                    ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE),
      # FGF regulation to D3
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("D3", input$network_Ca_choice) & 
                      is.element("FGF23", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # FGF regulation to kidney
      ifelse(input$network_organ_choice, 
             ifelse(input$network_hormonal_choice, 
                    ifelse(is.element("FGF23", input$network_Ca_choice), FALSE, TRUE), TRUE), TRUE)
    ), 
    stringsAsFactors = FALSE
  ) 
}