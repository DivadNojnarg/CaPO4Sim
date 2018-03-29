# *------------------------------------------------------------------
# | PROGRAM NAME: network.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This codes contains all network skeletons for CaPO4, PTH, .. 
# |           For each network, we define two dataframe: 
# |            node contains all informations
# |           related to nodes and edges to edges...
# *-----------------------------------------------------------------
# | DATA USED:  svg images from /CaPO4_network, external links from HSet
# |             svg images from /php1_zoom, /hypoD3_zoom and /hypopara_zoom
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1: generate the network
# |  PART 2: generate nodes
# |  PART 3: generate edges
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

# This function is used to generate a network as well
# as basic options such as physics, manipulations,
# selection
generate_network <- function(nodes, edges, usephysics = FALSE) {
  
  visNetwork(
    nodes, 
    edges, 
    width = "100%", 
    height = "100%") %>%
    visNodes(
      shapeProperties = 
        list(useBorderWithImage = FALSE, 
             interpolation = FALSE
        )
    ) %>%
    # put shadow on false
    visEdges(
      shadow = FALSE, 
      font = list(align = "horizontal")) %>%
    # add group selection option
    visOptions(
      highlightNearest = FALSE, 
      clickToUse = FALSE, 
      manipulation = FALSE, 
      collapse = FALSE,
      autoResize = TRUE) %>% 
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
        z-index: 100;') %>% 
    # stabilization prevents arrows from bouncing
    visPhysics(stabilization = TRUE, enabled = usephysics)
}

# % % % % #
#         #
#  CaPO4  #
#         #
# % % % % #

# Generate nodes for the CaPO4 network
Ca_int_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23937&Menu=1079&backbar=0"
PO4_int_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23408&Menu=1079&backbar=0)"
rapid_bone_web <- "https://academic.oup.com/ndt/article/26/8/2438/1917340/The-exchangeable-calcium-pool-physiology-and"
bone_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=24035&Menu=1079&backbar=0"
Ca_kidney_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23415&Menu=1079&backbar=0"
PO4_kidney_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23410&Menu=1079&backbar=0"
Ca_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=18891&Menu=1079&backbar=0"
PO4_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=18893&Menu=1079&backbar=0"
PTH_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23466&Menu=1079&backbar=0"
D3_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23484&Menu=1079&backbar=0"
FGF23_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23408&Menu=1079&backbar=0"

generate_nodes_Ca <- function(input) {
  
  data.frame(
    id = 1:16,
    shape = c(
      "image","image","image","image","image","image", "image","image",
      ifelse(input$network_hormonal_choice == "TRUE","image","text"),
      ifelse(input$network_hormonal_choice == "TRUE","image","text"),
      "image",
      ifelse(input$network_hormonal_choice == "TRUE","image","text"),
      ifelse(input$network_hormonal_choice == "TRUE","image","text"),
      ifelse(input$network_hormonal_choice == "TRUE","image","text"),
      ifelse(input$network_hormonal_choice == "TRUE","image","text"),
      ifelse(input$network_hormonal_choice == "TRUE","image","text")
    ), 
    image = c(
      "/CaPO4_network/intestine.svg", "/CaPO4_network/plasma.svg",
      "/CaPO4_network/rapid-bone.svg", "/CaPO4_network/bone.svg",
      "/CaPO4_network/kidney.svg", "/CaPO4_network/kidney_zoom1.svg",
      "/CaPO4_network/urine.svg", "/CaPO4_network/cells.svg", 
      "/CaPO4_network/Cap.svg", "/CaPO4_network/PO4.svg",
      if (is.null(input$background_choice)) {
        "/CaPO4_network/parathyroid_gland.svg"
      } else if (input$background_choice == "rat") {
        "/CaPO4_network/parathyroid_gland.svg"
      } else {
        "/CaPO4_network/parathyroid_gland_human.svg"
      }
      ,"/CaPO4_network/PTH.svg", "/CaPO4_network/D3.svg",
      "/CaPO4_network/D3.svg", "/CaPO4_network/D3.svg",
      "/CaPO4_network/FGF23.svg"
    ),
    label = c(rep("", 6), rep("",10)),
    fixed = list("x" = TRUE, "y" = TRUE),
    
    # tooltip to display an image
    title = c(
        # intestinal absorption tooltip
      if (input$run_php1) {
        HTML(
          paste("Detailed Ca intestinal absorption <hr>
                <a href=\"/php1_zoom/intestine/php1_notif_intestine.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                align=\"center\"  src=\"/php1_zoom/intestine/php1_notif_intestine.svg\"/></a>"
          )
        )
      } else if (input$run_hypopara) {
        HTML(
          paste("Detailed Ca intestinal absorption <hr>
                <a href=\"/hypopara_zoom/intestine/hypopara_notif_intestine.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                align=\"center\"  src=\"/hypopara_zoom/intestine/hypopara_notif_intestine.svg\"/></a>"
          )
        )
      } else if (input$run_hypoD3) {
        HTML(
          paste("Detailed Ca intestinal absorption <hr>
                <a href=\"/hypoD3_zoom/intestine/hypoD3_notif_intestine.svg\" target=\"_blank\">
                <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                align=\"center\"  src=\"/hypoD3_zoom/intestine/hypoD3_notif_intestine.svg\"/></a>"
          )
        )
        } else {
          paste(a("About intestinal Ca absorption", 
                  href = Ca_int_web,
                  target = "_blank"),br(),
                a("About intestinal PO4 absorption", 
                  href = PO4_int_web,
                  target = "_blank"))
        },
      "",
      
      # rapid bone pool tooltip
      paste(a("About rapid bone pool", 
              href = rapid_bone_web,
              target = "_blank")),
      
      # deep bone zoom tooltip
      if (input$run_php1) {
        HTML(
          paste("<div class=\"row\"> <div class=\"col-sm-6\">
                 Effect of PTH on bone <hr>
                 <a href=\"/php1_zoom/bone/php1_notif_bone1.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                 align=\"center\"  src=\"/php1_zoom/bone/php1_notif_bone1.svg\"/></a>
                 </div><div class=\"col-sm-6\">
                 Effect of D3 on bone <hr>
                 <a href=\"/php1_zoom/bone/php1_notif_bone2.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                 align=\"center\"  src=\"/php1_zoom/bone/php1_notif_bone2.svg\"/></a>
                 </div></div>"
          )
        )
      } else if (input$run_hypopara) {
        HTML(
          paste("<div class=\"row\"> <div class=\"col-sm-6\">
                 Effect of PTH on bone <hr>
                 <a href=\"/hypopara_zoom/bone/hypopara_notif_bone1.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                 align=\"center\"  src=\"/hypopara_zoom/bone/hypopara_notif_bone1.svg\"/></a>
                 </div><div class=\"col-sm-6\">
                 Effect of D3 on bone <hr>
                 <a href=\"/hypopara_zoom/bone/hypopara_notif_bone2.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                 align=\"center\"  src=\"/hypopara_zoom/bone/hypopara_notif_bone2.svg\"/></a>
                 </div></div>"
          )
        )
      } else if (input$run_hypoD3) {
        HTML(
          paste("<div class=\"row\"> <div class=\"col-sm-6\">
                 Effect of PTH on bone <hr>
                 <a href=\"/hypoD3_zoom/bone/hypoD3_notif_bone1.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                 align=\"center\"  src=\"/hypoD3_zoom/bone/hypoD3_notif_bone1.svg\"/></a>
                 </div><div class=\"col-sm-6\">
                 Effect of D3 on bone <hr>
                 <a href=\"/hypoD3_zoom/bone/hypoD3_notif_bone2.svg\" target=\"_blank\">
                 <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                 align=\"center\"  src=\"/hypoD3_zoom/bone/hypoD3_notif_bone2.svg\"/></a>
                 </div></div>"
          )
        )
      } else {
        paste(a("About bone", 
                href = bone_web,
                target = "_blank"))
      },
      
      # kidney tooltip
      paste(a("About Ca kidney handling", 
              href = Ca_kidney_web,
              target = "_blank"), br(),
            a("About PO4 kidney handling", 
              href = PO4_kidney_web,
              target = "_blank")),
      
      # kidney_zoom tooltip
      if (input$run_php1) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">", 
                "Detailed Ca PT reabsorption", "<hr>",
                "<a href=\"/php1_zoom/kidney/php1_notif_kidney1.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/php1_zoom/kidney/php1_notif_kidney1.svg\"/></a>", 
                "</div>", "<div class=\"col-sm-6\">",
                "Detailed Pi PT reabsorption", "<hr>",
                "<a href=\"/php1_zoom/kidney/php1_notif_kidney2.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/php1_zoom/kidney/php1_notif_kidney2.svg\"/></a>", 
                "</div>", "</div>", "<br>",
                "<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed Ca TAL reabsorption", "<hr>",
                "<a href=\"/php1_zoom/kidney/php1_notif_kidney3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/php1_zoom/kidney/php1_notif_kidney3.svg\"/></a>", 
                "</div>", "<div class=\"col-sm-6\">", 
                "Detailed Ca DCT reabsorption", "<hr>",
                "<a href=\"/php1_zoom/kidney/php1_notif_kidney4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/php1_zoom/kidney/php1_notif_kidney4.svg\"/></a>", 
                "</div>", "</div>"
          )
        )
      } else if (input$run_hypopara) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">", 
                "Detailed Ca PT reabsorption", "<hr>",
                "<a href=\"/hypopara_zoom/kidney/hypopara_notif_kidney1.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/hypopara_zoom/kidney/hypopara_notif_kidney1.svg\"/></a>", 
                "</div>", "<div class=\"col-sm-6\">", 
                "Detailed Pi PT reabsorption", "<hr>",
                "<a href=\"/hypopara_zoom/kidney/hypopara_notif_kidney2.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/hypopara_zoom/kidney/hypopara_notif_kidney2.svg\"/></a>", 
                "</div>", "</div>", "<br>",
                "<div class=\"row\">", "<div class=\"col-sm-6\">", 
                "Detailed Ca TAL reabsorption", "<hr>",
                "<a href=\"/hypopara_zoom/kidney/hypopara_notif_kidney3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/hypopara_zoom/kidney/hypopara_notif_kidney3.svg\"/></a>", 
                "</div>", "<div class=\"col-sm-6\">", 
                "Detailed Ca DCT reabsorption", "<hr>",
                "<a href=\"/hypopara_zoom/kidney/hypopara_notif_kidney4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/hypopara_zoom/kidney/hypopara_notif_kidney4.svg\"/></a>", 
                "</div>", "</div>"
          )
        )
      } else if (input$run_hypoD3) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">", 
                "Detailed Ca PT reabsorption", "<hr>",
                "<a href=\"/hypoD3_zoom/kidney/hypoD3_notif_kidney1.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/hypoD3_zoom/kidney/hypoD3_notif_kidney1.svg\"/></a>", 
                "</div>", "<div class=\"col-sm-6\">", 
                "Detailed Pi PT reabsorption", "<hr>",
                "<a href=\"/hypoD3_zoom/kidney/hypoD3_notif_kidney2.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/hypoD3_zoom/kidney/hypoD3_notif_kidney2.svg\"/></a>", 
                "</div>", "</div>", "<br>",
                "<div class=\"row\">", "<div class=\"col-sm-6\">", 
                "Detailed Ca TAL reabsorption", "<hr>",
                "<a href=\"/hypoD3_zoom/kidney/hypoD3_notif_kidney3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/hypoD3_zoom/kidney/hypoD3_notif_kidney3.svg\"/></a>", 
                "</div>", "<div class=\"col-sm-6\">", 
                "Detailed Ca DCT reabsorption", "<hr>",
                "<a href=\"/hypoD3_zoom/kidney/hypoD3_notif_kidney4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"/hypoD3_zoom/kidney/hypoD3_notif_kidney4.svg\"/></a>", 
                "</div>", "</div>"
          )
        )
      } else {
        ""
      },
      rep("",2),
      
      # calcium tooltip
      paste(a("About Calcium", 
              href = Ca_web,
              target = "_blank")),
      # PO4 tooltip
      paste(a("About Phosphate", 
              href = PO4_web,
              target = "_blank")),
      
      # PTH synthesis zoom
      if (input$run_php1) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed PTH mechanisms", "<hr>",
                "<a href=\"/php1_zoom/PTHg/php1_notif_PTHg1.svg\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                    align=\"center\"  src=\"/php1_zoom/PTHg/php1_notif_PTHg1.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Effect of D3 on PTH synthesis", "<hr>",
                "<a href=\"/php1_zoom/PTHg/php1_notif_PTHg2.svg\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"/php1_zoom/PTHg/php1_notif_PTHg2.svg\"/></a>",
                "</div>", "</div>", "<br>","<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Effect of Pi on PTH synthesis", "<hr>",
                "<a href=\"/php1_zoom/PTHg/php1_notif_PTHg3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"/php1_zoom/PTHg/php1_notif_PTHg3.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">", "Effect of Ca on PTH secretion", "<hr>",
                "<a href=\"php1_zoom/PTHg/php1_notif_PTHg4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"php1_zoom/PTHg/php1_notif_PTHg4.svg\"/></a>",
                "</div>", "</div>"
          )
        )
      } else if (input$run_hypopara) {
        HTML(
          paste("<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Detailed PTH mechanisms", "<hr>",
                "<a href=\"/hypopara_zoom/PTHg/hypopara_notif_PTHg1.svg\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                    align=\"center\"  src=\"/hypopara_zoom/PTHg/hypopara_notif_PTHg1.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">",
                "Effect of D3 on PTH synthesis", "<hr>",
                "<a href=\"/hypopara_zoom/PTHg/hypopara_notif_PTHg2.svg\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"/hypopara_zoom/PTHg/hypopara_notif_PTHg2.svg\"/></a>",
                "</div>", "</div>", "<br>","<div class=\"row\">", "<div class=\"col-sm-6\">",
                "Effect of Pi on PTH synthesis", "<hr>",
                "<a href=\"/hypopara_zoom/PTHg/hypopara_notif_PTHg3.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"/hypopara_zoom/PTHg/hypopara_notif_PTHg3.svg\"/></a>",
                "</div>", "<div class=\"col-sm-6\">", "Effect of Ca on PTH secretion", "<hr>",
                "<a href=\"hypopara_zoom/PTHg/hypopara_notif_PTHg4.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"hypopara_zoom/PTHg/hypopara_notif_PTHg4.svg\"/></a>",
                "</div>", "</div>"
          )
        )
      } else if (input$run_hypoD3) {
        HTML(
          paste("Detailed PTH mechanisms <hr>
                   <a href=\"/hypoD3_zoom/PTHg/hypoD3_notif_PTHg1.svg\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
                   align=\"center\"  src=\"/hypoD3_zoom/PTHg/hypoD3_notif_PTHg1.svg\"/></a>"
          )
        )
      } else {
        paste(a("About PTH", 
                href = PTH_web, 
                target = "_blank"))
      },
      # PTH tooltip
      paste(a("About PTH", 
              href = PTH_web, 
              target = "_blank")),
      # D3 tooltip
      paste(a("About vitamin D3", 
              href = D3_web,
              target = "_blank")),
      # D3 tooltip
      paste(a("About vitamin D3", 
              href = D3_web,
              target = "_blank")),
      # D3 tooltip
      paste(a("About vitamin D3", 
              href = D3_web,
              target = "_blank")),
      # FGF23 tooltip
      paste(a("About FGF23", 
              href = FGF23_web,
              target = "_blank"))
    ),
    
    x = if (is.null(input$background_choice)) {
          c(38,-65,-65,-256,180,360,170,-190,290,320,41,-418,330,385,-386,481)
        } else if (input$background_choice == "rat") {
          c(38,-65,-65,-256,180,360,170,-190,290,320,41,-418,330,385,-386,481)
        } else {
          c(13,-80,-185,-322,157,333,7,-175,290,320,9,-466,330,385,-386,481)
        },
    
    y = if (is.null(input$background_choice)) {
          c(-150,195,472,460,0,230,506,0,-317,-633,-452,240,-452,0,-106,-452)
        } else if (input$background_choice == "rat") {
          c(-150,195,472,460,0,230,506,0,-317,-633,-452,240,-452,0,-106,-452)
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
      )
  )
}

# Generate edges for the CaPO4 network
ac_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=24281&Menu=1079&backbar=0"
res_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=25370&Menu=1079&backbar=0"
CaSR_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23415&Menu=1079&backbar=0"

generate_edges_Ca <- function(input) {
  req(input$width_organs, input$width_hormones)
  data.frame(
    from = c(1, 
             # change the from direction depending on the net flux result
             if (input$run_php1) {
                2
             } else if (input$run_hypopara) {
              3
             } else if (input$run_hypoD3) {
              3
            } else {
              2
            },
            if (input$run_php1) {
              2
            } else if (input$run_hypopara) {
              3
            } else if (input$run_hypoD3) {
              3
            } else {
              2
            },
            rep(3,2),4,2,rep(5,2),rep(5,2),
            if (input$run_php1) {
              2
            } else if (input$run_hypopara) {
              8
            } else if (input$run_hypoD3) {
              8
            } else {
              2
            },
            rep(9,3),rep(10,3),rep(11,2),11,rep(13,2),rep(14,2),rep(15,2),
            rep(16,2)), 
    to = c(2,
           if (input$run_php1) {
             3
           } else if (input$run_hypopara) {
             2
           } else if (input$run_hypoD3) {
             2
           } else {
             3
           },
           if (input$run_php1) {
             3
           } else if (input$run_hypopara) {
             2
           } else if (input$run_hypoD3) {
             2
           } else {
             3
           },
           rep(4,2),2,5,rep(2,2),rep(7,2),
           if (input$run_php1) {
             8
           } else if (input$run_hypopara) {
             2
           } else if (input$run_hypoD3) {
             2
           } else {
             8
           },
           11,5,
           13,11,13,16,5,13,4,11,16,14,5,4,1,13,5),
    
    arrows = list(
      to = list(
        enabled = c(
          TRUE, 
          # show or hode arrow symbol depending on the net flux result
          rep(if (input$run_php1 | input$run_hypopara | input$run_hypoD3) TRUE else FALSE, 2), 
          rep(TRUE,8), 
          if (input$run_php1 | input$run_hypopara | input$run_hypoD3) TRUE else FALSE, 
          rep(TRUE,17)
        ), 
      scaleFactor = 1, 
      type = "arrow")
    ),
    
    label = c("","Net Ca","Net PO4","Ca","PO4",rep("",2),"Ca","PO4",
              "Ca","PO4", "Net PO4",rep("-",3),"+","-","+",
              "","+","+","-","+","-","+","+","+","-","-"),
    id = c("Abs_int","Net_Ca_pf","Net_PO4_pf","Ac_Ca","Ac_PO4","Res",7,"Reabs_Ca",
           "Reabs_PO4","U_Ca","U_PO4","Net_PO4_cells",13:29),
    width = c(rep(input$width_organs,12), rep(input$width_hormones,17)),
    font.size = c(rep(25,12),rep(60,17)),
    font.align = c("", if (!input$run_hypopara | !input$run_hypoD3) "bottom" else "top", 
                   if (!input$run_hypopara | !input$run_hypoD3) "bottom" else "top",
                   "top","bottom",rep("",4),"bottom",
                   "top","bottom","bottom",rep("top",2),"top","top",
                   "top","","bottom","top","top","bottom","bottom","top","top",
                   rep("top",2),"bottom"),
    color = list(color = c(rep("black", 29)), 
                 highlight = "yellow"),
    dashes = c(rep(FALSE,12), rep(TRUE,17)),
    title = c(rep("",3),
              paste(a("About bone formation", 
                      href = ac_web ,
                      target = "_blank")),
              paste(a("About bone formation", 
                      href = ac_web,
                      target = "_blank")),
              paste(a("About bone resorption", 
                      href = res_web,
                      target = "_blank")),
              rep("",6),
              paste(a("About the Calcium Sensing Receptor", 
                      href = CaSR_web,
                      target = "_blank")),
              paste(a("About the Calcium Sensing Receptor", 
                      href = CaSR_web,
                      target = "_blank")),
              rep("",15)),
    smooth = c(rep(TRUE,29)),
    length = c(200,rep(300,2),rep(300,2),200,300,
               200,rep(300,4),rep(200,8), 1800, rep(200,8)),
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
    stringsAsFactors = FALSE) 
  
}