#------------------------------------------------------------------------- 
#  This codes contains all network skeletons for CaPO4, PTH, ...
#  For each network, we define two dataframe: node contains all informations
#  related to nodes and edges to edges...
#
#-------------------------------------------------------------------------


# This function is used to generate a network as well
# as basic options such as physics, manipulations,
# selection

generate_network <- function(nodes, edges, usephysics = FALSE) {
  
  visNetwork(nodes, 
             edges, 
             width = "100%", 
             height = "100%") %>%
    visNodes(shapeProperties = list(useBorderWithImage = FALSE,
                                    interpolation = FALSE)) %>%
    # put shadow on false
    visEdges(shadow = FALSE, 
             font = list(align = "horizontal")) %>%
    # add group selection option
    visOptions(highlightNearest = FALSE, 
               clickToUse = FALSE, 
               manipulation = FALSE, 
               collapse = FALSE,
               autoResize = TRUE) %>% 
    # prevent edge from being selected when a node is selected
    visInteraction(hover = TRUE, 
                   hoverConnectedEdges = FALSE, 
                   selectConnectedEdges = FALSE, 
                   multiselect = FALSE, 
                   dragNodes = FALSE,
                   dragView = FALSE, 
                   zoomView = FALSE,
                   navigationButtons = FALSE,
                   selectable = TRUE,
                   tooltipStyle = 'position: fixed;
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
    shape = c("image","image","image","image","image","image", "image","image",
              ifelse(input$network_hormonal_choice == "TRUE","image","text"),
              ifelse(input$network_hormonal_choice == "TRUE","image","text"),
              "image",
              ifelse(input$network_hormonal_choice == "TRUE","image","text"),
              ifelse(input$network_hormonal_choice == "TRUE","image","text"),
              ifelse(input$network_hormonal_choice == "TRUE","image","text"),
              ifelse(input$network_hormonal_choice == "TRUE","image","text"),
              ifelse(input$network_hormonal_choice == "TRUE","image","text")), 
    image = c("intestine.svg","plasma.svg","rapid-bone.svg",
              "bone.svg","kidney.svg","kidney_zoom1.svg","urine.svg",
              "cells.svg","Cap.svg","PO4.svg",
              if (is.null(input$background_choice)) {
                "parathyroid_gland.svg"
              } else if (input$background_choice == "rat") {
                "parathyroid_gland.svg"
              } else {
                "parathyroid_gland_human.svg"
              }
              ,"PTH.svg", "D3.svg","D3.svg","D3.svg","FGF23.svg"),
    label = c(rep("", 6), rep("",10)),
    fixed = list("x" = TRUE, "y" = TRUE),
    
    # tooltip to display an image
    title = c(
        # intestinal absorption tooltip
        if (input$run_php1) {
          HTML(paste("<a href=\"php1_notif_intestine.png\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_intestine.png\"/></a>"
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
        HTML(paste("<a href=\"php1_notif_bone.png\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_bone.png\"/></a>"
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
        HTML(paste("<div class=\"row\">", "<div class=\"col-sm-6\">", 
                   "<a href=\"php1_notif_Ca_PTreab.png\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_Ca_PTreab.png\"/></a>", 
                  "</div>", "<div class=\"col-sm-6\">", 
                   "<a href=\"php1_notif_PO4_PTreab.png\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_PO4_PTreab.png\"/></a>", 
                   "</div>", "</div>", "<br>",
                   "<div class=\"row\">", "<div class=\"col-sm-6\">", 
                   "<a href=\"php1_notif_Ca_TALreab.png\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_Ca_TALreab.png\"/></a>", 
                   "</div>", "<div class=\"col-sm-6\">", 
                   "<a href=\"php1_notif_Ca_DCTreab.png\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_Ca_DCTreab.png\"/></a>", 
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
        HTML(paste("<div class=\"row\">", "<div class=\"col-sm-6\">", 
                    "<a href=\"php1_notif_1-2.png\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                    align=\"center\"  src=\"php1_notif_1-2.png\"/></a>", 
                    "</div>", "<div class=\"col-sm-6\">",
                   "<a href=\"php1_notif_PTHgD3inhib.png\" target=\"_blank\">
                    <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_PTHgD3inhib.png\"/></a>", 
                   "</div>", "</div>", "<br>","<div class=\"row\">", "<div class=\"col-sm-6\">", 
                   "<a href=\"php1_notif_PTHgPO4activ.png\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_PTHgPO4activ.png\"/></a>", 
                   "</div>", "<div class=\"col-sm-6\">", 
                  "<a href=\"php1_notif_PTHgCainhib.png\" target=\"_blank\">
                   <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\" 
                   align=\"center\"  src=\"php1_notif_PTHgCainhib.png\"/></a>", 
                   "</div>", "</div>"
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
      rep(FALSE,7),
      # PO4 Cells
      ifelse(is.element("PO4", input$network_Ca_choice), 
             ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
      ## Hormones ##
      # Ca plasma
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("Ca", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PO4 plasma
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("PO4", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PTHg
      ifelse(input$network_hormonal_choice,
      ifelse(is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE),
      
      # PTH plasma
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # D3 plasma
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("D3", input$network_Ca_choice) & 
                      is.element(c("Ca", "PTH", "PO4", "FGF23"), input$network_Ca_choice), FALSE, TRUE), TRUE),
      ifelse(input$network_hormonal_choice,
                 ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
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
    from = c(1,2,3,rep(3,2),4,2,rep(5,2),rep(5,2),8,
             rep(9,3),rep(10,3),rep(11,2),12,rep(13,2),rep(14,2),rep(15,2),
             rep(16,2)), 
    to = c(2,3,2,rep(4,2),2,5,rep(2,2),rep(7,2),2,11,5,
           13,11,13,16,5,13,4,11,16,14,5,4,1,13,5),
    
    arrows = list(to = list(enabled = c(TRUE, rep(FALSE,2), rep(TRUE,8), FALSE, rep(TRUE,17)), 
                            scaleFactor = 1, 
                            type = "arrow")),
    
    label = c("","Net Ca","Net PO4","Ca","PO4",rep("",2),"Ca","PO4",
              "Ca","PO4", "Net PO4",rep("-",3),"+","-","+",
              "","+","+","-","+","-","+","+","+","-","-"),
    id = c("Abs_int","Net_Ca_pf","Net_PO4_pf","Ac_Ca","Ac_PO4","Res",7,"Reabs_Ca",
           "Reabs_PO4","U_Ca","U_PO4","Net_PO4_cells",13:29),
    width = c(rep(input$width_organs,12), rep(input$width_hormones,17)),
    font.size = c(rep(25,12),rep(60,17)),
    font.align = c("","top","bottom","top","bottom",rep("",4),"bottom",
                   "top","bottom","bottom",rep("top",2),"top","top",
                   "top","",rep("bottom",2),"top","bottom","bottom","top","top",
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
    length = c(200,rep(300,2),rep(300,2),200,300,200,rep(300,4),rep(200,17)),
    # to show either Ca or PO4 or CaPO4 network arrows
    hidden = c(
      ## organ arrows ##
      FALSE, 
      ifelse(is.element("Ca", input$network_Ca_choice), 
             ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE), 
      ifelse(is.element("PO4", input$network_Ca_choice), 
             ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
      ifelse(is.element("Ca", input$network_Ca_choice), 
             ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE),
      ifelse(is.element("PO4", input$network_Ca_choice), 
             ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
      rep(FALSE,2),
      ifelse(is.element("Ca", input$network_Ca_choice), 
             ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE),
      ifelse(is.element("PO4", input$network_Ca_choice), 
             ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
      ifelse(is.element("Ca", input$network_Ca_choice), 
             ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE),
      rep(ifelse(is.element("PO4", input$network_Ca_choice), 
                 ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),2),
      ## hormonal regulations arrows ##
      # Ca regulation to PTH
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("PTH", input$network_Ca_choice) & 
                      is.element("Ca", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # Ca to Kidney
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("Ca", input$network_Ca_choice), FALSE, TRUE), TRUE),
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
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PTH regulation to D3
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("D3", input$network_Ca_choice) & 
                      is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # PTH regulation to bone
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("PTH", input$network_Ca_choice), FALSE, TRUE), TRUE),
      
      
      # D3 regulation to PTH
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("PTH", input$network_Ca_choice) & 
                      is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # D3 regulation to FGF23
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("FGF23", input$network_Ca_choice) & 
                      is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # D3 regulation to D3
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # D3 regulation to kidney
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # D3 regulation to bone
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # D3 regulation to intestine
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("D3", input$network_Ca_choice), FALSE, TRUE), TRUE),
      
      
      # FGF regulation to D3
      ifelse(input$network_hormonal_choice,
             ifelse(is.element("D3", input$network_Ca_choice) & 
                      is.element("FGF23", input$network_Ca_choice), FALSE, TRUE), TRUE),
      # FGF regulation to kidney
      ifelse(input$network_hormonal_choice, 
             ifelse(is.element("FGF23", input$network_Ca_choice), FALSE, TRUE), TRUE)

      ), 
    stringsAsFactors = FALSE) 
  
}