#------------------------------------------------------------------------- 
#  This codes contains all network skeleton for CaPO4, PTH, ...
#  For each network, we define two dataframe: node contains all informations
#  related to nodes and edges to edges...
#
#-------------------------------------------------------------------------


# % % % % % % % %  #
#                  #
#  Generate Graph  #
#                  #
# % % % % % % % %  #

# This function is used to generate a graph as well
# as basic options such as physics, manipulations,
# selection

generate_network <- function(nodes, edges, css_export) {
  
  visNetwork(nodes, 
             edges, 
             width = "100%", 
             height = "100%") %>%
    visNodes(shapeProperties = list(useBorderWithImage = FALSE)) %>%
    # put shadow on false
    visEdges(shadow = FALSE, 
             font = list(align = "horizontal"), 
             arrows = list(to = list(enabled = TRUE, 
                                     scaleFactor = 1, 
                                     type = "arrow"))) %>%
    # add group selection option
    visOptions(highlightNearest = FALSE, 
               clickToUse = FALSE, 
               manipulation = FALSE, 
               collapse = FALSE) %>% 
    # prevent edge from being selected when a node is selected
    visInteraction(hover = TRUE, 
                   hoverConnectedEdges = FALSE, 
                   selectConnectedEdges = FALSE, 
                   multiselect = FALSE, 
                   dragNodes = FALSE, 
                   dragView = FALSE, 
                   zoomView = FALSE,
                   navigationButtons = FALSE) %>% 
    # stabilization prevents arrows from bouncing
    visPhysics(stabilization = TRUE, enabled = TRUE) %>%
    # export the graph as pdf
    visExport(type = "pdf", style = css_export)
  
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

generate_nodes_Ca <- function() {
  
  data.frame(
    id = 1:21,
    shape = c("image","image","image","image","image","image","image","image",
              "circle","circle","circle","image","image","image","image","image",
              "image","image","image","image","image"), 
    image = c("food.svg","intestine.svg","feces.svg","plasma.svg","rapid-bone.svg",
              "bone.svg","kidney.svg","kidney_zoom1.svg", rep("",3),"urine.svg",
              "cells.svg","Cap.svg","PO4.svg","parathyroid_gland.svg","PTH.svg",
              "D3.svg","D3.svg","D3.svg","FGF23.svg"),
    label = c(rep("", 8),"PT","TAL","DCT", rep("",10)),
    fixed = list("x" = TRUE, "y" = TRUE),
    title = c("", 
              paste(a("About intestinal Ca absorption", 
                      href = Ca_int_web,
                      target="_blank"),br(),
                    a("About intestinal PO4 absorption", 
                      href = PO4_int_web,
                      target="_blank")),
              rep("",2),
              paste(a("About rapid bone pool", 
                      href = rapid_bone_web,
                      target="_blank")),
              paste(a("About bone", 
                      href = bone_web,
                      target="_blank")),
              paste(a("About Ca kidney handling", 
                      href = Ca_kidney_web,
                      target="_blank"), br(),
                    a("About PO4 kidney handling", 
                      href = PO4_kidney_web,
                      target="_blank")),
              rep("",6),
              paste(a("About Calcium", 
                      href = Ca_web,
                      target="_blank")),
              paste(a("About Phosphate", 
                      href = PO4_web,
                      target="_blank")),
              paste(a("About PTH", 
                      href = PTH_web, 
                      target="_blank")),
              paste(a("About PTH", 
                      href = PTH_web, 
                      target="_blank")),
              paste(a("About vitamin D3", 
                      href = D3_web,
                      target="_blank")),
              paste(a("About vitamin D3", 
                      href = D3_web,
                      target="_blank")),
              paste(a("About vitamin D3", 
                      href = D3_web,
                      target="_blank")),
              paste(a("About FGF23", 
                      href = FGF23_web,
                      target="_blank"))), # tooltip to display an image
    x = c(3,38,64,-65,-65,-256,180,360,380,450,430,170,-190,290,320,41,-418,330,385,-386,481),
    y = c(-633,-150,551,195,472,460,0,230,110,250,130,506,0,-317,-633,-452,240,-452,0,-106,-452),
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(rep(60,7), 150, rep(60,5), rep(25,2), 60, rep(25,5)),
    #fixed = list("x" = TRUE, "y" = TRUE),
    physics = rep(FALSE,21),
    hidden = c(rep(FALSE,21)))
  
}


# Generate edges for the CaPO4 network
ac_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=24281&Menu=1079&backbar=0"
res_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=25370&Menu=1079&backbar=0"
CaSR_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23415&Menu=1079&backbar=0"

generate_edges_Ca <- function(input) {
  
  data.frame(
    from = c(1,rep(2,2),rep(4,2),rep(5,2),rep(5,2),6,4,rep(7,2),rep(7,2),4,13,
             rep(14,3),rep(15,3),rep(16,2),17,rep(18,2),rep(19,2),rep(20,2),
             rep(21,2)), 
    to = c(2,4,3,rep(5,2),rep(4,2),rep(6,2),4,7,rep(4,2),rep(12,2),13,4,16,7,
           18,16,18,21,7,18,6,16,21,19,7,6,2,18,7),
    label = c(rep("",3),"Ca","PO4","Ca","PO4","Ca","PO4",rep("",2),"Ca","PO4",
              "Ca","PO4", rep("PO4",2),rep("-",3),"+","-","+",
              "","+","+","-","+","-","-","+","+","-","-"),
    id = 1:34,
    width = c(rep(8,17), rep(4,17)),
    font.size = c(rep(25,17),rep(60,17)),
    font.align = c(rep("",5),"bottom","top","top","bottom",rep("",4),"bottom",
                   "top","bottom","top","bottom",rep("top",2),"top","top",
                   "top","",rep("bottom",2),"top","bottom","bottom","top","top",
                   rep("top",2),"bottom"),
    color = list(color = c(rep("black", 34)), 
                 highlight = "yellow"),
    dashes = c(rep(FALSE,17), rep(TRUE,17)),
    title = c(rep("",7),
              paste(a("About bone formation", 
                      href = ac_web ,
                      target="_blank")),
              paste(a("About bone formation", 
                      href = ac_web,
                      target="_blank")),
              paste(a("About bone resorption", 
                      href = res_web,
                      target="_blank")),
              rep("",7),
              paste(a("About the Calcium Sensing Receptor", 
                      href = CaSR_web,
                      target="_blank")),
              paste(a("About the Calcium Sensing Receptor", 
                      href = CaSR_web,
                      target="_blank")),
              rep("",15)),
    smooth = c(rep(TRUE,34)),
    length = c(rep(200,3),rep(300,4),rep(300,2),200,300,200,rep(300,5),rep(200,17)),
    # to show either Ca or PO4 or CaPO4 network arrows
    hidden = c(rep(FALSE,3), 
               ifelse(is.element("Ca", input$network_Ca_choice), 
                      ifelse(is.element("PO4", input$network_Ca_choice),FALSE,FALSE),TRUE), 
               ifelse(is.element("PO4", input$network_Ca_choice), 
                      ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
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
                          ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),3),
               rep(FALSE,17)), 
    stringsAsFactors=FALSE) 
  
}

# % % % % #
#         #
#   PTH   #
#         #
# % % % % #

# generate PTHg nodes
generate_nodes_PTHg <- function() {
  
  data.frame(
    id = 1:5,
    shape = c("circle","circle","circle","circle","circle"), 
    label = c("","PTHg","","",""),
    x = c(-50,81,51,184,109), 
    y = c(-6,-6,119,-106,-165), 
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(10,10,10,10,10), 
    #fixed = list("x" = TRUE, "y" = TRUE),
    hidden = c(TRUE,FALSE,TRUE,TRUE,TRUE))
  
}


# generate PTHg edges
generate_edges_PTHg <- function() {
  
  data.frame(
    from = c(1,2,2,5), 
    to = c(2,3,4,4),
    label = c("PTHg synthesis", "PTHg degradation", 
              "PTHg exocytosis", "CaSR inhibition"),
    id = 1:4,
    width = 4,
    font.size = 12,
    color = list(color = c(rep("black", 4)), 
                 highlight = "yellow"),
    dashes = c(rep(FALSE,3),TRUE),
    smooth = c(rep(TRUE,4)),
    hidden = rep(FALSE,4),
    stringsAsFactors=FALSE)
  
}