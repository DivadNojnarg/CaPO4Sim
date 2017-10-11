#------------------------------------------------------------------------- 
#  This codes contains all network skeletons for CaPO4, PTH, ...
#  For each network, we define two dataframe: node contains all informations
#  related to nodes and edges to edges...
#
#-------------------------------------------------------------------------


# % % % % % % % %  #
#                  #
#  Generate Graph  #
#                  #
# % % % % % % % %  #

# This function wrap the generate_network function to
# build all the networks needed for this app
# Its takes nodes, edges, physics as well as the number
# of networks to build, namely n_networks
# For instance there are 8 networks

all_networks <- list(
  names = c("Ca","PTHg","kidney_zoom2","kidney_PT",
           "kidney_TAL","kidney_DCT","intestine","bone"),
  nodes = c("nodes_Ca","nodes_PTHg","nodes_kidney_zoom2","nodes_kidney_PT",
            "nodes_kidney_TAL","nodes_kidney_DCT","nodes_intestine","nodes_bone"),
  edges = c("edges_Ca","edges_PTHg","edges_kidney_zoom2","edges_kidney_PT",
            "edges_kidney_TAL","edges_kidney_DCT","edges_intestine","edges_bone"),
  generate_nodes = c(),
  generate_edges = c()
  )

build_all_networks <- function(nodes, edges, usephysics = FALSE, 
                               n_network = all_networks) {
  for (i in seq_along(all_networks$nodes)) {
    if (str_detect(string = all_networks$nodes[i], pattern = "Ca") == TRUE) {
     parse(text = all_networks[i]) <- reactive({ 
       generate_node_parse(text = all_networks[i])(input)
       })
     
     edges_parse(text = all_networks[i]) <- reactive({
       generate_edge_parse(text = all_networks[i])(input)
     })
    } else {
      nodes_parse(text = all_networks[i]) <- reactive({ 
        generate_node_parse(text = all_networks[i])()
      })
      
      edges_parse(text = all_networks[i]) <- reactive({
        generate_edge_parse(text = all_networks[i])()
      })
    }
    
  }
}

# This function is used to generate a network as well
# as basic options such as physics, manipulations,
# selection

generate_network <- function(nodes, edges, usephysics = FALSE) {
  
  visNetwork(nodes, 
             edges, 
             width = "100%", 
             height = "100%") %>%
    visNodes(shapeProperties = list(useBorderWithImage = FALSE)) %>%
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
                   selectable = TRUE) %>% 
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
              "cells.svg","Cap.svg","PO4.svg","parathyroid_gland.svg","PTH.svg",
              "D3.svg","D3.svg","D3.svg","FGF23.svg"),
    label = c(rep("", 6), rep("",10)),
    fixed = list("x" = TRUE, "y" = TRUE),
    title = c(paste(a("About intestinal Ca absorption", 
                      href = Ca_int_web,
                      target="_blank"),br(),
                    a("About intestinal PO4 absorption", 
                      href = PO4_int_web,
                      target="_blank")),
              "",
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
              rep("",3),
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
    x = c(38,-65,-65,-256,180,360,170,-190,290,320,41,-418,330,385,-386,481),
    y = c(-150,195,472,460,0,230,506,0,-317,-633,-452,240,-452,0,-106,-452),
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(rep(60,5), 150, rep(60,2), rep(25,2), 60, rep(25,5)),
    #fixed = list("x" = TRUE, "y" = TRUE),
    physics = rep(FALSE,16),
    hidden = c(rep(FALSE,7),
               ifelse(is.element("PO4", input$network_Ca_choice), 
                      ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
               FALSE,
               ifelse(is.element("PO4", input$network_Ca_choice), 
                      ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE),
               rep(FALSE,5),
               ifelse(is.element("PO4", input$network_Ca_choice), 
                      ifelse(is.element("Ca", input$network_Ca_choice),FALSE,FALSE),TRUE)))
  
}

# Generate edges for the CaPO4 network
ac_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=24281&Menu=1079&backbar=0"
res_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=25370&Menu=1079&backbar=0"
CaSR_web <- "https://kidneynccr.bio-med.ch/cms/Default.aspx?Page=23415&Menu=1079&backbar=0"

generate_edges_Ca <- function(input) {
  
  data.frame(
    from = c(1,2,3,rep(3,2),4,2,rep(5,2),rep(5,2),8,
             rep(9,3),rep(10,3),rep(11,2),12,rep(13,2),rep(14,2),rep(15,2),
             rep(16,2)), 
    to = c(2,3,2,rep(4,2),2,5,rep(2,2),rep(7,2),2,11,5,
           13,11,13,16,5,13,4,11,16,14,5,4,1,13,5),
    
    arrows = list(to = list(enabled = c(TRUE, rep(FALSE,2), rep(TRUE,8), FALSE, rep(TRUE,17)), 
                            scaleFactor = 1, 
                            type = "arrow")),
    
    label = c("","Net Flux","","Ca","PO4",rep("",2),"Ca","PO4",
              "Ca","PO4", "Net Flux",rep("-",3),"+","-","+",
              "","+","+","-","+","-","-","+","+","-","-"),
    id = c("Abs_int","Net_Ca_pf","Net_PO4_pf","Ac_Ca","Ac_PO4","Res",7,"Reabs_Ca",
           "Reabs_PO4","U_Ca","U_PO4","Net_PO4_cells",13:29),
    width = c(rep(8,12), rep(4,17)),
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
                      target="_blank")),
              paste(a("About bone formation", 
                      href = ac_web,
                      target="_blank")),
              paste(a("About bone resorption", 
                      href = res_web,
                      target="_blank")),
              rep("",6),
              paste(a("About the Calcium Sensing Receptor", 
                      href = CaSR_web,
                      target="_blank")),
              paste(a("About the Calcium Sensing Receptor", 
                      href = CaSR_web,
                      target="_blank")),
              rep("",15)),
    smooth = c(rep(TRUE,29)),
    length = c(200,rep(300,2),rep(300,2),200,300,200,rep(300,4),rep(200,17)),
    # to show either Ca or PO4 or CaPO4 network arrows
    hidden = c(FALSE, 
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
               rep(ifelse(input$network_hormonal_choice == "TRUE", FALSE, TRUE),3),
               rep(ifelse(input$network_hormonal_choice == "TRUE", 
                          ifelse(is.element("PO4", input$network_Ca_choice), FALSE, TRUE), 
                          TRUE),3),
               rep(ifelse(input$network_hormonal_choice == "TRUE", FALSE, TRUE),4),
               ifelse(input$network_hormonal_choice == "TRUE", 
                      ifelse(is.element("PO4", input$network_Ca_choice), FALSE, TRUE), 
                      TRUE),
               rep(ifelse(input$network_hormonal_choice == "TRUE", FALSE, TRUE),4),
               rep(ifelse(input$network_hormonal_choice == "TRUE", 
                          ifelse(is.element("PO4", input$network_Ca_choice), FALSE, TRUE), 
                          TRUE),2)), 
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
    id = 1:8,
    shape = c(rep("circle",8)), 
    label = c("","PTHg","","","","","",""),
    x = c(-50,71,51,173,78,8,3,-81), 
    y = c(-6,-6,119,-6,-93,-140,-11,-132), 
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(10,10,10,10,10,10,10,10), 
    fixed = list("x" = TRUE, "y" = TRUE),
    hidden = c(TRUE,FALSE,rep(TRUE,6)))
  
}

# generate PTHg edges
generate_edges_PTHg <- function() {
  
  data.frame(
    from = c(1,2,2,5,6,8), 
    to = c(2,3,4,4,7,7),
    arrows = list(to = list(enabled = TRUE, 
                            scaleFactor = 1, 
                            type = "arrow")),
    label = c("synthesis", "degradation", 
              "exocytosis", "-","+","-"),
    id = 1:6,
    width = 4,
    font.size = c(rep(10,3),rep(32,3)),
    font.align = c("bottom","top","bottom","bottom","bottom","top"),
    color = list(color = c(rep("black", 6)), 
                 highlight = "yellow"),
    dashes = c(rep(FALSE,3),rep(TRUE,3)),
    smooth = rep(TRUE,6),
    hidden = rep(FALSE,6),
    stringsAsFactors=FALSE)
  
}


# % % % % % % % % %  #
#                    #
#   Kidney_zoom_2    #
#                    #
# % % % % % % % % %  #

# generate kidney_zoom2 nodes
generate_nodes_kidney_zoom2 <- function() {
  
  data.frame(
    id = 1:3,
    shape = c(rep("text", 3)), 
    label = c(rep("", 3)),
    x = c(-125, -2, 34), 
    y = c(-35, 57, -135), 
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(10,10,10), 
    #fixed = list("x" = TRUE, "y" = TRUE),
    hidden = c(FALSE, FALSE, FALSE))
  
}

# generate kidney_zoom2 edges
generate_edges_kidney_zoom2 <- function() {
  
  data.frame()
  
}

# % % % % % % % % %  #
#                    #
#   Kidney_PT        #
#                    #
# % % % % % % % % %  #

# generate kidney_PT nodes
generate_nodes_kidney_PT <- function() {
  
  data.frame(
    id = 1:4,
    shape = c(rep("text", 4)), 
    label = c(rep("",4)),
    x = c(126,-39,-152,-117), 
    y = c(49,46,-54,-107), 
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(10,10,10,10), 
    #fixed = list("x" = TRUE, "y" = TRUE),
    hidden = c(FALSE, FALSE, FALSE,FALSE))
  
}

# generate kidney_PT edges
generate_edges_kidney_PT <- function() {
  
  data.frame(
    from = c(1,3), 
    to = c(2,4),
    arrows = list(to = list(enabled = TRUE, 
                            scaleFactor = 1, 
                            type = "arrow")),
    label = c(rep("", 2)),
    id = 1:2,
    width = 4,
    font.size = 12,
    color = list(color = c(rep("black", 2)), 
                 highlight = "yellow"),
    dashes = rep(TRUE,2),
    smooth = rep(TRUE,2),
    hidden = rep(FALSE,2),
    stringsAsFactors=FALSE)
  
}

# % % % % % % % % %  #
#                    #
#   Kidney_TAL        #
#                    #
# % % % % % % % % %  #

# generate kidney_TAL nodes
generate_nodes_kidney_TAL <- function() {
  
  data.frame(
    id = 1:5,
    shape = c(rep("text",5)), 
    label = c(rep("",5)),
    x = c(107,69,22,-48, -71), 
    y = c(-100,-155,-95,-104,-175), 
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(10,10,10,10,10), 
    #fixed = list("x" = TRUE, "y" = TRUE),
    hidden = c(FALSE, FALSE,FALSE,FALSE,FALSE))
  
}

# generate kidney_TAL edges
generate_edges_kidney_TAL <- function() {
  
  data.frame(
    from = c(1,2,4), 
    to = c(3,5,5),
    arrows = list(to = list(enabled = TRUE, 
                            scaleFactor = 1, 
                            type = "arrow")),
    label = c(rep("", 3)),
    id = 1:3,
    width = 4,
    font.size = 12,
    color = list(color = c(rep("black", 3)), 
                 highlight = "yellow"),
    dashes = rep(TRUE,3),
    smooth = rep(TRUE,3),
    hidden = rep(FALSE,3),
    stringsAsFactors=FALSE)
  
}

# % % % % % % % % %  #
#                    #
#   Kidney_DCT        #
#                    #
# % % % % % % % % %  #

# generate kidney_DCT nodes
generate_nodes_kidney_DCT <- function() {
  
  data.frame(
    id = 1:13,
    shape = c(rep("text",13)), 
    label = c(rep("",13)),
    x = c(74,158,16,12,8,35,85,-25,-72,-32,-73,-14,-52), 
    y = c(-135,82,85,-71,-22,-32,25,-62,-63,-40,-36,-26,25), 
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(rep(10,13)), 
    #fixed = list("x" = TRUE, "y" = TRUE),
    hidden = rep(FALSE,13))
  
}

# generate kidney_DCT edges
generate_edges_kidney_DCT <- function() {
  
  data.frame(
    from = c(1,6,8,12,2,3,10), 
    to = c(4,7,9,13,3,5,11),
    arrows = list(to = list(enabled = TRUE, 
                            scaleFactor = 1, 
                            type = "arrow")),
    label = c(rep("", 7)),
    id = 1:7,
    width = 4,
    font.size = 12,
    color = list(color = c(rep("black", 7)), 
                 highlight = "yellow"),
    dashes = c(rep(TRUE,4), rep(FALSE,3)),
    smooth = rep(TRUE,7),
    hidden = rep(FALSE,7),
    stringsAsFactors=FALSE)
  
}

# % % % % % % % % %  #
#                    #
#   Intestine        #
#                    #
# % % % % % % % % %  #

# generate intestine nodes
generate_nodes_intestine <- function() {
  
  data.frame(
    id = 1:9,
    shape = c(rep("text",9)), 
    label = c(rep("",9)),
    x = c(263,165,142,206,206,83,55,31,59), 
    y = c(358,357,264,238,295,263,186,249,307), 
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(rep(10,9)), 
    #fixed = list("x" = TRUE, "y" = TRUE),
    hidden = rep(FALSE,9))
  
}

# generate intestine edges
generate_edges_intestine <- function() {
  
  data.frame(
    from = c(1,2,rep(3,2),rep(6,3)), 
    to = c(2,3,4,5,7,8,9),
    arrows = list(to = list(enabled = TRUE, 
                            scaleFactor = 1, 
                            type = "arrow")),
    label = c(rep("", 7)),
    id = 1:7,
    width = 4,
    font.size = 12,
    color = list(color = c(rep("black", 7)), 
                 highlight = "yellow"),
    dashes = rep(TRUE,7),
    smooth = rep(TRUE,7),
    hidden = rep(FALSE,7),
    stringsAsFactors=FALSE)
  
}

# % % % % % % % % %  #
#                    #
#       Bone         #
#                    #
# % % % % % % % % %  #

# generate bone nodes
generate_nodes_bone <- function() {
  
  data.frame(
    id = 1:11,
    shape = c(rep("text",11)), 
    label = c(rep("",11)),
    x = c(-188,-179,-145,-168,-144,-119,-58,-61,-36,-20,-51), 
    y = c(49,112,112,5,17,27,37,121,200,-10,123), 
    color = list(background = "#97C2FC", border = "#97C2FC", 
                 highlight = list(background = "orange", border = "orange")),
    size = c(rep(10,11)), 
    #fixed = list("x" = TRUE, "y" = TRUE),
    hidden = rep(FALSE,11))
  
}

# generate bone edges
generate_edges_bone <- function() {
  
  data.frame(
    from = c(1,rep(3,3),7,9,11), 
    to = c(2,4,5,6,8,rep(10,2)),
    arrows = list(to = list(enabled = TRUE, 
                            scaleFactor = 0.3, 
                            type = "arrow")),
    label = c(rep("", 7)),
    id = 1:7,
    width = 4,
    font.size = 12,
    color = list(color = c(rep("black", 7)), 
                 highlight = "yellow"),
    dashes = rep(TRUE,7),
    smooth = rep(TRUE,7),
    hidden = rep(FALSE,7),
    stringsAsFactors=FALSE)
  
}
