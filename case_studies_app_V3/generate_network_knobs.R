generate_network_knobs <- function(input, output, session) {
  # handle the size of organ and hormonal nodes
  output$size_nodes_organs <- renderUI({
    tagList(
      knobInput("size_organs", 
                "Organs", 
                min = 50, 
                max = 100, 
                value = 70, 
                step = 5,
                displayPrevious = TRUE,
                fgColor = "#A9A9A9", 
                inputColor = "#A9A9A9",
                skin = "tron",
                width = "100px", 
                height = "100px")
    )
  })
  
  output$size_nodes_hormones <- renderUI({
    tagList(
      knobInput("size_hormones", 
                "Hormones", 
                min = 20, 
                max = 60, 
                value = 40, 
                step = 5,
                displayPrevious = TRUE,
                fgColor = "#A9A9A9", 
                inputColor = "#A9A9A9",
                skin = "tron",
                width = "100px", 
                height = "100px")
    )
  })
  
  # control width of arrows
  output$width_arrows_organs <- renderUI({
    tagList(
      knobInput("width_organs", 
                "Organs",
                angleOffset = -90,
                angleArc = 180,
                min = 4, 
                max = 14, 
                value = 8, 
                step = 1,
                displayPrevious = TRUE,
                fgColor = "#A9A9A9", 
                inputColor = "#A9A9A9",
                skin = NULL,
                width = "100px", 
                height = "100px")
    )
  })
  
  output$width_arrows_hormones <- renderUI({
    tagList(
      knobInput("width_hormones", 
                "Hormones", 
                angleOffset = -90,
                angleArc = 180,
                min = 1, 
                max = 8, 
                value = 4, 
                step = 1,
                displayPrevious = TRUE,
                fgColor = "#A9A9A9", 
                inputColor = "#A9A9A9",
                skin = NULL,
                width = "100px", 
                height = "100px")
    )
  })
}