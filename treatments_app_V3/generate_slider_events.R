generate_slider_events <- function(input) {
  tagList(
    # Generate the slider corresponding to the choosen treatment
    if (input$treatment_selected == "D3_inject") {
      sliderInput(
        "D3_inject", 
        "D3 injection", 
        value = 0.001,
        min = 0, 
        max = 0.1, 
        step = 0.001
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "D3 injection (pmol/min)"))
    }  else if (input$treatment_selected == "Ca_food") {
      sliderInput(
        "Ca_food", 
        "Ca intake", 
        value = 0.0022,
        min = 0, 
        max = 0.008, 
        step = 0.0001
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Calcium intake (μmol/min)"))
    } else if (input$treatment_selected == "Ca_inject") {
      sliderInput(
        "Ca_inject", 
        "Ca injection", 
        min = 0, 
        max = 0.002, 
        value = 0.001, 
        step = 0.0001
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Rate of injection of calcium in plasma (μmol/min)"))
    } else if (input$treatment_selected == "P_food") {
      sliderInput(
        "P_food", 
        "PO4 intake", 
        value = 1.55e-003,
        min = 0, 
        max = 0.01, 
        step = 0.0001
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Phosphate intake (μmol/min)"))
    } else if (input$treatment_selected == "P_inject") {
      sliderInput(
        "P_inject", 
        "PO4 injection", 
        value = 0.001, 
        min = 0, 
        max = 0.01, 
        step = 0.0001
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "PO4 injection (μmol/min)"))
    } else if (input$treatment_selected == "D3_intake_reduction") {
      sliderInput(
        "D3_intake_reduction", 
        "D3 intake reduction", 
        value = 1, 
        min = 1, 
        max = 100, 
        step = 1
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "D3 intake reduction fold"))
    }, 
    
    # Start, stop and add
    if (input$treatment_selected == "PTX" |
        input$treatment_selected == "cinacalcet") {
      NULL
    } else {
      numericInput(
        "t_stop",
        "Time when stops this event:", 
        value = 100, 
        min = 0, 
        max = NA, 
        width = "100%"
      )
    },
    column(
      width = 12,
      align = "center",
      actionBttn(
        inputId = "add_treatment", 
        size = "xs",
        label = NULL, 
        style = "material-circle", 
        color = "success", 
        icon = icon("plus")
      ) 
    )
  )
}