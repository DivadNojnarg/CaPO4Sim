generate_slider_events <- function(input) {
  tagList(
    # Generate the slider corresponding to the choosen treatment
    if (input$treatment_selected == "D3_inject") {
      f7Slider(
        inputId = "D3_inject",
        label = "D3 injection",
        value = 0.001,
        min = 0,
        max = 0.1,
        step = 0.001,
        scale = TRUE
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "D3 injection (pmol/min)"))
    }  else if (input$treatment_selected == "Ca_food") {
      f7Slider(
        inputId = "Ca_food",
        label = "Ca intake",
        value = 0.0022,
        min = 0,
        max = 0.008,
        step = 0.0001,
        scale = TRUE
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Calcium intake (μmol/min)"))
    } else if (input$treatment_selected == "Ca_inject") {
      f7Slider(
        inputId = "Ca_inject",
        label = "Ca injection",
        min = 0,
        max = 0.002,
        value = 0.001,
        step = 0.0001,
        scale = TRUE
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Rate of injection of calcium in plasma (μmol/min)"))
    } else if (input$treatment_selected == "P_food") {
      f7Slider(
        inputId = "P_food",
        label = "PO4 intake",
        value = 1.55e-003,
        min = 0,
        max = 0.01,
        step = 0.0001,
        scale = TRUE
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Phosphate intake (μmol/min)"))
    } else if (input$treatment_selected == "P_inject") {
      f7Slider(
        inputId = "P_inject",
        label = "PO4 injection",
        value = 0.001,
        min = 0,
        max = 0.01,
        step = 0.0001,
        scale = TRUE
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "PO4 injection (μmol/min)"))
    } else if (input$treatment_selected == "D3_intake_reduction") {
      f7Slider(
        inputId = "D3_intake_reduction",
        label = "D3 intake percentage reduction",
        value = 50,
        min = 0,
        max = 100,
        scale = TRUE
      )
    },

    # Start, stop and add
    if (input$treatment_selected == "PTX") {
      NULL
    } else {
      f7Stepper(
        inputId = "t_stop",
        label = "Duration (in minutes, 1440 min = 1 day):",
        value = 100000,
        min = 0,
        max = NA
      )
    },
    column(
      width = 12,
      align = "center",
      f7Button(
        inputId = "add_treatment",
        size = "small",
        label = f7Icon("add_round")
      )
    )
  )
}
