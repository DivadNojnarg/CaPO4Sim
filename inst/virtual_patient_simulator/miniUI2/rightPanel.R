rightPanel <- f7Panel(
  title = "Inputs",
  side = "right",
  theme = "light",
  effect = "cover",

  h4("Network options", align = "center"),
  f7checkBoxGroup(
    inputId = "background_choice",
    label = "Network background",
    choices = c("human"),
    selected = "human"
  ),
  f7checkBoxGroup(
    inputId = "network_Ca_choice",
    label = "Select a network",
    choices = c("Ca", "PO4", "PTH", "D3", "FGF23"),
    selected = NULL
  ),
  f7Toggle(
    inputId = "network_hormonal_choice",
    label = "Display hormones",
    checked = TRUE,
    color = "purple"
  ),
  f7Toggle(
    inputId = "network_organ_choice",
    label = "Display organs",
    checked = TRUE,
    color = "purple"
  ),
  hr(),
  f7Stepper(
    inputId = "size_organs",
    label = "Organs",
    min = 50,
    max = 100,
    value = 70,
    step = 5,
    color = "red"
  ),
  f7Stepper(
    inputId = "size_hormones",
    label = "Hormones",
    min = 20,
    max = 60,
    value = 40,
    step = 5,
    color = "red"
  ),
  f7Stepper(
    inputId = "width_organs",
    label = "Organs",
    min = 4,
    max = 14,
    value = 8,
    step = 1,
    color = "red"
  ),
  f7Stepper(
    inputId = "width_hormones",
    label = "Hormones",
    min = 1,
    max = 8,
    value = 4,
    step = 1,
    color = "red"
  ),

  hr(),
  h4("Solver options", align = "center"),
  f7Slider(
    inputId = "tmax",
    label = "Maximum simulated time",
    value = 500,
    min = 0,
    max = 1000
  ) %>%
    f7Tooltip(
      "tmax should exist and set between 1 and 100000."
    ),
  f7Slider(
    inputId = "t_now",
    label = "Time after simulation",
    min = 1,
    max = 500,
    value = 500
  )
)
