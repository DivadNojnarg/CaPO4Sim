#' @title CaPO4 Network Options UI module
#'
#' @description Options for the network
#'
#' @param id module id
#'
#' @export
networkOptionsUi <- function(id) {

  ns <- NS(id)

  tagList(

    # background choice
    rintrojs::introBox(
      data.step = 5,
      data.intro = help_text[5],
      data.position = "left",
      shinyWidgets::prettyCheckboxGroup(
        inputId = ns("background_choice"),
        label = "Show Background",
        choices = c("human"), #c("rat", "human"),
        animation = "pulse",
        thick = TRUE,
        status = "primary",
        inline = TRUE,
        selected = "human"
      )
    ),
    hr(),

    # enable hormones?
    rintrojs::introBox(
      data.step = 6,
      data.intro = help_text[6],
      data.position = "left",
      shinyWidgets::prettySwitch(
        inputId = ns("hormonal_choice"),
        label = "Regulations?",
        value = TRUE,
        status = "success",
        slim = TRUE,
        bigger = TRUE
      ),

      # enable organs
      shinyWidgets::prettySwitch(
        inputId = ns("organ_choice"),
        label = "Organs?",
        value = TRUE,
        status = "success",
        slim = TRUE,
        bigger = TRUE
      )
    ),

    # filter elements to display
    rintrojs::introBox(
      data.step = 7,
      data.intro = help_text[7],
      data.position = "left",
      shinyWidgets::prettyCheckboxGroup(
        inputId = ns("components_choice"),
        label = "Show Selected Regulatory Networks",
        choices = c("Ca", "PO4", "PTH", "D3", "FGF23"), #c("Na", "SNS", "RAAS", "ANP", "ADH"),
        animation = "pulse",
        thick = TRUE,
        status = "primary",
        inline = TRUE
      )
    ),

    hr(),

    # Control Nodes size
    h4("Icon size"),
    rintrojs::introBox(
      data.step = 8,
      data.intro = help_text[8],
      data.position = "left",
      fluidRow(
        column(
          width = 6,
          class = "col-xs-6",
          uiOutput(outputId = ns("size_nodes_organs"))
        ),
        column(
          width = 6,
          class = "col-xs-6",
          uiOutput(outputId = ns("size_nodes_hormones"))
        )
      )
    ),

    # Control arrow properties
    h4("Arrow width"),
    rintrojs::introBox(
      data.step = 9,
      data.intro = help_text[9],
      data.position = "left",
      fluidRow(
        column(
          width = 6,
          class = "col-xs-6",
          uiOutput(outputId = ns("width_edges_organs"))
        ),
        column(
          width = 6,
          class = "col-xs-6",
          uiOutput(outputId = ns("width_edges_hormones"))
        )
      )
    )
  )
}


#' @title CaPO4 Network Options server module
#'
#' @description Create a CaPO4 network options
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#' @param mobile Whether we are on cellphone/tablets or not. Slot for input$ismobile().
#'
#' @export
networkOptions <- function(input, output, session, mobile) {

  ns <- session$ns

  outputIds <- c(
    "size_nodes_organs",
    "size_nodes_hormones",
    "width_edges_organs",
    "width_edges_hormones"
  )

  knobsProps <- reactive({
    req(!is.null(mobile()))
    list(
      inputIds = outputIds,
      labels = c("Organs", "Hormones", "Organs", "Hormones"),
      mins = c(50, 20, 4, 1),
      maxs = c(100, 60, 14, 4),
      values = c(
        if (mobile()) 85 else 70,
        if (mobile()) 60 else 40,
        8,
        4
      ),
      steps = c(rep(5, 2), rep(1, 2)),
      displayPrevious = rep(TRUE, 4),
      fgColors = rep("#A9A9A9", 4),
      inputColors = rep("#A9A9A9", 4),
      skins = rep("tron", 4),
      width = rep(if (mobile()) "75px" else "100px", 4),
      height = rep(if (mobile()) "75px" else "100px", 4)
    )
  })

  # generate the 4 knobInputs
  lapply(1:4, FUN = function(i) {
    output[[outputIds[[i]]]] <- renderUI({
      with(
        knobsProps(),
        shinyWidgets::knobInput(
          inputId = ns(inputIds[[i]]),
          label = labels[[i]],
          min = mins[[i]],
          max = maxs[[i]],
          value = values[[i]],
          step = steps[[i]],
          displayPrevious = displayPrevious[[i]],
          fgColor = fgColors[[i]],
          inputColor = inputColors[[i]],
          skin = skins[[i]],
          width = width[[i]],
          height = height[[i]]
        )
      )
    })
  })

  #-------------------------------------------------------------------------
  #  Events
  #-------------------------------------------------------------------------

  # display or not display the network background
  # classic addClass and removeClass do not work
  observe({
    if (!is_empty(input$background_choice)) {
      if (input$background_choice == "rat") {
        shinyjs::runjs("$('#network_cap').addClass('network_caprat')")
        shinyjs::runjs("$('#network_cap').removeClass('network_caphuman')")
      } else {
        shinyjs::runjs("$('#network_cap').removeClass('network_caprat')")
        shinyjs::runjs("$('#network_cap').addClass('network_caphuman')")
      }
    } else {
      shinyjs::runjs("$('#network_cap').addClass('network_capnone')")
      shinyjs::runjs("$('#network_cap').removeClass('network_caphuman')")
      shinyjs::runjs("$('#network_cap').removeClass('network_caprat')")
    }
  })


  # prevent user from selecting multiple background
  # in a module when using selector arg, need to prefix the
  # element id by its namespace. This is a bit dirty but totally expected...
  # When using only id, the namespace is not needed (in the same module)
  observe({
    if (is.element("rat", input$background_choice) &&
        !is.element("human", input$background_choice)) {
      shinyjs::disable(selector = "#network_options-background_choice input[value='human']")
    } else {
      shinyjs::enable(selector = "#network_options-background_choice input[value='human']")
    }
    if (is.element("human", input$background_choice) &&
        !is.element("rat", input$background_choice)) {
      shinyjs::disable(selector = "#network_options-background_choice input[value='rat']")
    } else {
      shinyjs::enable(selector = "#network_options-background_choice input[value='rat']")
    }
  })


  # when enable regulation is selected, activates all the checkboxes
  # the reverse case does not work for unknow reason
  observeEvent(input$hormonal_choice, {
    if (input$hormonal_choice) {
      shinyWidgets::updatePrettyCheckboxGroup(
        session,
        inputId = "components_choice",
        selected = c("Ca","PO4", "PTH", "D3", "FGF23")
      )
    }
  })


  #-------------------------------------------------------------------------
  #  Return values
  #-------------------------------------------------------------------------


  # return useful inputs
  return(
    list(
      background = reactive(input$background_choice),
      regulations = reactive(input$hormonal_choice),
      organs = reactive(input$organ_choice),
      components = reactive(input$components_choice),
      organs_nodes_size = reactive(input$size_nodes_organs),
      hormones_nodes_size = reactive(input$size_nodes_hormones),
      organs_edges_size = reactive(input$width_edges_organs),
      hormones_edges_size = reactive(input$width_edges_hormones)
    )
  )
}
