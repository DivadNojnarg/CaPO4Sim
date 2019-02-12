#' @title CaPO4 Network Options UI module
#'
#' @description Options for the network
#'
#' @param id module id
#'
#' @importFrom shiny tagList
#'
#' @export
networkOptionsUi <- function(id) {

  ns <- NS(id)

  tagList(
    # background choice
    introBox(
      data.step = 10,
      data.intro = help_text[10],
      prettyCheckboxGroup(
        inputId = ns("background_choice"),
        label = "Background",
        choices = c("rat", "human"),
        animation = "pulse",
        thick = TRUE,
        status = "primary",
        inline = TRUE,
        selected = "rat"
      )
    ),
    hr(),

    # enable hormones?
    introBox(
      data.step = 11,
      data.intro = help_text[11],
      prettySwitch(
        inputId = ns("hormonal_choice"),
        label = "Regulations?",
        value = TRUE,
        status = "success",
        slim = TRUE,
        bigger = TRUE
      ),

      # enable organs
      prettySwitch(
        inputId = ns("organ_choice"),
        label = "Organs?",
        value = TRUE,
        status = "success",
        slim = TRUE,
        bigger = TRUE
      )
    ),

    # filter elements to display
    introBox(
      data.step = 12,
      data.intro = help_text[12],
      prettyCheckboxGroup(
        inputId = ns("components_choice"),
        label = "Choose your Network",
        choices = c("Ca", "PO4", "PTH", "D3", "FGF23"),
        animation = "pulse",
        thick = TRUE,
        status = "primary",
        inline = TRUE
      )
    ),

    hr(),

    # Control Nodes size
    h4("Nodes size"),
    introBox(
      data.step = 13,
      data.intro = help_text[13],
      fluidRow(
        column(
          width = 6,
          uiOutput(outputId = ns("size_nodes_organs"))
        ),
        column(
          width = 6,
          uiOutput(outputId = ns("size_nodes_hormones"))
        )
      )
    ),

    # Control arrow properties
    h4("Arrow width"),
    introBox(
      data.step = 14,
      data.intro = help_text[14],
      fluidRow(
        column(
          width = 6,
          uiOutput(outputId = ns("width_edges_organs"))
        ),
        column(
          width = 6,
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

  # generate the knob to control node and edges properties
  # handle the size of organ and hormonal nodes
  output$size_nodes_organs <- renderUI({
    req(!is.null(mobile()))
    tagList(
      knobInput(
        inputId = ns("size_nodes_organs"),
        label = "Organs",
        min = 50,
        max = 100,
        value = if (mobile()) 85 else 70,
        step = 5,
        displayPrevious = TRUE,
        fgColor = "#A9A9A9",
        inputColor = "#A9A9A9",
        skin = "tron",
        width = if (mobile()) "75px" else "100px",
        height = if (mobile()) "75px" else "100px"
      )
    )
  })

  output$size_nodes_hormones <- renderUI({
    req(!is.null(mobile()))
    tagList(
      knobInput(
        inputId = ns("size_nodes_hormones"),
        label = "Hormones",
        min = 20,
        max = 60,
        value = if (mobile()) 60 else 40,
        step = 5,
        displayPrevious = TRUE,
        fgColor = "#A9A9A9",
        inputColor = "#A9A9A9",
        skin = "tron",
        width = if (mobile()) "75px" else "100px",
        height = if (mobile()) "75px" else "100px"
      )
    )
  })

  # control width of arrows
  output$width_edges_organs <- renderUI({
    req(!is.null(mobile()))
    tagList(
      knobInput(
        inputId = ns("width_edges_organs"),
        label = "Organs",
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
        width = if (mobile()) "75px" else "100px",
        height = if (mobile()) "75px" else "100px"
      )
    )
  })

  output$width_edges_hormones <- renderUI({
    req(!is.null(mobile()))
    tagList(
      knobInput(
        inputId = ns("width_edges_hormones"),
        label = "Hormones",
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
        width = if (mobile()) "75px" else "100px",
        height = if (mobile()) "75px" else "100px"
      )
    )
  })


  #-------------------------------------------------------------------------
  #  Events
  #-------------------------------------------------------------------------

  # display or not display the network background
  # classic addClass and removeClass do not work
  observe({
    if (!is_empty(input$background_choice)) {
      if (input$background_choice == "rat") {
        runjs("$('#network_cap').addClass('network_caprat')")
        runjs("$('#network_cap').removeClass('network_caphuman')")
      } else {
        runjs("$('#network_cap').removeClass('network_caprat')")
        runjs("$('#network_cap').addClass('network_caphuman')")
      }
    } else {
      runjs("$('#network_cap').addClass('network_capnone')")
      runjs("$('#network_cap').removeClass('network_caphuman')")
      runjs("$('#network_cap').removeClass('network_caprat')")
    }
  })


  # prevent user from selecting multiple background
  observe({
    if (is.element("rat", input$background_choice) &&
        !is.element("human", input$background_choice)) {
      runjs("$('#network_options-background_choice input[value='human']').prop('disabled', true)")
      #disable(selector = "#background_choice input[value='human']")
    } else {
      runjs("$('#network_options-background_choice input[value='human']').prop('disabled', false)")
      #enable(selector = "#background_choice input[value='human']")
    }
    if (is.element("human", input$background_choice) &&
        !is.element("rat", input$background_choice)) {
      runjs("$('#network_options-background_choice input[value='rat']').prop('disabled', true)")
      #disable(selector = "#background_choice input[value='rat']")
    } else {
      runjs("$('#network_options-background_choice input[value='rat']').prop('disabled', false)")
      #enable(selector = "#background_choice input[value='rat']")
    }
  })


  # when enable regulation is selected, activates all the checkboxes
  # the reverse case does not work for unknow reason
  observeEvent(input$hormonal_choice, {
    if (input$hormonal_choice) {
      updatePrettyCheckboxGroup(
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
