#' @title CaPO4 glossary UI module
#'
#' @description Create a CaPO4 glossary
#'
#' @param id module id.
#'
#' @export
glossaryCaPO4Ui <- function(id) {

  ns <- NS(id)

  boxTag <- shinydashboard::box(
    solidHeader = TRUE,
    width = 12,
    height = "50%",
    style = "overflow-x: scroll;",
    DT::dataTableOutput(ns("glossary"))
  )
  boxTag$children[[1]] <- tagAppendAttributes(boxTag$children[[1]], id = "boxGlossary")
  boxTag
}




#' @title CaPO4 glossary server module
#'
#' @description Create a CaPO4 glossary
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @export
glossaryCaPO4 <- function(input, output, session) {
  glossary <- data.frame(
    "abreviation" = c("Ca", "Pi", "PTH", "D3", "FGF23",
                      "PTHg", "CaSR", "VDR", "PHP1"),
    "full name" = c(
      "Ionized plasma calcium concentration",
      "Total plasma phosphate concentration",
      "Parathyroid hormone",
      "1,25 dihydroxy vitamin D3 (calcitriol)",
      "Fibroblast growth factor 23",
      "Parathyroid glands",
      "Calcium sensing receptor",
      "Vitamin D receptor",
      "Primary hyperparathyroidism"),
    "units" = c("mM (mmol/l)", "mM", "ng/l", rep("", 6))
  )
  glossary <- DT::datatable(
    glossary,
    escape = c(rep(FALSE, 3), TRUE),
    options = list(dom = 't')
    ) %>%
    DT::formatStyle(
      'full.name',
      color = 'black',
      backgroundColor = 'orange',
      fontWeight = 'bold'
    )

  output$glossary <- DT::renderDataTable(glossary)
}
