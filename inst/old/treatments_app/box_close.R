#-------------------------------------------------------------------------
#  This code contains a custom box function from shinydashboard 
#  (https://github.com/rstudio/shinydashboard/blob/master/R/boxes.R). 
#  It is modified compared to classic one: collapsible has been removed and a remove argument was added
#  so as to allow the user to close the box. It is based on AdminLTE javascript function 
#  (https://github.com/rstudio/shinydashboard/blob/master/inst/AdminLTE/app.js)
#
#  David Granjon, the Interface Group, Zurich
#  July 13th, 2017
#-------------------------------------------------------------------------

"%OR%" <- function(a, b) if (!is.null(a)) a else b

box_close <- function(..., title = NULL, footer = NULL, status = NULL,
                solidHeader = FALSE, background = NULL, width = 6,
                height = NULL, removable = FALSE, removed = FALSE) {
  
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (removable && removed) {
    boxClass <- paste(boxClass, "removed-box")
  }
  if (!is.null(background)) {
    validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", title)
  }
  
  removeTag <- NULL
  if (removable) {
    buttonStatus <- status %OR% "default" # returns an error when allowed
    
    removeIcon <- if (removed) "times" else "times"
    
    removeTag <- div(class = "box-tools pull-right",
                       tags$button(class = paste0("btn btn-box-tool"),
                                   `data-widget` = "remove",
                                   shiny::icon(removeIcon)
                       )
    )
  }
  
  headerTag <- NULL
  if (!is.null(titleTag)  || !is.null(removeTag)) {
    headerTag <- div(class = "box-header",
                     titleTag,
                     removeTag
    )
  }
  
  div(class = if (!is.null(width)) paste0("col-sm-", width),
      div(class = boxClass,
          style = if (!is.null(style)) style,
          headerTag,
          div(class = "box-body", ...),
          if (!is.null(footer)) div(class = "box-footer", footer)
      )
  )
}