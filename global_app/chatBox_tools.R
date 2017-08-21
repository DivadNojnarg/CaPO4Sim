#' Create a chat box for the main body of a dashboard.
#'
#' A \code{chatBox} displays \code{chatMessage}s as main box content, an optinal
#'   \code{contactList} on header, and an optional \code{newMessage} reminder on
#'   header. The \code{contactList} are created by \code{\link{chatContactList}}
#'   which in turn contains multiple \code{\link{chatContact}}s.
#'
#' @inheritParams box
#' @param ... For chat message, this should consist of \code{\link{chatMessage}}s.
#' @param .list An optional list containing messages to put in the chatBox same as
#'   the \code{...} arguments, but in list format. This can be useful when working
#'   with programmatically generated chatMessage.
#'
#' @family boxes
#' @seealso \code{\link{box}} for usage examples.
#'
#' @rdname chatBox
#' @export
#' 

######################## Usefull function from ygdashboard to import #################
# Can be found here: https://github.com/gyang274/ygdashboard/tree/master/R

validateStatus <- function(status) {
  
  if (status %in% validStatuses) {
    return(TRUE)
  }
  
  stop("Invalid status: ", status, ". Valid statuses are: ",
       paste(validStatuses, collapse = ", "), ".")
}

validStatuses <- c("primary", "success", "info", "warning", "danger")

moduleOutput <- function(outputId, tag = tags$li) {
  tag(id = outputId, class = "ygdashboard-module-output")
}

#######################

chatBox <- function(..., textInputId, btnInputId, placeholder = "Type Message ...",
                    title = "chatBox", status = "warning", solidHeader = FALSE, background = NULL, width = 6, height = NULL,
                    admin = "Guang Yang", client = "Ex Machina", contactList = NULL, newMessage = NULL) {
  
  boxClass <- paste0("box direct-chat")
  
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status, " direct-chat-", status)
  }
  if (!is.null(background)) {
    validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  
  
  tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    tags$div(
      class=boxClass,
      # <!-- /.box-header -->
      tags$div(
        class="box-header with-border",
        h3(class="box-title", title),
        tags$div(
          class="box-tools pull-right",
          if (!is.null(newMessage)) tags$span(`data-toggle`="tooltip", title=paste0(newMessage, "New Messages"), class="badge bg-yellow", newMessage),
          tags$button(
            type="button", class="btn btn-box-tool", `data-widget`="collapse", shiny::icon("minus")
          ),
          tags$button(
            type="button", class="btn btn-box-tool", `data-toggle`="tooltip", title="Contacts", `data-widget`="chat-pane-toggle", shiny::icon("comments")
          ),
          tags$button(
            type="button", class="btn btn-box-tool", `data-widget`="remove", shiny::icon("times")
          )
        )
      ),
      # <!-- /.box-body -->
      tags$div(
        class="box-body",
        # <!--/.direct-chat-messages-->
        tags$div(
          class="direct-chat-messages",
          ...
        ),
        # <!--/.direct-chat-contacts-->
        tags$div(
          class="direct-chat-contacts",
          contactList
        )
      ),
      # <!-- /.box-footer -->
      # create input as a shiny textInput ...
      tags$div(
        class="box-footer",
        tags$form(
          action="#", method="post"
        ),
        tags$div(
          class = "input-group form-group shiny-input-container", style="width:100%",
          # style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
          tags$input(
            id = textInputId, type="text", class="form-control", placeholder = placeholder),
          tags$span(
            class="input-group-btn",
            tags$button(
              id = btnInputId, class=paste0("btn btn-", status, " btn-flat action-button"), "Send"
            )
          )
        )
      )
    )
  )
  
}

#' @rdname chatBox
#' @export
chatMessage <- function(name, text,
                        position = c("left", "right"), timestamp = "Just Now") {
  
  position <- match.arg(position)
  
  tags$div(
    class=paste0("direct-chat-msg ", position),
    # <!-- message. position to the left/right -->
    tags$div(
      class="direct-chat-info clearfix",
      tags$span(
        class="direct-chat-name pull-left", name
      ),
      tags$span(
        class="direct-chat-timestamp pull-right", timestamp
      )
    ),
    tags$div(
      class="direct-chat-text",
      text
    )
  )
  
}


#' Create a dynamic chat message output for ygdashboard (client side)
#'
#' This can be used as a placeholder for dynamically-generated \code{\link{chatMessage}}.
#'
#' @param outputId Output variable name.
#'
#' @seealso \code{\link{renderChatMessage}} for the corresponding server side function
#'   and examples.
#' @family box outputs
#' @export
chatMessageOutput <- function(outputId) {
  moduleOutput(outputId, tag = tags$div)
}

#' Create dynamic chat message output (server side)
#'
#' @inheritParams shiny::renderUI
#'
#' @seealso \code{\link{chatMessageOutput}} for the corresponding client side function
#'   and examples.
#' @family box outputs
#' @export
renderChatMessage <- shiny::renderUI

#' Create a chat contact list for the chat box header.
#'
#' @param ... For chat contacts, this should consist of \code{\link{chatContact}}s.
#'
#' @rdname chatBox
#' @export
chatContactList <- function(..., .list = NULL) {
  
  items <- c(list(...), .list)
  
  tags$ul(
    class="contact-list",
    items
  )
  
}

#' @rdname chatBox
#' @export
chatContact <- function(name, date = "Just Now", text = NULL) {
  
  tags$li(
    tags$a(
      href="#",
      tags$div(
        class="contacts-list-info",
        tags$span(
          class="contacts-list-name", name,
          tags$small(class="contacts-list-date pull-right", date)
        ),
        tags$span(
          class="contacts-list-msg", text
        )
      )
    )
  )
  
}