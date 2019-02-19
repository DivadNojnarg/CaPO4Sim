# custom top right button in the title
# better position than at the bottom...
top_dismiss_btn <- tags$button(
  type = "button",
  class = "btn btn-default pull-right",
  `data-dismiss` = "modal",
  icon("close")
)


#' @title Create a modal image wrapper
#'
#' @description A modal image wrapper
#'
#' @param src Image path or url.
#' @param size Image size. 440px by default.
modal_img <- function(src, size = "440px") {
  a(
    href = src,
    target = "_blank",
    img(
      src = src,
      id = "zoom_image",
      width = size,
      height = size,
      border = "0"
    )
  )
}



#' @title Generate a single modal
#'
#' @description Creates a single modal
#'
#' @param ... Modal content.
#' @param title Modal title.
#' @param location Organ location. Can be \code{c("intestine", "bones", "kidneys", "PTHg")}.
#' @param diseases Current diseases. NULL or \code{c("php1", "hypopara", "hypoD3")}.
generate_modal <- function(..., title, location, diseases = NULL) {

  # modal name
  modalName <- if(is.null(diseases)) {
    paste0("modal_zoom_", location)
  } else {
    paste0("modal_zoom_", location, "_", diseases)
  }

  # modal Tag
  modalTag <- modalDialog(
    title = tagList(
      title,
      top_dismiss_btn
    ),
    ...,
    size = "m",
    footer = NULL
  )
  assign(modalName, modalTag, envir = .GlobalEnv)
}


diseases_ids <-  c("base_case", "php1", "hypopara", "hypoD3")
locations <- c("intestine", "bones", "kidneys", "PTHg")


#' @title Generate all modals
#'
#' @description Creates a all modals (16)
#'
#' @param diseases Current diseases. NULL or \code{c("php1", "hypopara", "hypoD3")}.
#' @param location Organ location. Can be \code{c("intestine", "bones", "kidneys", "PTHg")}.
create_all_modals <- function(diseases, locations) {
  lapply(seq_along(locations), FUN = function(i) {
    lapply(seq_along(diseases), FUN = function(j) {

      generate_modal(
        title = if (diseases[[j]] == "base_case") {
          paste("Detailed Baseline", locations[[i]], "Mechanisms")
        } else if (diseases[[j]] == "php1") {
          paste("Detailed", locations[[i]] , "Mechanisms During Primary Hyperparathyroidism")
        } else if (diseases[[j]] == "hypopara") {
          paste("Detailed", locations[[i]] , "Mechanisms During Hypoparathyroidism")
        } else {
          paste("Detailed", locations[[i]] , "Mechanisms During Vitamin D3 Deficiency")
        },
        location = locations[[i]],
        diseases = if (j == 1) NULL else diseases[[j]],
        if (locations[[i]] == "intestine") {
          fluidRow(
            column(
              width = 12,
              align = "center",
              modal_img(paste0(diseases[[j]], "_zoom/intestine/", diseases[[j]], "_notif_intestine.svg"))
            )
          )
        } else if (locations[[i]] == "bones") {
          tagList(
            fluidRow(
              column(
                width = 12,
                align = "center",
                "Effect of PTH on bone",
                br(),
                modal_img(paste0(diseases[[j]], "_zoom/bone/", diseases[[j]], "_notif_bone1.svg"))
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 12,
                align = "center",
                "Effect of D3 on bone",
                br(),
                modal_img(paste0(diseases[[j]], "_zoom/bone/", diseases[[j]], "_notif_bone2.svg"))
              )
            )
          )
        } else if (locations[[i]] == "kidneys") {
          tagList(
            fluidRow(
              column(
                width = 6,
                align = "center",
                "Detailed Ca PT reabsorption",
                br(),
                modal_img(src = paste0(diseases[[j]], "_zoom/kidney/", diseases[[j]], "_notif_kidney1.svg"), size = "260px")
              ),
              column(
                width = 6,
                align = "center",
                "Detailed Pi PT reabsorption",
                br(),
                modal_img(src = paste0(diseases[[j]], "_zoom/kidney/", diseases[[j]], "_notif_kidney2.svg"), size = "260px")
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 6,
                align = "center",
                "Detailed Ca TAL reabsorption",
                br(),
                modal_img(src = paste0(diseases[[j]], "_zoom/kidney/", diseases[[j]], "_notif_kidney3.svg"), size = "260px")
              ),
              column(
                width = 6,
                align = "center",
                "Detailed Ca DCT reabsorption",
                br(),
                modal_img(src = paste0(diseases[[j]], "_zoom/kidney/", diseases[[j]], "_notif_kidney4.svg"), size = "260px")
              )
            )
          )
        } else {
          if (diseases[[j]] == "base_case") {
            fluidRow(
              column(
                width = 12,
                align = "center",
                modal_img("base_case_zoom/PTHg/base_case_notif_PTHg.svg")
              )
            )
          } else if (diseases[[j]] == "hypoD3") {
            fluidRow(
              column(
                width = 12,
                align = "center",
                "Detailed PTH mechanisms",
                br(),
                modal_img(src = "hypoD3_zoom/PTHg/hypoD3_notif_PTHg1.svg", size = "260px")
              )
            )
          } else {
            tagList(
              fluidRow(
                column(
                  width = 6,
                  align = "center",
                  "Detailed PTH mechanisms",
                  br(),
                  if (j == 2) {
                    modal_img(src = "php1_zoom/PTHg/php1_notif_PTHg1.svg", size = "260px")
                  } else {
                    modal_img(src = "hypopara_zoom/PTHg/hypopara_notif_PTHg1.svg", size = "260px")
                  }
                ),
                column(
                  width = 6,
                  align = "center",
                  "Effect of D3 on PTH synthesis",
                  br(),
                  if (j == 2) {
                    modal_img(src = "php1_zoom/PTHg/php1_notif_PTHg2.svg", size = "260px")
                  } else {
                    modal_img(src = "hypopara_zoom/PTHg/hypopara_notif_PTHg2.svg", size = "260px")
                  }
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  align = "center",
                  "Effect of Pi on PTH synthesis",
                  br(),
                  if (j == 2) {
                    modal_img(src = "php1_zoom/PTHg/php1_notif_PTHg3.svg", size = "260px")
                  } else {
                    modal_img(src = "hypopara_zoom/PTHg/hypopara_notif_PTHg3.svg", size = "260px")
                  }
                ),
                column(
                  width = 6,
                  align = "center",
                  "Effect of Ca on PTH secretion",
                  br(),
                  if (j == 2) {
                    modal_img(src = "php1_zoom/PTHg/php1_notif_PTHg4.svg", size = "260px")
                  } else {
                    modal_img(src = "hypopara_zoom/PTHg/hypopara_notif_PTHg4.svg", size = "260px")
                  }
                )
              )
            )
          }
        }
      )
    })
  })
}

create_all_modals(diseases = diseases_ids, locations = locations)
