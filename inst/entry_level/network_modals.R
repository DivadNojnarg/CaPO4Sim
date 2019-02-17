# *------------------------------------------------------------------
# | PROGRAM NAME: network_modals.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This code provide all modals needed to generate the
# |           detailed zooms for  each case study
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)
# |
# |
# *------------------------------------------------------------------

# custom top right button in the title
# better position than at the bottom...
top_dismiss_btn <- tags$button(
  type = "button",
  class = "btn btn-default pull-right",
  `data-dismiss` = "modal",
  icon("close")
)


# Modal img
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


# generate modal
generate_modal <- function(..., title, location, disease = NULL) {

  # modal name
  modalName <- if(is.null(disease)) {
    paste0("modal_zoom_", location)
  } else {
    paste0("modal_zoom_", location, "_", disease)
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

#-------------------------------------------------------------------------
# Intestinal Mechanisms
#-------------------------------------------------------------------------

# baseline
generate_modal(
  title = "Detailed Baseline Intestinal Mechanisms",
  location = "intestine",
  fluidRow(
    column(
      width = 12,
      align = "center",
      modal_img("base_case_zoom/intestine/base_case_notif_intestine.svg")
    )
  )
)

# php1
generate_modal(
  title = "Detailed Baseline Intestinal Mechanisms",
  location = "intestine",
  disease = "php1",
  fluidRow(
    column(
      width = 12,
      align = "center",
      modal_img("php1_zoom/intestine/php1_notif_intestine.svg")
    )
  )
)


# hypopara
generate_modal(
  title = "Detailed Baseline Intestinal Mechanisms",
  location = "intestine",
  disease = "hypopara",
  fluidRow(
    column(
      width = 12,
      align = "center",
      modal_img("hypopara_zoom/intestine/hypopara_notif_intestine.svg")
    )
  )
)


# hypoD3
generate_modal(
  title = "Detailed Baseline Intestinal Mechanisms",
  location = "intestine",
  disease = "hypoD3",
  fluidRow(
    column(
      width = 12,
      align = "center",
      modal_img("hypoD3_zoom/intestine/hypoD3_notif_intestine.svg")
    )
  )
)


#-------------------------------------------------------------------------
# Bone Mechanisms
#-------------------------------------------------------------------------

# baseline
generate_modal(
  title = "Detailed Baseline Bone Mechanisms",
  location = "bones",
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Effect of PTH on bone",
      br(),
      modal_img("base_case_zoom/bone/base_case_notif_bone1.svg")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Effect of D3 on bone",
      br(),
      modal_img("base_case_zoom/bone/base_case_notif_bone2.svg")
    )
  )
)

# php1
generate_modal(
  title = "Detailed Baseline Bone Mechanisms",
  location = "bones",
  disease = "php1",
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Effect of PTH on bone",
      br(),
      modal_img("php1_zoom/bone/php1_notif_bone1.svg")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Effect of D3 on bone",
      br(),
      modal_img("php1_zoom/bone/php1_notif_bone2.svg")
    )
  )
)


# hypopara
generate_modal(
  title = "Detailed Baseline Bone Mechanisms",
  location = "bones",
  disease = "hypopara",
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Effect of PTH on bone",
      br(),
      modal_img("hypopara_zoom/bone/hypopara_notif_bone1.svg")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Effect of D3 on bone",
      br(),
      modal_img("hypopara_zoom/bone/hypopara_notif_bone2.svg")
    )
  )
)


# hypoD3
generate_modal(
  title = "Detailed Baseline Bone Mechanisms",
  location = "bones",
  disease = "hypoD3",
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Effect of PTH on bone",
      br(),
      modal_img("hypoD3_zoom/bone/hypoD3_notif_bone1.svg")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Effect of D3 on bone",
      br(),
      modal_img("hypoD3_zoom/bone/hypoD3_notif_bone2.svg")
    )
  )
)


#-------------------------------------------------------------------------
# Kidney Mechanisms
#-------------------------------------------------------------------------

# baseline
generate_modal(
  title = "Detailed Baseline Kidney Mechanisms",
  location = "kidneys",
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed Ca PT reabsorption",
      br(),
      modal_img(src = "base_case_zoom/kidney/base_case_notif_kidney1.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Detailed Pi PT reabsorption",
      br(),
      modal_img(src = "base_case_zoom/kidney/base_case_notif_kidney2.svg", size = "260px")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed Ca TAL reabsorption",
      br(),
      modal_img(src = "base_case_zoom/kidney/base_case_notif_kidney3.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Detailed Ca DCT reabsorption",
      br(),
      modal_img(src = "base_case_zoom/kidney/base_case_notif_kidney4.svg", size = "260px")
    )
  )
)


# php1
generate_modal(
  title = "Detailed Baseline Kidney Mechanisms",
  location = "kidneys",
  disease = "php1",
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed Ca PT reabsorption",
      br(),
      modal_img(src = "php1_zoom/kidney/php1_notif_kidney1.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Detailed Pi PT reabsorption",
      br(),
      modal_img(src = "php1_zoom/kidney/php1_notif_kidney2.svg", size = "260px")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed Ca TAL reabsorption",
      br(),
      modal_img(src = "php1_zoom/kidney/php1_notif_kidney3.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Detailed Ca DCT reabsorption",
      br(),
      modal_img(src = "php1_zoom/kidney/php1_notif_kidney4.svg", size = "260px")
    )
  )
)


# hypopara
generate_modal(
  title = "Detailed Baseline Kidney Mechanisms",
  location = "kidneys",
  disease = "hypopara",
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed Ca PT reabsorption",
      br(),
      modal_img(src = "hypopara_zoom/kidney/hypopara_notif_kidney1.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Detailed Pi PT reabsorption",
      br(),
      modal_img(src = "hypopara_zoom/kidney/hypopara_notif_kidney2.svg", size = "260px")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed Ca TAL reabsorption",
      br(),
      modal_img(src = "hypopara_zoom/kidney/hypopara_notif_kidney3.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Detailed Ca DCT reabsorption",
      br(),
      modal_img(src = "hypopara_zoom/kidney/hypopara_notif_kidney4.svg", size = "260px")
    )
  )
)


# hypoD3
generate_modal(
  title = "Detailed Baseline Kidney Mechanisms",
  location = "kidneys",
  disease = "hypoD3",
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed Ca PT reabsorption",
      br(),
      modal_img(src = "hypoD3_zoom/kidney/hypoD3_notif_kidney1.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Detailed Pi PT reabsorption",
      br(),
      modal_img(src = "hypoD3_zoom/kidney/hypoD3_notif_kidney2.svg", size = "260px")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed Ca TAL reabsorption",
      br(),
      modal_img(src = "hypoD3_zoom/kidney/hypoD3_notif_kidney3.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Detailed Ca DCT reabsorption",
      br(),
      modal_img(src = "hypoD3_zoom/kidney/hypoD3_notif_kidney4.svg", size = "260px")
    )
  )
)

#-------------------------------------------------------------------------
# Parathyroid Glands Mechanisms
#-------------------------------------------------------------------------

# baseline
generate_modal(
  title = "Detailed Baseline Parathyroid Gland Mechanisms",
  location = "PTHg",
  fluidRow(
    column(
      width = 12,
      align = "center",
      modal_img("base_case_zoom/PTHg/base_case_notif_PTHg.svg")
    )
  )
)

# php1
generate_modal(
  title = "Detailed Baseline Parathyroid Gland Mechanisms During Primary-Hyperparathyroidism",
  location = "PTHg",
  disease = "php1",
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed PTH mechanisms",
      br(),
      modal_img(src = "php1_zoom/PTHg/php1_notif_PTHg1.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Effect of D3 on PTH synthesis",
      br(),
      modal_img(src = "php1_zoom/PTHg/php1_notif_PTHg2.svg", size = "260px")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Effect of Pi on PTH synthesis",
      br(),
      modal_img(src = "php1_zoom/PTHg/php1_notif_PTHg3.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Effect of Ca on PTH secretion",
      br(),
      modal_img(src = "php1_zoom/PTHg/php1_notif_PTHg4.svg", size = "260px")
    )
  )
)

# hypopara
generate_modal(
  title = "Detailed Baseline Parathyroid Gland Mechanisms During Hypoparathyroidism",
  location = "PTHg",
  disease = "hypopara",
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Detailed PTH mechanisms",
      br(),
      modal_img(src = "hypopara_zoom/PTHg/hypopara_notif_PTHg1.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Effect of D3 on PTH synthesis",
      br(),
      modal_img(src = "hypopara_zoom/PTHg/hypopara_notif_PTHg2.svg", size = "260px")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      align = "center",
      "Effect of Pi on PTH synthesis",
      br(),
      modal_img(src = "hypopara_zoom/PTHg/hypopara_notif_PTHg3.svg", size = "260px")
    ),
    br(),
    column(
      width = 6,
      align = "center",
      "Effect of Ca on PTH secretion",
      br(),
      modal_img(src = "hypopara_zoom/PTHg/hypopara_notif_PTHg4.svg", size = "260px")
    )
  )
)


# hypoD3
generate_modal(
  title = "Detailed Parathyroid Gland Mechanisms During Vitamin D3 Deficiency",
  location = "PTHg",
  disease = "hypoD3",
  fluidRow(
    column(
      width = 12,
      align = "center",
      "Detailed PTH mechanisms",
      br(),
      modal_img(src = "hypoD3_zoom/PTHg/hypoD3_notif_PTHg1.svg", size = "260px")
    )
  )
)
