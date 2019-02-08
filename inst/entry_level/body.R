# *------------------------------------------------------------------
# | PROGRAM NAME: body.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  Contains the dashboard body
# |*------------------------------------------------------------------
# | DATA USED:  /www/rintrojs_count.js
# |             /www/hotjar.js
# |             3 youtube links for movies
# |             /logos/about_us.jpg
# |
# |*------------------------------------------------------------------
# | CONTENTS:
# |
# |  PART 1:  load CSS, shinyjs, introjs, MathJax, bs_popovers, bs_tooltips
# |           shinyFeedback, sweetalerts
# |  PART 2:  load js scripts (hotjar tracking, ...)
# |  PART 3:  main network_box, graph_box, load other tabPanels
# |           like movies, glossary, about_section
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)
# |
# |
# *------------------------------------------------------------------

body <- dashboardBody(

  # load the css
  includeCSS(path = "www/css/case_studies_app.css"),

  # include scripts
  tags$head(includeScript(paste0(getwd(), "/www/js/rintrojs_count.js"))),
  includeScript(path = "www/js/find-navigator.js"),
  tags$head(includeScript(paste0(getwd(), "/www/js/hotjar.js"))),

  # Load other JS facilities
  useShinyjs(),
  introjsUI(),
  useToastr(),
  withMathJax(),
  use_bs_popover(),
  use_bs_tooltip(),
  useShinyFeedback(),
  useSweetAlert(),

  # shinyEffects
  setShadow(class = "box"),
  setShadow(class = "dropdown-menu"),
  setZoom(class = "box", scale = 1.01),
  setShadow(class = "modal-content"),
  setZoom(class = "modal-content"),

  # Main application Panel
  tabItems(
    tabItem(
      tabName = "main",
      fluidRow(
        # load the CaPO4 network box
        network_box(),
        # load the graph box
        graph_box()
      )
    ),

    # Video panels
    tabItem(
      tabName = "video",
      fluidRow(
        #tagAppendAttributes(
          carousel(
            id = "videoCarousel",
            carouselItem(
              caption = "Calcium",
              tags$iframe(
                width = "100%",
                height = "450",
                src = "https://youtube.com/embed/9x2QFK6_IkQ",
                frameborder = "0",
                `allowfullscreen` <- NA
              )
            ),
            carouselItem(
              caption = "Phosphate",
              tags$iframe(
                width = "100%",
                height = "450",
                src = "https://youtube.com/embed/1eh5VF6poWo",
                frameborder = "0",
                `allowfullscreen` <- NA
              )
            ),
            carouselItem(
              caption = "PTH",
              tags$iframe(
                width = "100%",
                height = "450",
                src = "https://youtube.com/embed/5OEenuXMjyg",
                frameborder = "0",
                `allowfullscreen` <- NA
              )
            )
          ),
          align = "center"
        #)
      )
    ),

    # About section Panel
    tabItem(
      tabName = "about",
      div(
        id = "about_us",
        HTML(
          paste("<img style=\"height: 100%; width: 100%; object-fit: contain\"
                border=\"0\" align=\"center\"  src=\"logos/about_us.jpg\"/> ")
        )
      )
    ),
    # Glossary Panel
    tabItem(
      tabName = "glossary",
      div(
        id = "glossary",
        box(
          id = "boxglossary",
          solidHeader = TRUE,
          width = 12,
          height = "50%",
          dataTableOutput("glossary")
        )
      )
    )
  )
)
