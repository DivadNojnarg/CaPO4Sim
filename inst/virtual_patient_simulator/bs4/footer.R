footer <- bs4DashFooter(
  fluidRow(
    column(
      width = 3,
      align = "center",
      "The Interface Group",
      a(
        href = "http://interfacegroup.ch/people/",
        target = "_blank",
        img(src = "logos/interface_logo.jpeg", height = "30px")
      )
    ),
    column(
      width = 3,
      align = "center",
      "With",
      a(
        href = "https://shiny.rstudio.com",
        target = "_blank",
        img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px")
      ),
      "by",
      a(
        href = "http://www.rstudio.com",
        target = "_blank",
        img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px")
      )
    ),
    column(
      width = 6,
      align = "center",
      a(
        href = "http://www.nccr-kidney.ch",
        target = "_blank",
        img(src = "logos/nccr_logo.png", height = "50px")
      ),
      a(
        href = "http://www.uzh.ch/de.html",
        target = "_blank",
        img(src = "logos/uzh_logo.png", height = "30px")
      ),
      "and",
      a(
        href = "https://www.unil.ch/fbm/fr/home.html",
        target = "_blank",
        img(src = "logos/unil_logo.png", height = "55px")
      )
    )
  )
) 