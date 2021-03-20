# *------------------------------------------------------------------
# | PROGRAM NAME: footer.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the footer of the dashboard
# *-----------------------------------------------------------------
# | DATA USED:  logos from the /logos folder and from the web
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)
# |
# |
# *------------------------------------------------------------------

interface_logo <- "logos/interface.jpeg"
uzh_logo <- "logos/uzh.svg"
unil_logo <- "logos/unil.svg"
nccr_logo <- "logos/nccr.svg"

left_footer <- fluidRow(
  column(
    width = 3,
    align = "center",
    "The Interface Group",
    a(
      href = "http://interfacegroup.ch/people/",
      target = "_blank",
      img(src = interface_logo, height = "30px")
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
      img(src = nccr_logo, height = "50px")
    ),
    a(
      href = "http://www.uzh.ch/de.html",
      target = "_blank",
      img(src = uzh_logo, height = "30px")
    ),
    "and",
    a(
      href = "https://www.unil.ch/fbm/fr/home.html",
      target = "_blank",
      img(src = unil_logo, height = "30px")
    )
  )
)

footer <- dashboardFooter(
  left = left_footer,
  right = NULL
)
