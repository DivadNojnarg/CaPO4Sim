interface_logo <- "logos/interface.jpeg"
uzh_logo <- "logos/uzh.svg"
unil_logo <- "logos/unil.svg"
nccr_logo <- "logos/nccr.svg"

footer <- fluidRow(
  column(
    width = 6,
    align = "center",
    "The Interface Group",
    a(
      href = "http://interfacegroup.ch/people/",
      target = "_blank",
      img(src = interface_logo, height = "30px")
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
