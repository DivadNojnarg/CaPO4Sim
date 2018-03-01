generate_dynamicFooter <- function() {
  dashboardFooter(
    mainText = h5(
      div(style = "display: inline",
          div("2017-2018, the Interface Group", style = "display: inline",
              a(href = "http://interfacegroup.ch/people/", target = "_blank",
                img(src = "interface_logo.png", height = "30px")
              )),
          HTML("<span id=\"tab\"></span>"),
          div("Built with", style = "display: inline",
              a(href = "https://shiny.rstudio.com", target = "_blank",
                img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", 
                    height = "30px")
              ),
              "by",
              a(href = "http://www.rstudio.com", target = "_blank",
                img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                    height = "30px"))),
          HTML("<span id=\"tab\"></span>"),
          div("Funded by", style = "display: inline",
              a(href = "http://www.nccr-kidney.ch", target = "_blank", 
                img(src = "nccr_logo.png", height = "50px")),
              a(href = "http://www.uzh.ch/de.html", target = "_blank", 
                img(src = "uzh_logo.png", height = "30px")),
              "and",
              a(href = "https://www.unil.ch/fbm/fr/home.html", target = "_blank",
                img(src = "unil_logo.png", height = "55px")
              ))
      )), 
    subText = HTML("<b>Version:</b> Beta 3.1")
  ) 
} 