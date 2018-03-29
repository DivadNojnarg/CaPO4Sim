# *------------------------------------------------------------------
# | PROGRAM NAME: generate_dynamicFooter.R
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

generate_dynamicFooter <- function() {
  dashboardFooter(
    mainText = h5(
      div(style = "display: inline",
          div("2017-2018, the Interface Group", style = "display: inline",
              a(href = "http://interfacegroup.ch/people/", target = "_blank",
                img(src = "logos/interface_logo.png", height = "30px")
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
                img(src = "logos/nccr_logo.png", height = "50px")),
              a(href = "http://www.uzh.ch/de.html", target = "_blank", 
                img(src = "logos/uzh_logo.png", height = "30px")),
              "and",
              a(href = "https://www.unil.ch/fbm/fr/home.html", target = "_blank",
                img(src = "logos/unil_logo.png", height = "55px")
              ))
      )), 
    subText = HTML("<b>Version:</b> Beta 3.2")
  ) 
} 