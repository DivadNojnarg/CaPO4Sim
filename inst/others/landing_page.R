#################### Landing page example #####################

library(shiny)
library(shinymaterial)
library(shinyWidgets)

# Wrap shinymaterial apps in material_page
ui <- material_page(id = "material_page",
  title = "Basic Page + Parallax",
  nav_bar_color = "grey darken-4", #light-blue darken-1
  
  tags$head(
    tags$style(HTML("
                    #card {
                    
                    text-align:center;
                    
                    }
                    
                    #image {
                    text-align:center;
                    }

                    #material_page{
                    background-image: url(https://userscontent2.emaze.com/images/6bef0e83-37f7-4eff-bbe5-d954a3291d2c/3dcb49e8eb4b1d54d7709dc19f3fdb03.jpg);
          
                    }
                    
                    "))
  ),
  
  # Typically the image will be placed in a folder labeled 'www' 
  # at the same level as the application (server.R & ui.R)
  material_parallax(
    image_source =
      "https://s-media-cache-ak0.pinimg.com/originals/32/00/71/32007153027a378558cb4ce680bcf927.jpg"
      #"http://s1.picswalls.com/wallpapers/2016/06/10/4k-desktop-wallpaper_065227602_309.jpg"
      #"http://gameranx.com/wp-content/uploads/2016/03/Firewatch-4K-Wallpaper-1.jpg"
  ),
  tags$h1("Page Content"),
  
  material_row(
    material_column(
      width = 4,
      material_card(
        div(id = "card",
            title = "Example Card 1",
            depth = 5,
            shiny::tags$h5("Calcium/Phosphate Homeostasis "),
            div(id = "image",
                a(href = "http://172.23.144.204:3838/capApp/global_app/", target="_blank",
                  img(src="https://d30y9cdsu7xlg0.cloudfront.net/png/26262-200.png")),
                h6("Explore the regulation of calcium and phosphate homeostasis through a mathematical model.")
            )
        )
      )
    ),
    
    material_column(
      width = 4,
      material_card(
        div(id = "card",
            title = "Example Card 2",
            depth = 5,
            shiny::tags$h5("Card Content 2"),
            div(id = "image",
                a(href = "", target="_blank",
                  img(src="https://d30y9cdsu7xlg0.cloudfront.net/png/26262-200.png"))
            )
        )
      )
    ),
    
    material_column(
      width = 4,
      material_card(
        div(id = "card",
            title = "Example Card 3",
            depth = 5,
            shiny::tags$h5("Card Content 3"),
            div(id = "image",
                a(href = "", target="_blank",
                  img(src="https://d30y9cdsu7xlg0.cloudfront.net/png/26262-200.png"))
            )
        )
      )
    )
    
  )
  
)

server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)