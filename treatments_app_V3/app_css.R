app_css <- function() {
  # network CSS
  tags$head(
    tags$style(
      HTML(".network_cap{
              padding-left: 0; 
              padding-right: 0;
              background-image:url('rat_wholebody.svg');
              background-size: cover;
              background-repeat: no-repeat;
              background-position: center;
              height:100%;
            }

            .network_capnone{
              padding-left: 0; 
              padding-right: 0;
              height:100%;
            }

            .network_caprat{
              padding-left: 0; 
              padding-right: 0;
              background-image:url('rat_wholebody.svg');
              background-size: cover;
              background-repeat: no-repeat;
              background-position: center;
              height:100%;
            }

            .network_caphuman{
              padding-left: 0; 
              padding-right: 0;
              background-image:url('human_wholebody.svg');
              background-size: cover;
              background-repeat: no-repeat;
              background-position: center;
              height:100%;
            }

            #boxvideo{
              text-align: center;
            }

            mark {
              background-color: yellow;
            }

            .newClass{
              min-width: 900px;
              max-width: 900px;
            }

            #about_us{
              margin-left: auto;
              margin-right: auto;
              max-width:70%;
              max-height:70%;
            }

            #zoom_image{
              border: 3px black solid;
              border-radius: 10px;
              box-shadow: 6px 6px 0px black;
            }

            #dropdown_treatment {
              overflow-y: scroll; 
              max-height: 500px;
              overflow-x: hidden;
            }

            #tab { 
              display:inline-block; 
              margin-left: 100px; 
            }
          ")
    )
  )
}