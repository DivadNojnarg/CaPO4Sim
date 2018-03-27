app_css <- function() {
  tags$head(
    tags$style(
      HTML(" #page{
                position: relative;
              }
              
              .js-irs-0 .irs-single, 
              .js-irs-0 .irs-bar-edge,
              .js-irs-0 .irs-bar{
                background: orange;
              }
              
              .js-irs-1 .irs-single, 
              .js-irs-1 .irs-bar-edge, 
              .js-irs-1 .irs-bar{
                background: orange;
              }
              
              .js-irs-2 .irs-single, 
              .js-irs-2 .irs-bar-edge, 
              .js-irs-2 .irs-bar{
                background: orange;
              }
              
              .theme-orange .irs-single, 
              .theme-orange .irs-bar-edge, 
              .theme-orange .irs-bar{
                background: orange;
              }
              
              .network_capnone{
                padding-left: 0; 
                padding-right: 0;
                height:100%;
              }
              
              .network_caprat{
                padding-left: 0; 
                padding-right: 0;
                background-image:url('/CaPO4_network/rat_wholebody.svg');
                background-size: cover;
                background-repeat: no-repeat;
                background-position: center;
                height:100%;
              }
              
              .network_caphuman{
                padding-left: 0; 
                padding-right: 0;
                background-image:url('/CaPO4_network/human_wholebody.svg');
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
              
              #tab { 
                display:inline-block; 
                margin-left: 100px; 
              }

              #shiny-notification-notifid {
                position: fixed; 
                top: 20%; 
                right: 76% ; 
                width: 20em; 
                opacity: 1; 
                z-index:100;
              }

              #shiny-notification-graph_notif {
                position: fixed; 
                top: 30%; 
                left: 70% ; 
                width: 20em; 
                opacity: 1; 
                z-index:100;
              }
              ")
    )
  )
}