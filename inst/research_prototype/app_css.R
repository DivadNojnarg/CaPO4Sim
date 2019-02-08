app_css <- function() {
  # network CSS
  tags$head(
    tags$style(
      HTML("#networkPTH{
              background-image:url('parathyroid_gland_zoom.png');
              background-size: auto 100%;
              background-repeat: no-repeat;
              background-position: center center;
            }

            #networkkidney_zoom2{
              background-image:url('kidney_zoom2.png');
              background-size: auto 100%;
              background-repeat: no-repeat;
              background-position: center center;
            }
            
            #networkkidney_PT{
              background-image:url('kidney_PT_zoom.png');
              background-size: auto 100%;
              background-repeat: no-repeat;
              background-position: center center;
            }
            
            #networkkidney_PT_PO4{
              background-image:url('kidney_PT_PO4_zoom.png');
              background-size: auto 100%;
              background-repeat: no-repeat;
              background-position: center center;
            }
            
            #networkkidney_TAL{
              background-image:url('kidney_TAL_zoom.png');
              background-size: auto 100%;
              background-repeat: no-repeat;
              background-position: center center;
            }
            
            #networkkidney_DCT{
              background-image:url('kidney_DCT_zoom.png');
              background-size: auto 100%;
              background-repeat: no-repeat;
              background-position: center center;
            }
            
            #networkintestine{
              background-image:url('intestine_zoom.png');
              background-size: auto 100%;
              background-repeat: no-repeat;
              background-position: center center;
            }
            
            #networkbone{
              background-image:url('bone_zoom.png');
              background-size: auto 100%;
              background-repeat: no-repeat;
              background-position: center center;
            }
            
            .network_cap{
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
            
            #tab {
              display:inline-block;
              margin-left: 100px;
            }
            ")
    )
  )
}