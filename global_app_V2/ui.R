#-------------------------------------------------------------------------
#  This UI code contains the global UI of the application. It calls
#  header, body and sidebar (which is NULL in this case) and load all 
#  javascript libraries such as shinyJS, extendShinyjs, MathJax... as well
#  as the theme by default which is cerulean (can be changed with theme selector)
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#
#  bsplus only works with R > 3.3, so pay attention to update R before installing
#  other packages. On shiny-server, always install R packages by running R in the
#  shiny folder. Put the app in src/shiny-server/myApp and access via: 
#  server_ip:3838/myApp
#
#-------------------------------------------------------------------------

# Define UI 
shinyUI(fluidPage(
  
  # network CSS
  
  tags$head(
    tags$style(HTML("
                    #networkPTH{
                      background-image:url('parathyroid_gland_zoom.png');
                      background-size: auto 100%;
                      background-repeat: no-repeat;
                      background-position: center center;
                    }

                    #networkkidney_zoom2{
                      background-image:url('kidney_zoom2.svg');
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

                    "))
  ),
  
  # include the script for Hotjar tracking
  tags$head(includeScript("www/hotjar.js")),
  # include the script needed to find the web browser
  tags$head(includeScript("www/find_navigator.js")),
  
  # to print help
  introjsUI(),
  # JS interactions
  useShinyjs(),
  
  # Application theme
  theme = shinytheme("journal"),
  
  # include a dashboard
  header_box_network,
  dashboardPage(skin = "black", header, sidebar, body)
  
))