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
                      background-image:url('PTHg_zoom.svg');
                      background-size: 100% 100%;
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
  
  # to print help
  introjsUI(),
  # to move graphs and resize them
  includeJqueryUI(),
  useShinyjs(),
  withMathJax(), 
  
  # Application theme
  theme = shinytheme("yeti"),

  # include a dashboard
  dashboardPage(skin = "black", header, sidebar, body)
  
))