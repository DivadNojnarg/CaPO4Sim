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
  
  # 4 danger notification that can be switched on or off as required
  tags$style("#shiny-notification-menu_notif {position: fixed; top: 1%; right: 50% ; width: 40em; opacity: 1;}"),
  tags$style("#shiny-notification-graph_notif {position: fixed; top: 30%; right: 25% ; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-control_notif {position: fixed; top: 70%; right: 15% ; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-diagram_notif {position: fixed; top: 75%; left: 10% ; width: 20em; opacity: 1;}"),
  
  # network CSS
  tags$style(".vis-button.vis-up {visibility: hidden;}"), # hide css class navigation from visNetwork
  tags$style(".vis-button.vis-down {visibility: hidden;}"), #see https://github.com/datastorm-open/visNetwork/blob/master/inst/htmlwidgets/lib/vis/vis.css
  tags$style(".vis-button.vis-left {visibility: hidden;}"), # around line 701
  tags$style(".vis-button.vis-right {visibility: hidden;}"),
  
  tags$head(
    tags$style(HTML("
                    #networkPTH{
                      background-image:url('PTHg_zoom.svg');
                      background-size: 100% 100%;
                      background-repeat: no-repeat;
                      background-position: center center;
                    }

                    #network_cap{
                      background-image:url('rat_wholebody.svg');
                      background-size: 100% 100%;
                      background-repeat: no-repeat;
                      background-position: center center;
                    }

                    "))
  ),
  
  # tags$head(tags$script(src="ng-knob.js")),
  # tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/angularjs/1.4.8/angular.min.js")),
  # tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.10/d3.min.js")),
  
  includeJqueryUI(), # to move graphs and resize them
  useShinyjs(),
  #extendShinyjs(text = jscode, functions = c("closeWindow")),
  withMathJax(), 
  
  # Application theme
  theme = shinytheme("yeti"),
  #shinythemes::themeSelector(),
  #theme = "bootswatch-journal.css",
  
  # include a dashboard
  dashboardPage(skin = "black", header, sidebar, body)
  
))