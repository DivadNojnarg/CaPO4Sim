#-------------------------------------------------------------------------
#  This UI code contains the global UI of the application. It calls
#  header, body and sidebar (which is NULL in this case) and load all 
#  javascript libraries such as shinyJS, extendShinyjs, MathJax... as well
#  as the theme by default which is cerulean (can be changed with theme selector)
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------

# Define UI 
shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("

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
                    
                    "))
    ),

  # 4 danger notification that can be switched on or off as required
  tags$style("#shiny-notification-menu_notif {position: fixed; top: 1%; right: 50% ; width: 40em; opacity: 1;}"),
  tags$style("#shiny-notification-graph_notif {position: fixed; top: 30%; right: 10% ; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-control_notif {position: fixed; top: 75%; right: 15% ; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-diagram_notif {position: fixed; top: 20%; left: 15% ; width: 20em; opacity: 1;}"),
  
  # perform some javascript events such as show/hide ...
  useShinyjs(), 
  # load the help animation library
  introjsUI(),
  # make beautiful notifications, replace showNotifications by shiny
  useToastr(),
  
  use_bs_popover(),
  use_bs_tooltip(),
  
  # Application theme
  theme = shinytheme("journal"),
  
  # include a dashboard
  navbar,
  dashboardPage(skin = "black", header, sidebar, body)
  
))