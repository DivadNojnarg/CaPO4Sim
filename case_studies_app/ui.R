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

                    #page{
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

  # notification that can be switched on or off as required
  #tags$style("#shiny-notification-notifid {position: fixed; top: 25%; right: 76% ; width: 20em; opacity: 1;}"),
  tags$style("#shiny-notification-notifid {position: fixed; top: 25%; 
             right: 76% ; width: 20em; opacity: 1; z-index:100;}"),
  
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
  div(id = "page",
  navbar,
  dashboardPage(skin = "black", header, sidebar, body)
  )
  
))