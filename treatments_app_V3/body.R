#-------------------------------------------------------------------------
#  This code contains the body of shinydashboard. It is an advanced dashboard
#  using several advanced javascript properties such as MathJax display,
#  organize each Boxes relatively to each others via jqui commands (shinyjqui), 
#  trigger some help modals when required. 
#
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

body <- dashboardBody(
  
  # include CSS
  app_css(),
  
  # include the script for Hotjar tracking
  #tags$head(includeScript("www/hotjar.js")),
  # include the script needed to find the web browser
  #tags$head(includeScript("www/find_navigator.js")),
  
  # to print help
  introjsUI(),
  # JS interactions
  useShinyjs(),
  # print feedback for input
  useShinyFeedback(),
  useSweetAlert(),
  
  tabItems(
    # Network panel
    tabItem(
      tabName = "main",
      
      fluidRow(
        
        # left colum
        column(
          width = 3,
          style = 'padding:0px;',
          
          # profile box
          boxPlus(
            width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE,
            closable = FALSE,
            boxProfile(
              src = "https://i.pinimg.com/originals/f6/64/69/f66469f759bb4e11898fd14136e8e21b.jpg",
              title = "Mr Rat",
              subtitle = "Lazy and greedy",
              boxProfileItemList(
                bordered = FALSE,
                boxProfileItem(
                  title = "Age",
                  description = 22
                ),
                boxProfileItem(
                  title = "Height",
                  description = "170 cm"
                ),
                boxProfileItem(
                  title = "Friends",
                  description = "80 kg"
                )
              )
            )
          ),
          
          # info box (previous diseases, treatements)
          boxPlus(
            width = 12, solidHeader = FALSE, status = "primary",
            title = "Medical History", collapsible = TRUE, closable = FALSE,
            enable_label = TRUE,
            label_text = 2,
            label_status = "danger",
            userPost(
              id = 1,
              src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
              author = "By Pr. Jonathan Burke",
              description = "14/05/2018 - 10 AM",
              "Kidney stone disease , also known as
              urolithiasis , is when a solid piece of 
              material (kidney stone) occurs in the urinary tract . 
              Kidney stones typically form in the kidney and leave 
              the body in the urine stream. A small stone may pass 
              without causing symptoms. If a stone grows to more 
              than 5 millimeters (0.2 in) it can cause blockage of 
              the ureter resulting in severe pain in the lower 
              back or abdomen. A stone may also result in blood in 
              the urine, vomiting, or painful urination.",
              userPostToolItemList(
                userPostToolItem(dashboardLabel("item 1")),
                userPostToolItem(dashboardLabel("item 2", status = "danger"), side = "right")
              )
            ),
            userPost(
              id = 2,
              src = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
              author = "By Dr.Adam Jones",
              description = "5 days ago",
              userPostMedia(src = "https://steemitimages.com/0x0/https://img.esteem.ws/4dssd1ydr0.jpg"),
              userPostToolItemList(
                userPostToolItem(dashboardLabel("item 1")),
                userPostToolItem(dashboardLabel("item 2", status = "danger"), side = "right")
              )
            )
          ),
          
          # task list, examinations
          boxPlus(
            width = 12, solidHeader = FALSE, status = "primary",
            title = "Task List", collapsible = TRUE, closable = FALSE,
            enable_label = TRUE,
            label_text = 3,
            label_status = "danger",
            todoList(
              sortable = FALSE,
              todoListItem(
                label = "Design a nice theme",
                "Some text here"
              ),
              todoListItem(
                label = "Make the theme responsive",
                "Some text here"
              ),
              todoListItem(
                checked = TRUE,
                label = "Let theme shine like a star"
              )
            )
          )
          
        ),
        
        
        # patient operation table
        column(
          width = 6,
          style = 'padding:0px;',
          
          widgetUserBox(
            title = "Examination Table", closable = FALSE,
            subtitle = starBlock(grade = 5, color = "blue"),
            type = NULL,
            width = 12,
            src = "https://thumbs.dreamstime.com/b/red-heart-pulse-heart-rate-athlete-gym-workout-single-icon-cartoon-style-vector-symbol-stock-web-90353642.jpg",
            color = "yellow",
            introBox(
              div(id = "network_cap",
                  withSpinner(visNetworkOutput("network_Ca", height = "900px"), 
                              size = 2, 
                              type = 8, 
                              color = "#000000")
              ),
              data.step = 2,
              data.intro = help_text[2]
            ),
            footer = tagList(
              dashboardLabel("Label 1", status = "info"),
              dashboardLabel("Label 2", status = "success"),
              dashboardLabel("Label 3", status = "warning"),
              dashboardLabel("Label 4", status = "primary"),
              dashboardLabel("Label 5", status = "danger")
            )
          )
        ),
        
        # event/results column
        column(
          width = 3,
          style = 'padding:0px;',
          
          # results box
          boxPlus(
            width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE,
            closable = FALSE,
            withSpinner(
              plotlyOutput("plot_node", height = "300px", width = "300px"),
              size = 2,
              type = 8,
              color = "#000000"
            )
          ),
          
          # timeline event box
          boxPlus(
            width = 12, solidHeader = FALSE, status = "primary",
            closable = FALSE,
            collapsible = TRUE,
            enable_label = TRUE,
            label_text = 3,
            label_status = "danger",
            style = "overflow-y: scroll;",
            title = "Recent Events",
            timelineBlock(
              style = "height: 300px",
              timelineEnd(color = "danger"),
              timelineLabel(2018, color = "teal"),
              timelineItem(
                title = "Item 1",
                icon = "gears",
                color = "olive",
                time = "now",
                timelineItemMedia(src = "pills.svg"),
                hr(),
                footer = "Here is the footer"
              ),
              timelineItem(
                title = "Item 2",
                timelineItemMedia(src = "syringe.svg"),
                border = FALSE
              ),
              timelineLabel(2015, color = "orange"),
              timelineItem(
                title = "Item 3",
                icon = "paint-brush",
                color = "maroon",
                timelineItemMedia(src = "medicine.svg")
              ),
              timelineStart(color = "gray")
            )
          )
          
        )
      )
      
    ),
    # Demonstration Panel
    tabItem(
      tabName = "demo",
      
      div(id = "boxvideo",
          box(id = "boxvideo", solidHeader = TRUE,
              HTML('<iframe width="560" height="315"
                   src="https://www.youtube.com/embed/AKFyJfYdJhA"
                   frameborder="0" allowfullscreen></iframe>')
          )
      )
    ),
    # About section Panel
    tabItem(
      tabName = "about",
      div(id = "about_us",
          HTML(paste("<img style=\"height: 100%; width: 100%; object-fit: contain\" 
                      border=\"0\" align=\"center\"  src=\"about_us.jpg\"/> "))#,
          #HTML(paste(tags$img(src = "about_us.jpg")))
      )
    )
  )
)