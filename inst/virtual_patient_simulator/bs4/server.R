#-------------------------------------------------------------------------
#  This application is a R-Shiny implementation of a calcium and phosphate
#  homeostasis model. It aims at being used by medical students but also
#  researchers. See https://divadnojnarg.github.io for more informations
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------

server <- function(input, output, session) {

  # enable fullscreen
  callModule(module = fullScreen, id = "fullScreenTrigger")

  #-------------------------------------------------------------------------
  #  useful datas: initialization. These data are not in global.R since
  #  they are some time reloaded by the program. In global.R they would not
  #  be reloaded, which would corrupt the new session
  #
  #-------------------------------------------------------------------------

  # all students names for the session
  students_names <- paste(rep("Jane Doe", 5), c(1:5))

  # load all questions
  questions <- generate_questions()

  # load patient files
  patient_datas <- patient_selector()

  # Load state values based on files previously created for each case (php1, hypopara, hypoD3)
  patient_state_0 <- patient_datas$initial_conditions

  # patient disease
  patient_disease <- patient_datas$disease_id

  # game answers
  if (patient_disease == "php1") {
    answer <- c("primary hyperparathyroidism")
  } else if (patient_disease == "hypopara") {
    answer <- c("hypoparathyroidism")
  } else if (patient_disease == "hypoD3") {
    answer <- c("vitamin D3 deficiency")
  } else if (patient_disease == "hyperD3") {
    answer <- c("vitamin D3 intoxication")
  }

  # disease answer list for students
  diseases_list <- c(
    "nephrolithiasis",
    "primary hyperparathroidism",
    "vitamin D3 intoxication",
    "hypoparathyroidism",
    "ricket",
    "oncogenic osteomalacia",
    "FGF23 deficiency",
    "vitamin D3 deficiency",
    "nephrocalcinosis",
    "depression",
    "nonalcoholic fatty liver disease"
  )

  # below is needed to handle treatments events
  treatment_choices <- c(
    #"PTX",
    #"D3_inject",
    #"Ca_food",
    #"Ca_inject",
    #"P_food",
    #"P_inject",
    "D3_intake_reduction",
    "cinacalcet",
    "bisphosphonate",
    "furosemide"
  )

  # plot summary list
  summary_plot_names <- c(
    "Ca_p",
    "PO4_p",
    "PTH_p",
    "D3_p",
    "FGF_p"
  )

  # initialization of the patient feedback observer
  patient_feedback <- NULL

  # # inititalization of the timer
  # minutes_time <- 60 # the application will stop in 60 minutes
  start_time <- Sys.time()
  # end_time <- start_time + minutes_time * 60

  # store the app url
  app_url <- reactive({
    paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname, ":",
      session$clientData$url_port
    )
  })

  # store the current user folder
  user_folder <- reactive({
    paste0(
      users_logs, "/",
      input$user_name, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
  })

  #-------------------------------------------------------------------------
  #  Store times, state and parameters in reactive values that can
  #  react to user inputs
  #
  #-------------------------------------------------------------------------

  # Basic reactive expressions needed by the solver
  times <- reactive({
    seq(0, ifelse(parameters()[["t_stop"]] != 0, parameters()[["t_stop"]], input$tmax), by = 1)
  })

  # initial conditions
  states <- reactiveValues(
    val = list(),
    counter = 1,
    name = "start_case"
  )

  # storing parameters event from the data frame to a reactive list
  parameters_event <- reactive({
    c(
      # static event parameters
      "PTX_coeff" = ifelse(isTRUE(events$PTX), 0, 1),
      # dynamic event parameters
      generate_event_parms(events$current)
    )
  })

  # Create parameters sets for all diseases and treatments
  parameters_disease <- reactive({
    c("k_prod_PTHg" = ifelse(
      patient_disease == "php1", 300*4.192,
      ifelse(patient_disease == "hypopara", 0, 4.192)
    ),
    "D3_inact" = ifelse(
      patient_disease == "hypoD3", 0,
      ifelse(patient_disease == "hyperD3", 5e-004, 2.5e-005)
    )
    )
  })


  # make a vector of disease related parameters,
  # fixed_parameters and parameters related to events
  parameters <- reactive({
    c(parameters_disease(), parameters_fixed, parameters_event())
  })

  #-------------------------------------------------------------------------
  #  Render Patient boxes: patient_info,
  #  medical_history, timeline events as well
  #  as the graph and CaPO4 network box
  #
  #-------------------------------------------------------------------------

  # patient info box
  output$patient_info <- renderUI({
    medical_history <- patient_datas$medical_history
    len <- length(medical_history$pathologies)

    bs4Card(
      title = "Past Medical History",
      footer = NULL,
      status = "primary",
      elevation = 4,
      solidHeader = TRUE,
      headerBorder = TRUE,
      gradientColor = NULL,
      width = 12,
      height = NULL,
      collapsible = TRUE,
      collapsed = FALSE,
      closable = FALSE,
      labelStatus = "danger",
      labelText = len,
      labelTooltip = NULL,
      dropdownMenu = NULL,
      dropdownIcon = "wrench",
      overflow = FALSE,

      cardProfile(
        src = patient_datas$picture,
        title = patient_datas$name,
        subtitle = NULL,
        cardProfileItemList(
          bordered = FALSE,
          cardProfileItem(
            title = "Age",
            description = patient_datas$age
          ),
          cardProfileItem(
            title = "Height",
            description = patient_datas$height
          ),
          cardProfileItem(
            title = "Weight",
            description = patient_datas$weight
          )
        )
      ),
      br(),
      lapply(1:len, FUN = function(i){
        userPost(
          id = i,
          collapsed = FALSE,
          src = medical_history$doctors_avatars[[i]],
          author = medical_history$doctors[[i]],
          description = strong(medical_history$pathologies[[i]]),
          HTML(paste(medical_history$disease_description[[i]])),
          if (!is.null(medical_history$disease_image[[i]])) {
            userPostMedia(src = medical_history$disease_image[[i]])
          }
        )
      })
    )
  })

  # the user notebook
  output$user_notebook <- renderUI({
    if (events$logged) {
      comments <- comments$history
      len <- nrow(comments)

      bs4SocialCard(
        closable = FALSE,
        width = 12,
        title = paste0(input$user_name, "'s notebook"),
        subtitle = start_time,
        src = "https://image.flaticon.com/icons/svg/305/305983.svg",
        if (events$animation >= 8) {
          tagList(
            column(
              width = 12,
              align = "center",
              actionBttn(
                inputId = "diagnosis",
                size = "lg",
                label = "Diagnosis",
                style = "fill",
                color = "primary",
                icon = icon("search")
              )
            ),
            br()
          )
        },
        if (events$animation < 8) {
          tagList(
            textAreaInput(
              inputId = "user_comment",
              label = questions[[events$animation + 1]],
              value = "I enter here all my observations!"
            ),
            column(
              width = 12,
              align = "center",
              actionBttn(
                inputId = "user_add_comment",
                size = "sm",
                icon = "Next",
                style = "fill",
                color = "success"
              )
            )
          )
        },
        comments = if (len > 0) {
          tagList(
            lapply(1:len, FUN = function(i) {
              cardComment(
                src = "https://image.flaticon.com/icons/svg/305/305983.svg",
                title = questions[[i]],
                date = comments$date[[i]],
                comments$description[[i]]
              )
            })
          )
        } else {
          NULL
        },
        footer = NULL
      )
    }
  })

  # Event to be added in the timeLine
  output$recent_events <- renderUI({
    if (events$logged) {
      if (events$animation_started) {
        len <- nrow(events$history)
        name <- events$history$event
        start_time <- events$history$real_time
        rate <- events$history$rate
        plasma_values <- plasma_analysis$history

        withMathJax(
          bs4Card(
            title = "Recent Events",
            footer = NULL,
            status = "primary",
            elevation = 4,
            solidHeader = TRUE,
            headerBorder = TRUE,
            gradientColor = NULL,
            width = 12,
            height = NULL,
            collapsible = TRUE,
            collapsed = FALSE,
            closable = FALSE,
            labelStatus = "danger",
            labelText = len,
            labelTooltip = NULL,
            dropdownMenu = NULL,
            dropdownIcon = "wrench",
            overflow = TRUE,

            # treatments input are
            # in the event box
            if (!is.null(events$answered)) {
              tagList(
                prettyCheckboxGroup(
                  inputId = "treatment_selected",
                  label = "Select a new treatment:",
                  choices = c(
                    #"parathyroid surgery" = "PTX",
                    #"D3 iv injection" = "D3_inject",
                    #"Ca supplementation" = "Ca_food",
                    #"Ca iv injection" = "Ca_inject",
                    #"Pi iv injection" = "P_inject",
                    #"Pi supplementation" = "P_food",
                    "D3 intake reduction" = "D3_intake_reduction",
                    "Cinacalcet" = "cinacalcet",
                    "Bisphosphonate" = "bisphosphonate",
                    "Furosemide" = "furosemide"
                  ),
                  thick = TRUE,
                  inline = TRUE,
                  animation = "pulse"
                ),
                uiOutput(outputId = "sliderInject"),
                hr()
              )
            },

            if (len > 0) {

              items <- lapply(1:len, FUN = function(i){
                item <- tagAppendAttributes(
                  bs4TimelineItem(
                    title = name[[i]],
                    icon = "medkit",
                    status = "orange",
                    time = bs4Badge(
                      position = "left",
                      rounded = FALSE,
                      status = "warning",
                      start_time[[i]]
                    ),
                    bs4TimelineItemMedia(
                      src = if (name[[i]] %in% c("D3_inject", "Ca_inject", "P_inject")) {
                        "treatments_img/syringe.svg"
                      } else if (name[[i]] %in% c("Ca_food", "P_food", "D3_intake_reduction")) {
                        "treatments_img/medicine.svg"
                      } else if (name[[i]] == "PTX") {
                        "treatments_img/surgery.svg"
                      } else if (name[[i]] %in% c("cinacalcet", "furosemide", "bisphosphonate")) {
                        "treatments_img/pills.svg"
                      } else if (name[[i]] == "plasma analysis") {
                        "treatments_img/test-tube.svg"
                      },
                      width = "40",
                      height = "40"
                    ),
                    # in case of plasma analysis, display the results next to the logo
                    if (name[[i]] == "plasma analysis") {
                      tagList(
                        paste0("$$[Ca^{2+}_p] = ", round(plasma_values[i, 'Ca_p'], 2), " mM [1.1-1.4 mM]$$"),
                        paste0("$$[P_i] = ", round(plasma_values[i, "PO4_p"], 2), " mM [0.8-1.6 mM]$$"),
                        paste0("$$[PTH_p] = ", round(plasma_values[i, "PTH_p"] * 100) * 1.33, " pM [3-16 pM]$$"),
                        # scale D3
                        paste0("$$[1,25D3_p] = ", round(plasma_values[i, "D3_p"]) / 4, " pM [36-150 pM]$$"),
                        # scale FGF23
                        paste0("$$[FGF23_p] = ", round(plasma_values[i, "FGF_p"] / 25, 2), " pM [0.3-2.1 pM]$$")
                      )
                    },
                    footer = NULL
                    #if (!is.null(name[[i]])) {
                    #  if (name[[i]] != "PTX")
                    #    if (!(name[[i]] %in% c("PTX", "plasma analysis"))) {
                    #      dashboardLabel(status = "danger", rate[[i]])
                    #    }
                    #  else NULL
                    #}
                  ),
                  align = "middle"
                )

                item$children[[2]]$children[[3]] <- tagAppendAttributes(
                  item$children[[2]]$children[[3]],
                  style = "overflow-x: auto;"
                )

                item

              })

              bs4Timeline(
                width = 12,
                style = "height: 400px;",
                bs4TimelineStart(status = "danger"),
                br(),
                items,
                br(),
                bs4TimelineEnd(status = "gray")
              )
            }
          )
        )
      }
    }
  })

  # graph box
  output$graphs_box <- renderUI({
    if (events$logged) {
      if (events$animation_started) {
        bs4Card(
          width = 12,
          elevation = 4,
          #title = "Click on the plasma node to display concentrations",
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          closable = FALSE,
          withSpinner(
            plotlyOutput(
              "plot_node",
              height = "300px",
              width = "100%"
            ),
            size = 2,
            type = 8,
            color = "#000000"
          )
        )
      }
    }
  })

  # network box
  output$network_box <- renderUI({
    if (events$logged) {
      if (events$animation_started) {
        cardTag <- bs4Card(
          title = tagList(
            actionBttn(
              inputId = "run",
              size = "lg",
              label = "Run",
              style = "fill",
              color = "primary",
              icon = icon("play")
            ),
            actionBttn(
              inputId = "summary",
              size = "lg",
              label = "Summary",
              style = "fill",
              color = "royal",
              icon = icon("tv")
            )
          ),
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          width = 12,
          closable = FALSE,
          #enable_sidebar = TRUE,
          #sidebar_width = 50,
          #sidebar_background = "#888888",
          #sidebar_start_open = FALSE,
          #sidebar_content = tagList(
          #  getting_started()
          #),
          div(
            id = "network_cap",
            withSpinner(
              visNetworkOutput(
                "network_Ca",
                height = if (input$isMobile) "450px" else "900px"
              ),
              size = 2,
              type = 8,
              color = "#000000"
            )
          ),
          footer = NULL
        )

        cardTag[[2]]$children[[1]]$children[[2]] <- tagAppendAttributes(
          cardTag[[2]]$children[[1]]$children[[2]],
          class = "p-0"
        )

        cardTag

      }
    }
  })


  # wrap the whole UI
  output$patient_ui <- renderUI({
    fluidRow(
      # left colum
      column(
        width = if (events$animation_started) 3 else 6,
        style = 'padding:0px;',

        # profile box
        uiOutput("patient_info"),
        # user notebook
        uiOutput("user_notebook")
      ),

      # patient operation table
      column(
        width = 6,
        style = 'padding:0px;',
        uiOutput("network_box")
      ),
      # event/results column
      column(
        width = 3,
        style = 'padding:0px;',
        # results box
        uiOutput("graphs_box"),
        # timeline event box
        uiOutput("recent_events")
      )
    )
  })


  #-------------------------------------------------------------------------
  #  Javascript alerts: to give instructions to users, handle when the
  #  game ends
  #
  #-------------------------------------------------------------------------

  # time <- reactiveValues(switcher = FALSE)
  #
  # # set up a timer during which user have to finish the game
  # # and generate the related progress bar
  # countdown <- reactive({
  #   invalidateLater(1000, session)
  #   countdown <- end_time - Sys.time()
  # })
  #
  # # switch between minutes and seconds when coutdown < 1 minute
  # observe({
  #   if (countdown()<= 1.02) {
  #     time$switcher <- TRUE
  #   }
  # })
  #
  # # convert in percentage for the progress bar
  # percent_countdown <- reactive({
  #   countdown <- countdown()
  #   if (!time$switcher) {
  #     countdown / minutes_time * 100
  #   } else {
  #     countdown / 60 * 100
  #   }
  # })
  #
  # # render the progress bar for countdown
  # output$currentTime <- renderUI({
  #   if (!events$stop) {
  #     countdown <- countdown()
  #     percent_countdown <- percent_countdown()
  #     statusClass <- if (!time$switcher) {
  #       if (66 < percent_countdown & percent_countdown <= 100) {
  #         "success"
  #       } else if (30 < percent_countdown & percent_countdown <= 66) {
  #         "warning"
  #       } else {
  #         "danger"
  #       }
  #     } else {
  #       "danger"
  #     }
  #     progressBar(
  #       id = "countdown",
  #       value = percent_countdown,
  #       status = statusClass,
  #       striped = TRUE,
  #       size = "xs",
  #       title = paste0("End in ", round(countdown), if (!time$switcher) " min" else " sec")
  #     )
  #   }
  # })

  # When the counter is equal to 0, each time the session is opened,
  # show the how to start sweetAlert
  # I set up a delay of 5 seconds so that the alert is not displayed before
  # the page is fully loaded (in case we use preloaders in the dashboardPagePlus
  # the preloader lasts around 3s...)
  observe({
    if (!events$logged) {
      shinyjs::delay(
        5000,
        confirmSweetAlert(
          session,
          inputId = "register_user",
          title = "How to start?",
          text = tagList(
            img(src = "interface_img/start.svg", width = "100px", height = "100px"),
            br(),
            HTML(
              "You will be presented with a patient case-study related
               to CaPO4 homeostasis. The goal of this activity is to
               <b>establish</b> a diagnosis and <b>treat</b>
               the patient correctly:
               <ol>
               <li> To establish your diagnostic, you can click on any compartment e.g.
                click on plasma to conduct blood plasma analyses. </li>
               <li> After having established an initial diagnostic you will be
               offered multiple treatment options. </li>
              </ol>"
            ),
            hr(),
            column(
              align = "center",
              width = 12,
              selectInput(
                inputId = "user_name",
                label = "Your name:",
                choices = students_names,
                selected = NULL,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
              )
            )
          ),
          btn_labels = c(NA, "Confirm"),
          type = "warning",
          html = TRUE
        )
      )
    }
  })

  # disable the confirm button if the user name is missing
  observe({
    if (!is.null(input$user_name)) {
      shinyjs::toggleState(
        selector = "button.swal-button.swal-button--confirm",
        condition = input$user_name != ""
      )
    }
  })

  # when the user is registered, set logged to TRUE
  observeEvent(input$register_user,{
    if (input$user_name != "") {
      events$logged <- TRUE
    }
  })

  # # shift stop when countdown is 0
  # observe({
  #   if (countdown() <= 0)
  #     events$stop <- TRUE
  # })
  #
  # # When the timer is 0 the game is over if the user has no diagnosis
  # # and treatment
  # observe({
  #   if (is.null(input$close_app)) {
  #     if (events$stop) {
  #       confirmSweetAlert(
  #         inputId = "close_app",
  #         danger_mode = TRUE,
  #         session,
  #         title = "This is the end!",
  #         text = tagList(
  #           img(src = "interface_img/finish.svg", width = "100px", height = "100px"),
  #           br(),
  #           HTML(
  #             paste(
  #               "It seems that the game is finished.
  #               You can restart or close the game."
  #             )
  #           )
  #         ),
  #         btn_labels = c("Restart", "Stop"),
  #         type = "error",
  #         html = TRUE
  #       )
  #     }
  #   }
  # })
  #
  # # Handle what happens when the user close or restart the app
  # observeEvent(input$close_app, {
  #   if (input$close_app) {
  #     sendSweetAlert(
  #       session,
  #       title = "Stop in 5 seconds...",
  #       type = "error"
  #     )
  #     shinyjs::delay(5000, {
  #       js$closeWindow()
  #       stopApp()
  #     })
  #   } else {
  #     session$reload()
  #   }
  # })


  # init the directory where user datas will be saved
  observeEvent(input$register_user, {
    req(input$register_user)
      # create the new folder
      dir.create(user_folder())
  })


  # # give the user the opportunity to load a previous session
  # observeEvent(input$register_user, {
  #   user_folder <- paste0(getwd(), "/www/users_datas/")
  #   file_list <- as.vector(list.files(user_folder))
  #
  #   confirmSweetAlert(
  #     session,
  #     danger_mode = TRUE,
  #     inputId = "load_previous_session",
  #     title = "Want to load an older session?",
  #     text = tagList(
  #       column(
  #         width = 12,
  #         align = "center",
  #         prettyRadioButtons(
  #           inputId = "old_session",
  #           label = "Choose a saved session:",
  #           choices = file_list,
  #           animation = "pulse",
  #           status = "info"
  #         )
  #       )
  #     ),
  #     btn_labels = c("Cancel", "Load"),
  #     type = "warning",
  #     html = TRUE
  #   )
  # })
  #
  # # load the previous session
  # observeEvent(input$load_previous_session, {
  #   if (input$load_previous_session) {
  #     user_folder <- paste0(getwd(), "/www/users_datas/")
  #     temp_folder <- paste0(user_folder, input$old_session)
  #     file_list <- list.files(temp_folder)
  #     lapply(1:length(file_list), FUN = function(i) {
  #       print(paste0(temp_folder, "/", file_list[[i]]))
  #       readRDS(file = paste0(temp_folder, "/", file_list[[i]]))
  #     })
  #
  #     # replace start_time by the value of when the folder was first created
  #     start_time <- unlist(str_split(input$old_session, "-", n = 2))[[2]]
  #   }
  # })


  # handle case when the use press the diagnosis button
  observeEvent(input$diagnosis, {
    confirmSweetAlert(
      session,
      inputId = "diagnosis_answer",
      title = "What is the disease of this patient?",
      btn_labels = c("Send"),
      type = "warning",
      text = tagList(
        column(
          align = "center",
          width = 12,
          selectInput(
            inputId = "disease_name",
            label = "",
            choices = diseases_list,
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          )
        )
      ),
      html = TRUE
    )
  })

  # treat the diagnosis answer
  observeEvent(input$diagnosis_answer, {
    user_answer <- input$disease_name
    if (user_answer != "") {
      test <- str_detect(answer, regex(paste0("\\b", user_answer, "\\b"), ignore_case = TRUE))
      if (test) {
        events$answered <- TRUE
        sendSweetAlert(
          session,
          title = paste0("Congratulations ", input$user_name, " !"),
          text = HTML(
            paste0(
              "This patient has,", answer,
              "It would be better to treat him now. Remember you have
              <b>15 minutes</b> to complete this activity."
            )
          ),
          type = "success",
          html = TRUE
        )
      } else {
        events$answered <- FALSE
        sendSweetAlert(
          session,
          title = "Wasted!",
          text = paste0(input$user_name, ", it seems that your answer is wrong!"),
          type = "error"
        )
      }

      # save the answer status
      saveRDS(
        object = c(events$answered, user_answer),
        file = paste0(user_folder(), "/user_answer.rds")
      )
    } else {
      sendSweetAlert(
        session,
        title = "Missing diagnosis!",
        text = paste0(input$user_name, ", it seems that your answer is empty!"),
        type = "error"
      )
    }
  })

  # prevent the user from resubmitting an answer if he correctly guessed
  # the patient disease
  observe({
    if (!is.null(events$answered)) {
      if (events$answered) {
        shinyjs::disable("diagnosis")
      }
    }
  })

  # a label to indicate the user whether the diagnosis is ok or not
  # in the header
  output$user_game_status <- renderUI({
    game_status <- if (!is.null(events$answered)) {
      if (events$answered) "success" else "danger"
    } else {
      "warning"
    }
    game_text <- if (!is.null(events$answered)) {
      if (events$answered)
        paste0(input$disease_name, ": successful diagnosis")
      else paste0(input$disease_name, ": unsuccessful diagnosis")
    } else {
      "No diagnosis yet"
    }
    div(
      style = "margin-top: 7.5px; margin-left: 10px;",
      class = "diagnosis-badge",
      bs4Badge(
        game_text,
        status = game_status,
        rounded = TRUE,
        position = "left"
      )
    )
  })

  # Give users the opportunity to save data
  output$download_logs <- downloadHandler(
    filename = function() paste0(input$user_name, "_logs.rds"),
    content = function(file) {
      saveRDS(
        list(
          my_events = events$history,
          my_comments = comments$history,
          my_answer = c(events$answered, input$disease_name)
        ),
        file
      )
    }
  )

  # clean all empty folders when the application starts
  observe({
    dir_list <- list.dirs(users_logs)
    if (length(dir_list) > 1) {
      lapply(2:length(dir_list), FUN = function(i) {
        temp_dir <- dir_list[[i]]
        temp_file_list <- list.files(temp_dir)
        if (length(temp_file_list) == 0) unlink(x = temp_dir, recursive = TRUE)
      })
    }
  })

  #-------------------------------------------------------------------------
  # Calcium/PTH/D3/FGF3 feedback: give the user some feedback
  # regarding the current state of the app
  #
  #-------------------------------------------------------------------------

  # how to use the notebook
  observe({
    if (!is_empty(input$register_user)) {
      shinyjs::delay(
        1000,
        confirmSweetAlert(
          session,
          inputId = "diagnosis_intro",
          title = "How to use the notebook?",
          text = tagList(
            img(src = "interface_img/notebook.svg", width = "100px", height = "100px"),
            br(),
            HTML("A serie of questions will help you during
                 the diagnostic process. Click on <img src='interface_img/next.svg' height='50' width='50'>
                 to go through the questions. Once you completed all questions,
                 submit your diagnosis by clicking on
                 <img src='interface_img/diagnosis.svg' height='70' width='70'>.")
          ),
          btn_labels = c(NULL, "Ok"),
          type = "warning",
          html = TRUE
        )
      )
    }
  })

  # Introduction to plasma analysis
  observeEvent(input$user_add_comment, {
    if (events$animation == 3) {
      confirmSweetAlert(
        session,
        inputId = "plasma_analysis_intro",
        title = "How to deal with plasma analysis?",
        text = tagList(
          img(src = "CaPO4_network/plasma.svg", width = "100px", height = "100px"),
          br(),
          "You can access any plasma concentration by clicking on the",
          img(src = "CaPO4_network/plasma.svg", width = "50px", height = "50px"),
          " node. Besides, other compartments are available such as",
          img(src = "CaPO4_network/parathyroid_gland_human.svg", width = "50px", height = "50px"),
          img(src = "CaPO4_network/cells.svg", width = "50px", height = "50px"),
          img(src = "CaPO4_network/bone.svg", width = "50px", height = "50px"),
          "and", img(src = "CaPO4_network/rapid-bone.svg", width = "50px", height = "50px")
        ),
        btn_labels = c(NULL, "Ok"),
        type = "warning",
        html = TRUE
      )
    }
  })

  # Introduction to treatments
  observeEvent(input$diagnosis_answer, {
    if (events$animation == 8) {
      shinyjs::delay(
        1000,
        confirmSweetAlert(
          session,
          inputId = "treatments_intro",
          title = "How to deal with treatments?",
          text = tagList(
            img(src = "treatments_img/pills.svg", width = "100px", height = "100px"),
            br(),
            column(
              width = 12,
              align = "center",
              HTML(
                "Now that you have posed your initial diagnostic, you may explore different treatment options.
                For each:
                <ol>
                <li> Select the treatment in the timeline </li>
                <li> Specify dosage and duration (if relevant) </li>
                <li> Click on <img src='interface_img/add_treatment.svg' height='50' width='50'>
                to add the treatment</li>
                <li> Click on <img src='interface_img/run.svg' height='50' width='50'></li>
                <li> You may visualize changes due to your last intervention in the top right panel </li>
                <li> To visualize the entire simulation history, click on
                <img src='interface_img/summary.svg' height='70' width='70'></li>
                </ol>
                You can perform several treatments. Note that interventions cannot
                be erased from the timeline (i.e. you cannot go back in time).
                But you can always start over and explore a different approach.
                "
              )
            )
          ),
          btn_labels = c(NULL, "Ok"),
          type = "warning",
          html = TRUE
        )
      )
      # increament by 1 to prevent this alert
      # from being displayed each time since
      # the button is hidden when equal to 8
      events$animation <- events$animation + 1
    }
  })

  # increase the animation counter by 1 each time a new comment
  # is added by the user
  observeEvent(input$user_add_comment, {
    events$animation <- events$animation + 1
  })

  # say that the animation is started when the user has clicked on next
  observeEvent(events$animation , {
    if (events$animation == 1) {
      events$animation_started <- TRUE
    }
  })

  # # warn the user when Calcium, PTH, vitamin D3 are above their physiological ranges
  # observe({
  #   out <- out()
  #   # event only triggered if the user is logged in
  #   if (events$logged) {
  #
  #     # Calcium conditions
  #     Cap_range <- (out[, "Ca_p"] > 1.1 && out[, "Ca_p"] < 1.3)
  #     # Pi conditions
  #     PO4p_range <- (out[, "PO4_p"] > 0.8 && out[, "PO4_p"] < 1.5)
  #     # PTH conditions
  #     PTHp_range <- (out[, "PTH_p"] > 8 && out[, "PTH_p"] < 51)
  #     # D3 conditions
  #     D3p_range <- (out[, "D3_p"] > 80 && out[, "D3_p"] < 700)
  #     # FGF23 conditions
  #     FGFp_range <- (out[, "FGF_p"] > 12 && out[, "FGF_p"] < 21)
  #
  #     if (!Cap_range) {
  #       patient_feedback <- paste0(
  #         patient_feedback, p(" [Ca2+]p is out of bounds", class = "text-danger")
  #       )
  #     }
  #     if (!PO4p_range) {
  #       patient_feedback <- paste0(
  #         patient_feedback, p(" [Pi]p is out of bounds", class = "text-danger")
  #       )
  #     }
  #     if (!PTHp_range) {
  #       patient_feedback <- paste0(
  #         patient_feedback, p(" [PTH]p is out of bounds", class = "text-danger")
  #       )
  #     }
  #     if (!D3p_range) {
  #       patient_feedback <- paste0(
  #         patient_feedback, p(" [D3]p is out of bounds", class = "text-danger")
  #       )
  #     }
  #     if (!FGFp_range) {
  #       patient_feedback <- paste0(
  #         patient_feedback, p(" [FGF23]p is out of bounds", class = "text-danger")
  #       )
  #     }
  #
  #     # send the alert message with all feedbacks
  #     sendSweetAlert(
  #       session,
  #       title = paste0("Oups ", input$user_name, " !"),
  #       text = HTML(paste0(
  #         "It seems that: ", patient_feedback,
  #         "You should do something!")
  #       ),
  #       type = "warning",
  #       html = TRUE
  #     )
  #   }
  # })

  # output$current_calcium <- renderUI({
  #   Ca_p <- round(out()[, "Ca_p"], 2)
  #   if (Ca_p > 1.1 && Ca_p < 1.3) {
  #     p(Ca_p)
  #   } else if (Ca_p < 1.1) {
  #     p(class = "text-danger", paste0("$$[Ca]$$"))
  #   } else {
  #     p(class = "text-success", Ca_p)
  #   }
  # })

  #-------------------------------------------------------------------------
  # sidebar User panel: print name and date
  #
  #-------------------------------------------------------------------------

  output$user_panel <- renderUI({
    # use invalidate later to simulate a clock
    invalidateLater(1000)
    bs4SidebarUserPanel(
      text = tags$small(paste(input$user_name, Sys.time())),
      img = "https://image.flaticon.com/icons/svg/305/305983.svg"
    )
  })

  #-------------------------------------------------------------------------
  # Handle user comments
  #
  #-------------------------------------------------------------------------

  # create the comment dataframe to store all comments
  comments <- reactiveValues(
    history = data.frame(
      description = NULL,
      date = NULL,
      stringsAsFactors = FALSE
    )
  )

  # each time the user add a new comment, add it to the table
  observeEvent(input$user_add_comment, {
    if (!is.null(input$user_comment)) {
      temp_comment <- data.frame(
        description = input$user_comment,
        date = Sys.time(),
        stringsAsFactors = FALSE
      )
      comments$history <- rbind(comments$history, temp_comment)
    }
  })


  #-------------------------------------------------------------------------
  #  This part handle events, plasma analysis, triggered by the user
  #  as well as the export function to save the timeline Event
  #
  #-------------------------------------------------------------------------

  # Set events parameters in reactiveValues so as to modify them later
  # history stores all events whereas current correspond to the last called
  # event in the stack
  events <- reactiveValues(
    history = data.frame(
      id = NULL,
      real_time = NULL,
      event = NULL,
      rate = NULL,
      start_time = NULL,
      stop_time = NULL,
      status = NULL,
      stringsAsFactors = FALSE
    ),
    current = data.frame(
      id = NULL,
      real_time = NULL,
      event = NULL,
      rate = NULL,
      start_time = NULL,
      stop_time = NULL,
      status = NULL,
      stringsAsFactors = FALSE
    ),
    counter = 1,
    stop = FALSE,
    answered = NULL,
    PTX = FALSE,
    logged = FALSE,
    animation = 0,
    animation_started = FALSE
  )


  # handle plasma analysis history
  plasma_analysis <- reactiveValues(history = data.frame(stringsAsFactors = FALSE))

  observeEvent(input$current_node_id, {
    node_id <- input$current_node_id
    if (node_id == 2) {
      temp_plasma_analysis <- out()[nrow(out()), -1]
      plasma_analysis$history <- rbind(plasma_analysis$history, temp_plasma_analysis)
    }
  })

  observeEvent(input$add_treatment, {
    if (!is.null(input$add_treatment)) {
      # prevent plasma analysis from being done when PTX was already
      # performed before
      if (input$treatment_selected == "PTX" && isTRUE(events$PTX)) {
        NULL
      } else {
        temp_plasma_analysis <- out()[nrow(out()), -1]
        plasma_analysis$history <- rbind(plasma_analysis$history, temp_plasma_analysis)
      }
    }
  })

  # generate the slider corresponding to a given treatment
  output$sliderInject <- renderUI({
    req(input$treatment_selected)
    generate_slider_events(input)
  })

  # plasma analysis events
  observeEvent(input$current_node_id, {
    node_id <- input$current_node_id
    if (node_id == 2) {
      if (nrow(events$history) == 0) {
        temp_event <- data.frame(
          id = events$counter,
          real_time = Sys.time(),
          event = "plasma analysis",
          rate = "undefined",
          start_time = "undefined",
          stop_time = "undefined",
          status = "active",
          stringsAsFactors = FALSE
        )
      } else {
        temp_event <- data.frame(
          id = events$counter,
          real_time = if (events$history[nrow(events$history), "event"] == "PTX" ||
                          events$history[nrow(events$history), "event"] == "plasma analysis") {
            events$history[nrow(events$history), "real_time"]
            # need to wait before the end of the previous event
          } else {
            # calculate the time difference between the previous event
            # end and when the user press the add event button
            dt <- difftime(
              time1 = Sys.time(),
              time2 = events$history[nrow(events$history), "real_time"] +
                as.numeric(events$history[nrow(events$history), "stop_time"]),
              units = c("mins"),
              tz = Sys.timezone(location = TRUE)
            )
            # if the user press before the previous event is finished
            # we consider that the next event happens just after
            if (dt <= 0) {
              events$history[nrow(events$history), "real_time"] +
                as.numeric(events$history[nrow(events$history), "stop_time"])
              # otherwise, we consider the elapsed time plus the time
              # that takes the event (t_stop)
            } else {
              Sys.time()
            }
          },
          event = "plasma analysis",
          rate = "undefined",
          start_time = "undefined",
          stop_time = "undefined",
          status = "active",
          stringsAsFactors = FALSE
        )
      }
      events$history <- rbind(events$history, temp_event)
      events$counter <- events$counter + 1
    }
  })

  # Add treatments to the event list
  observeEvent(input$add_treatment, {
    # the same treatment can be added
    # multiple times. However, parathyroidectomy
    # cannot be performed more than once
    if (input$treatment_selected != "PTX") {
      if (nrow(events$history) == 0) {
        temp_event <- data.frame(
          id = events$counter,
          real_time = Sys.time(),
          event = input$treatment_selected,
          rate = if (!(input$treatment_selected %in%
                       c("bisphosphonate", "furosemide", "cinacalcet"))) {
            input[[paste(input$treatment_selected)]]
          } else {
            "undefined"
          },
          start_time = 0,
          stop_time = input$t_stop,
          status = "active",
          stringsAsFactors = FALSE
        )
      } else {
        temp_event <- data.frame(
          id = events$counter,
          # if PTX was performed before, we do not need to wait
          real_time = if (events$history[nrow(events$history), "event"] == "PTX" ||
                          events$history[nrow(events$history), "event"] == "plasma analysis") {
            events$history[nrow(events$history), "real_time"]
            # need to wait before the end of the previous event
          } else {
            # calculate the time difference between the previous event
            # end and when the user press the add event button
            dt <- difftime(
              time1 = Sys.time(),
              time2 = events$history[nrow(events$history), "real_time"] +
                as.numeric(events$history[nrow(events$history), "stop_time"]),
              units = c("mins"),
              tz = Sys.timezone(location = TRUE)
            )
            # if the user press before the previous event is finished
            # we consider that the next event happens just after
            if (dt <= 0) {
              events$history[nrow(events$history), "real_time"] +
                as.numeric(events$history[nrow(events$history), "stop_time"]) +
                input$t_stop
              # otherwise, we consider the elapsed time plus the time
              # that takes the event (t_stop)
            } else {
              Sys.time() + input$t_stop
            }
          },
          event = input$treatment_selected,
          rate = if (!(input$treatment_selected %in%
                       c("bisphosphonate", "furosemide", "cinacalcet"))) {
            input[[paste(input$treatment_selected)]]
          } else {
            "undefined"
          },
          start_time = 0,
          stop_time = input$t_stop,
          status = "active",
          stringsAsFactors = FALSE
        )
      }
      events$history <- rbind(events$history, temp_event)
      events$counter <- events$counter + 1
      events$current <- temp_event
    } else {
      if (!isTRUE(events$PTX)) {
        if (nrow(events$history) == 0) {
          temp_event <- data.frame(
            id = events$counter,
            real_time = Sys.time(),
            event = input$treatment_selected,
            rate = "undefined",
            start_time = "undefined",
            stop_time = "undefined",
            status = "active",
            stringsAsFactors = FALSE
          )
        } else {
          temp_event <- data.frame(
            id = events$counter,
            # if PTX was performed before, we do not need to wait
            real_time = if (events$history[nrow(events$history), "event"] == "plasma analysis") {
              events$history[nrow(events$history), "real_time"]
              # need to wait before the end of the previous event
            } else {
              # calculate the time difference between the previous event
              # end and when the user press the add event button
              dt <- difftime(
                time1 = Sys.time(),
                time2 = events$history[nrow(events$history), "real_time"] +
                  as.numeric(events$history[nrow(events$history), "stop_time"]),
                units = c("mins"),
                tz = Sys.timezone(location = TRUE)
              )
              # if the user press before the previous event is finished
              # we consider that the next event happens just after
              if (dt < 0) {
                events$history[nrow(events$history), "real_time"] +
                  as.numeric(events$history[nrow(events$history), "stop_time"])
                # otherwise, we consider the elapsed time plus the time
                # that takes the event (t_stop)
              } else {
                Sys.time()
              }
            },
            event = input$treatment_selected,
            rate = "undefined",
            start_time = "undefined",
            stop_time = "undefined",
            status = "active",
            stringsAsFactors = FALSE
          )
        }
        events$history <- rbind(events$history, temp_event)
        events$counter <- events$counter + 1
        events$PTX <- TRUE
      } else {
        showNotification(
          "Cannot perform parathyroidectomy more than once!",
          type = "error",
          closeButton = TRUE
        )
      }
    }
  })

  # flush the stack of current events
  # 5 seconds after the user click on run
  observeEvent(input$run, {
    shinyjs::delay(1000, {
      events$current <- data.frame(
        id = NULL,
        real_time = NULL,
        event = NULL,
        rate = NULL,
        start_time = NULL,
        stop_time = NULL,
        status = NULL,
        stringsAsFactors = FALSE
      )
    })
  })

  #-------------------------------------------------------------------------
  #
  #  Integrate equations using deSolve package to generate table
  #  out is a reactive intermediate component that is called by
  #  to make plots or other stuffs. We used the compiled version of
  #  the code, to make computations faster
  #
  #-------------------------------------------------------------------------

  # will be used the save all out elements
  out_history <- reactiveValues(
    item = list(),
    counter = 0,
    summary = data.frame()
  )

  out <- reactive({
    input$run
    isolate({
      parameters <- parameters()
      times <- times()
      # always solve from the last state
      as.data.frame(
        ode(
          # when opening the application, y will be state_0 since states$val
          # is an empty list. However, for the next runs, states$val is
          # populated with the last simulated final state and so on
          # each time the user press run
          y = if (is_empty(states$val)) {
            patient_state_0
          } else {
            states$val[[length(states$val)]]
          },
          times = times,
          func = "derivs",
          parms = parameters,
          dllname = "compiled_core",
          initfunc = "initmod",
          nout = 33,
          outnames = c(
            "U_Ca", "U_PO4", "Abs_int_Ca",
            "Abs_int_PO4", "Res_Ca", "Res_PO4",
            "Ac_Ca", "Ac_PO4", "Reabs_Ca", "Reabs_PO4",
            "Ca_pf", "Ca_fp", "PO4_pf", "PO4_fp",
            "PO4_pc", "PO4_cp", "PTHg_synth",
            "PTHg_synth_D3", "PTHg_synth_PO4",
            "PTHg_exo_CaSR", "PTHg_deg", "PTHg_exo",
            "PTHp_deg", "Reabs_PT_PTH",
            "Reabs_TAL_CaSR", "Reabs_TAL_PTH",
            "Reabs_DCT_PTH", "Reabs_DCT_D3",
            "Abs_int_D3", "Res_PTH", "Res_D3",
            "Reabs_PT_PO4_PTH", "Reabs_PT_PO4_FGF"
          )
        )
      )
    })
  })

  # update initial conditions to the last state of the system each time an event
  # has occured. Need to delayed by the time needed for computation before updating
  # which is not really obvious since we don't know exactly what time it will take.
  observe({
    input$run
    shinyjs::delay(1000, {
      out <- out()
      temp_state <- c(
        "PTH_g" = out[nrow(out),"PTH_g"],
        "PTH_p" = out[nrow(out),"PTH_p"],
        "D3_p" = out[nrow(out),"D3_p"],
        "FGF_p" = out[nrow(out),"FGF_p"],
        "Ca_p" = out[nrow(out),"Ca_p"],
        "Ca_f" = out[nrow(out),"Ca_f"],
        "Ca_b" = out[nrow(out),"Ca_b"],
        "PO4_p" = out[nrow(out),"PO4_p"],
        "PO4_f" = out[nrow(out),"PO4_f"],
        "PO4_b" = out[nrow(out),"PO4_b"],
        "PO4_c" = out[nrow(out),"PO4_c"],
        "CaHPO4_p" = out[nrow(out),"CaHPO4_p"],
        "CaH2PO4_p" = out[nrow(out),"CaH2PO4_p"],
        "CPP_p" = out[nrow(out),"CPP_p"],
        "CaHPO4_f" = out[nrow(out),"CaHPO4_f"],
        "CaH2PO4_f" = out[nrow(out),"CaH2PO4_f"],
        "CaProt_p" = out[nrow(out),"CaProt_p"],
        "NaPO4_p" = out[nrow(out),"NaPO4_p"],
        "Ca_tot" = out[nrow(out),"Ca_tot"],
        "PO4_tot" = out[nrow(out),"PO4_tot"],
        "EGTA_p" = out[nrow(out),"EGTA_p"],
        "CaEGTA_p" = out[nrow(out),"CaEGTA_p"]
      )
      states$counter <- states$counter + 1
      states$val[[states$counter]] <- temp_state
      states$name <- input$treatment_selected
    })
  })


  # when the user clicks on summary rerun the simulation with all events
  observeEvent(input$summary, {
    showModal(
      modalDialog(
        title = fluidRow(
          column(
            width = 9,
            align = "left",
            p(style = "text-align: center;", "Overview of your patient")
          ),
          column(
            width = 3,
            align = "right",
            tags$button(
              type = "button",
              class = "btn btn-default float-right",
              `data-dismiss` = "modal",
              icon("close"),
              "Dismiss"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "center",
            bs4TabSetPanel(
              id = "tabset1",
              side = "left", # generate the 5 plots
              .list = lapply(1:length(summary_plot_names), FUN = function(i) {
                name <- summary_plot_names[[i]]
                bs4TabPanel(
                  tabName = name,
                  active = if (i == 1) TRUE else FALSE,
                  withSpinner(
                    plotlyOutput(paste0("plot_summary_", name)),
                    size = 2,
                    type = 8,
                    color = "#000000"
                  )
                )
              })
            )
          )
        ),
        size = "m",
        footer = NULL
      )
    )
  })

  # out_summary <- eventReactive(input$summary, {
  #   if (nrow(events$history) >= 2) {
  #     times <- as.list(events$history[, "real_time"])
  #     len <- length(times)
  #     delta_t <- lapply(2:len, FUN = function(i) {
  #       difftime(
  #         time1 = times[[i]],
  #         time2 = times[[i - 1]],
  #         units = c("secs"),
  #         tz = Sys.timezone(location = TRUE)
  #       )
  #     })
  #
  #
  #
  #   }
  #
  # })


  # cumulative datas
  datas_summary <- reactive({
    datas <- out_history$summary %>%
      filter(time %% 50 == 0) %>%
      accumulate_by(~time)

    # add bounds for each variable
    low_norm_Ca_p <- data.frame(low_norm_Ca_p = rep(1.1, length(datas[, "time"])))
    high_norm_Ca_p <- data.frame(high_norm_Ca_p = rep(1.3, length(datas[, "time"])))
    low_norm_PO4_p <- data.frame(low_norm_PO4_p = rep(0.8, length(datas[, "time"])))
    high_norm_PO4_p <- data.frame(high_norm_PO4_p = rep(1.5, length(datas[, "time"])))
    low_norm_PTH_p <- data.frame(low_norm_PTH_p = rep(1.5, length(datas[, "time"])))
    high_norm_PTH_p <- data.frame(high_norm_PTH_p = rep(7, length(datas[, "time"])))
    low_norm_D3_p <- data.frame(low_norm_D3_p = rep(50, length(datas[, "time"])))
    high_norm_D3_p <- data.frame(high_norm_D3_p = rep(180, length(datas[, "time"])))
    low_norm_FGF_p <- data.frame(low_norm_FGF_p = rep(8, length(datas[, "time"])))
    high_norm_FGF_p <- data.frame(high_norm_FGF_p = rep(51, length(datas[, "time"])))

    # bind all values
    datas <- cbind(
      datas,
      low_norm_Ca_p,
      high_norm_Ca_p,
      low_norm_PO4_p,
      high_norm_PO4_p,
      low_norm_PTH_p,
      high_norm_PTH_p,
      low_norm_D3_p,
      high_norm_D3_p,
      low_norm_FGF_p,
      high_norm_FGF_p
    )
  })

  # cumulative plot (5 plots)
  lapply(1:length(summary_plot_names), FUN = function(i) {
    name <- summary_plot_names[[i]]
    output[[paste0("plot_summary_", name)]] <- renderPlotly({
      if (nrow(out_history$summary) >= 1) {
        plot_ly(
          datas_summary(),
          x = datas_summary()[, "time"],
          y = if (name == "PTH_p") {
            datas_summary()[, name] * 100
          } else if (name == "D3_p") {
            datas_summary()[, name] / 4
          } else if (name == "FGF_p") {
            datas_summary()[, name] / 16.8 * 32
          } else {
            datas_summary()[, name]
          },
          name = if (name %in% c("Ca_p", "PO4_p")) {
            paste0(name, " (mM)")
          } else if (name == "FGF_p") {
            paste0(name, " (pg/mL)")
          } else {
            paste0(name, " (pM)")
          },
          frame = ~frame,
          type = 'scatter',
          mode = 'lines',

          line = list(
            simplyfy = FALSE,
            color = if (name == "Ca_p") {
              'rgb(27, 102, 244)'
            } else if (name == "PO4_p") {
              'rgb(244, 27, 27)'
            } else {
              'black'
            }
          )
        ) %>%
          add_lines(
            y = datas_summary()[, paste0("low_norm_", name)],
            frame = ~frame,
            name = if (name %in% c("Ca_p", "PO4_p")) {
              paste0("Low ", name, " bound (mM)")
            } else {
              paste0("Low ", name, " bound (pM)")
            },
            line = list(
              color = 'rgb(169,169,169)',
              width = 4,
              dash = 'dash'
            )
          ) %>%
          add_lines(
            y = datas_summary()[, paste0("high_norm_", name)],
            frame = ~frame,
            name = if (name %in% c("Ca_p", "PO4_p")) {
              paste0("High ", name, " bound (mM)")
            } else {
              paste0("High ", name, " bound (pM)")
            },
            line = list(
              color = 'rgb(169,169,169)',
              width = 4,
              dash = 'dot'
            )
          ) %>%
          layout(
            xaxis = list(
              title = "time (min)",
              zeroline = FALSE
            ),
            yaxis = list(
              title = if (name %in% c("Ca_p", "PO4_p")) {
                paste0(name, " (mM)")
              } else {
                paste0(name, " (pM)")
              },
              zeroline = FALSE
            ),
            showlegend = if (input$isMobile) FALSE else TRUE
          ) %>%
          animation_opts(
            # animation speed (the lower, the faster)
            frame = 5,
            transition = 0,
            redraw = FALSE
          ) %>%
          animation_slider(
            hide = FALSE
          ) %>%
          config(displayModeBar = FALSE)
      }
    })
  })

  # each time the user click on run, the history is saved
  observeEvent(input$run, {
    out <- out()
    len <- length(out_history$item)
    if (len >= 1) {
      # translate all time by the number of time points
      # in the previous run + 1
      out_history$counter <- out_history$counter + nrow(out_history$item[[len]])
      out[, "time"] <- out[, "time"] + out_history$counter
    }
    out_history$item[[len + 1]] <- out
    # merge all dataframe into a big one
    out_history$summary <- bind_rows(out_history$item)
  })

  #-------------------------------------------------------------------------
  #
  #  The network part: make interactive diagramms of Ca and PO4 homeostasis
  #  as well as regulation by hormones such as PTH, vitamin D3 and FGF23
  #
  #-------------------------------------------------------------------------

  # Generate the CaP Graph network
  nodes_Ca <- reactive({generate_nodes_Ca(input)})
  edges_Ca <- reactive({generate_edges_Ca(input)})

  # Generate the output of the Ca graph to be used in body
  output$network_Ca <- renderVisNetwork({

    nodes_Ca <- nodes_Ca()
    edges_Ca <- edges_Ca()
    input$network_hormonal_choice

    generate_network(
      input,
      nodes = nodes_Ca,
      edges = edges_Ca,
      usephysics = TRUE
    ) %>%
      # simple click event to allow graph ploting
      visEvents(
        selectNode = "
          function(nodes) {
            Shiny.onInputChange('current_node_id', nodes.nodes);
          }"
      ) %>%
      # unselect node event
      visEvents(
        deselectNode = "
          function(nodes) {
            Shiny.onInputChange('current_node_id', 'null');
          }"
      ) %>%
      # add the doubleclick function to handle zoom views
      visEvents(
        doubleClick = "
          function(nodes) {
            Shiny.onInputChange('current_node_bis_id', nodes.nodes);
          }"
      ) %>%
      visEvents(
        selectEdge = "
          function(edges) {
            Shiny.onInputChange('current_edge_id', edges.edges);
          }"
      ) %>%
      visEvents(
        deselectEdge = "
          function(edges) {
            Shiny.onInputChange('current_edge_id', 'null');
          }"
      ) %>%
      # very important: change the whole graph position after drawing
      visEvents(
        type = "on",
        stabilized = "
          function() {
            this.moveTo({
              position: {x:0, y:-13.43},
              offset: {x: 0, y:0}
            });
          }"
      ) %>%
      # very important: allow to detect the web browser used by client
      # use before drawing the network. Works with find_navigator.js
      visEvents(
        type = "on",
        initRedraw = paste0("
          function() {
            this.moveTo({scale:", if (input$isMobile) 0.3 else 0.6, "});
        }")
      ) # to set the initial zoom (1 by default)
  })

  # Events for the CaPO4 Homeostasis diagramm whenever a flux change
  # Change arrow color relatively to the value of fluxes for Ca injection/PO4
  # injection as well as PO4 gavage
  observe({
    out <- out()
    edges_Ca <- edges_Ca()
    arrow_lighting_live(
      out,
      edges = edges_Ca,
      session,
      t_target = input$t_now
    )
  })


  # change the selected node size to better highlight it
  last <- reactiveValues(selected_node = NULL, selected_edge = NULL)

  observeEvent(input$current_node_id, {
    req(input$current_node_id)
    selected_node <- input$current_node_id
    nodes_Ca <- nodes_Ca()
    # javascript return null instead of NULL
    # cannot use is.null
    if (!identical(selected_node, "null")) {
      last$selected_node <- selected_node
      # organ nodes
      if (selected_node %in% c(1:5, 7:8, 11)) {
        nodes_Ca$size[selected_node] <- 100
        # Kidney zoom node
      } else if (selected_node == 6) {
        nodes_Ca$size[selected_node] <- 214
        # regulation nodes
      } else {
        nodes_Ca$size[selected_node] <- 57
      }
      visNetworkProxy("network_Ca") %>%
        visUpdateNodes(nodes = nodes_Ca)
      # reset the node size when unselected
    } else {
      if (last$selected_node %in% c(1:5, 7:8, 11)) {
        nodes_Ca$size[last$selected_node] <- 70
      } else if (last$selected_node == 6) {
        nodes_Ca$size[last$selected_node] <- 150
      } else {
        nodes_Ca$size[last$selected_node] <- 40
      }
      visNetworkProxy("network_Ca") %>%
        visUpdateNodes(nodes = nodes_Ca)
    }
  })

  # change the selected edge size to
  # better highlight it
  observeEvent(input$current_edge_id,{
    req(input$current_edge_id)
    selected_edge <- input$current_edge_id
    edges_Ca <- edges_Ca()
    edge_id <- match(selected_edge, edges_Ca$id)
    if (!identical(selected_edge, "null")) {
      last$selected_edge <- edge_id
      # organs edges
      if (edge_id %in% c(1:12)) {
        edges_Ca$width[edge_id] <- 24
        # regulations edges
      } else {
        edges_Ca$width[edge_id] <- 12
      }
      visNetworkProxy("network_Ca") %>%
        visUpdateEdges(edges = edges_Ca)
      # reset the edge size when unselected
    } else {
      if (edge_id %in% c(1:12)) {
        edges_Ca$width[edge_id] <- 8
      } else {
        edges_Ca$width[edge_id] <- 4
      }
      visNetworkProxy("network_Ca") %>%
        visUpdateEdges(edges = edges_Ca)
    }
  })


  # handle the size of organ and hormonal nodes
  output$size_nodes_organs <- renderUI({
    req(!is.null(input$isMobile))
    knobInput(
      "size_organs",
      "Organs",
      min = 50,
      max = 100,
      value = if (input$isMobile) 85 else 70,
      step = 5,
      displayPrevious = TRUE,
      fgColor = "#A9A9A9",
      inputColor = "#A9A9A9",
      skin = "tron",
      width = if (input$isMobile) "75px" else "100px",
      height = if (input$isMobile) "75px" else "100px"
    )
  })

  output$size_nodes_hormones <- renderUI({
    req(!is.null(input$isMobile))
    knobInput(
      "size_hormones",
      "Hormones",
      min = 20,
      max = 60,
      value = if (input$isMobile) 60 else 40,
      step = 5,
      displayPrevious = TRUE,
      fgColor = "#A9A9A9",
      inputColor = "#A9A9A9",
      skin = "tron",
      width = if (input$isMobile) "75px" else "100px",
      height = if (input$isMobile) "75px" else "100px"
    )
  })

  # control width of arrows
  output$width_arrows_organs <- renderUI({
    req(!is.null(input$isMobile))
    knobInput(
      "width_organs",
      "Organs",
      angleOffset = -90,
      angleArc = 180,
      min = 4,
      max = 14,
      value = 8,
      step = 1,
      displayPrevious = TRUE,
      fgColor = "#A9A9A9",
      inputColor = "#A9A9A9",
      skin = NULL,
      width = if (input$isMobile) "75px" else "100px",
      height = if (input$isMobile) "75px" else "100px"
    )
  })

  output$width_arrows_hormones <- renderUI({
    req(!is.null(input$isMobile))
    knobInput(
      "width_hormones",
      "Hormones",
      angleOffset = -90,
      angleArc = 180,
      min = 1,
      max = 8,
      value = 4,
      step = 1,
      displayPrevious = TRUE,
      fgColor = "#A9A9A9",
      inputColor = "#A9A9A9",
      skin = NULL,
      width = if (input$isMobile) "75px" else "100px",
      height = if (input$isMobile) "75px" else "100px"
    )
  })

  #-------------------------------------------------------------------------
  #
  #  The graph part: calls out(), parameters_bis()
  #  Interactive graph as a result of click on the diagram
  #
  #-------------------------------------------------------------------------

  # Generate a graph when node is clicked.
  # The graph corresponds to the node clicked
  output$plot_node <- renderPlotly({
    validate(need(input$current_node_id, "Select one node on the graph!"))
    out <- out()
    plot_node(input, node = input$current_node_id , out, parameters_fixed)
  })

  output$plot_edge <- renderPlotly({
    validate(need(input$current_edge_id, "Select one edge on the graph!"))
    out <- out()
    plot_edge(edge = input$current_edge_id , out)
  })

  #-------------------------------------------------------------------------
  #
  #  Handle dangerous parameter values by the user
  #
  #-------------------------------------------------------------------------

  # prevent the user to put infinite value in the max time of integration
  # With compiled code, tmax = 100000 min is a correct value
  observeEvent(input$tmax,{
    # critical value for tmax
    feedbackWarning(
      inputId = "tmax",
      show = !is.na(input$tmax),
      text = "tmax should exist and set between 1 and 100000."
    )

    # check if input tmax does not exists or is not numeric
    if (is.na(input$tmax)) {
      sendSweetAlert(
        session,
        title = "Ooops ...",
        text = "Invalid value: tmax should be set correctly.",
        type = "error"
      )
      reset("tmax") # value is reset
    } else {
      # if yes, check it is negative
      if (input$tmax <= 0) {
        sendSweetAlert(
          session,
          title = "Ooops ...",
          text = "Invalid value: tmax must be higher than 0.",
          type = "error"
        )
        reset("tmax") # value is reset
        # check whether it is too high
      } else if (input$tmax > 100000) {
        sendSweetAlert(
          session,
          title = "Ooops ...",
          text = "Invalid value: the maximum
                       time of simulation is too high!",
          type = "error"
        )
        reset("tmax") # value is reset
      }
    }
  })

  #-------------------------------------------------------------------------
  #
  #  Useful tasks such as save, reset, load ...
  #
  #-------------------------------------------------------------------------

  # reset parameters individually
  button_states <- reactiveValues(values = list())
  observeEvent(input$reset_t_now,{
    # call the function to reset the given slider
    sliders_reset(button_states, input)
  })

  # disable the summary button as long as input$run is lower than 1
  observe({
    if (!is.null(input$run)) {
      toggleState(id = "summary", condition = input$run >= 1)
    }
  })

  # make the run button blinking when a new event is added
  # but remove it when run is pressed
  observeEvent(input$add_treatment, {
    addClass(id = "run", class = "run_glowing_blue")
  })
  observeEvent(input$run, {
    removeClass(id = "run", class = "run_glowing_blue")
  })

  # make the Summary button blinking when run was pressed at least once
  observeEvent(input$run, {
    addClass(id = "summary", class = "run_glowing_purple")
  })
  observeEvent(input$summary, {
    removeClass(id = "summary", class = "run_glowing_purple")
  })

  # make the run button glowing when not clicked
  observeEvent(input$diagnosis_intro, {
    addClass(id = "user_add_comment", class = "run_glowing_green")
  })
  observeEvent(input$add_user_comment, {
    removeClass(id = "user_add_comment", class = "run_glowing_green")
  })

  # make diagnosis blinking when there remains 5 min
  # before the app close, only if it exists (if the user
  # never clicked on next, diagmosis does not exist!!!)
  # observe({
  #   if (countdown() <= 5) {
  #     if (!is_empty(input$diagnosis)) {
  #       if (input$diagnosis == 0) {
  #         addClass(id = "diagnosis", class = "run_glowing_blue")
  #       }
  #     }
  #   }
  # })
  observe({
    if (!is_empty(input$diagnosis)) {
      if (input$diagnosis > 0) {
        removeClass(id = "diagnosis", class = "run_glowing_blue")
      }
    }
  })

  # prevent user from selecting multiple treatments as the same time
  observe({
    if (!is.null(input$treatment_selected)) {
      treatment <- match.arg(input$treatment_selected, treatment_choices)
      idx <- match(input$treatment_selected, treatment_choices)
      other_treatments <- treatment_choices[-idx]
      lapply(seq_along(other_treatments), FUN = function(j) {
        disable(selector = paste0("#treatment_selected input[value='", other_treatments[[j]], "']"))
      })
    } else {
      enable(id = "treatment_selected")
    }
  })

  # display or not display the network background
  observe({
    # add invalidate later so that the background class is
    # applied after the application startup
    invalidateLater(1000, session)
    if (!is_empty(input$background_choice)) {
      if (input$background_choice == "rat") {
        addClass(id = "network_cap", class = "network_caprat")
        removeClass(id = "network_cap", class = "network_caphuman")
      } else {
        removeClass(id = "network_cap", class = "network_caprat")
        addClass(id = "network_cap", class = "network_caphuman")
      }
    } else {
      addClass(id = "network_cap", class = "network_capnone")
      removeClass(id = "network_cap", class = "network_caphuman")
      removeClass(id = "network_cap", class = "network_caprat")
    }
  })

  # prevent user from selecting multiple background
  observe({
    if (is.element("rat", input$background_choice) &&
        !is.element("human", input$background_choice)) {
      disable(selector = "#background_choice input[value='human']")
    } else {
      enable(selector = "#background_choice input[value='human']")
    }
    if (is.element("human", input$background_choice) &&
        !is.element("rat", input$background_choice)) {
      disable(selector = "#background_choice input[value='rat']")
    } else {
      enable(selector = "#background_choice input[value='rat']")
    }
  })

  # when enable regulation is selected, activates all the checkboxes
  # the reverse case does not work for unknow reason
  observeEvent(input$network_hormonal_choice, {
    if (input$network_hormonal_choice == TRUE) {
      updatePrettyCheckboxGroup(
        session,
        inputId = "network_Ca_choice",
        selected = c("Ca","PO4", "PTH", "D3", "FGF23")
      )
    }
  })
}
