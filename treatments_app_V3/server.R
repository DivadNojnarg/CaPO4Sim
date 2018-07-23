#-------------------------------------------------------------------------
#  This application is a R-Shiny implementation of a calcium and phosphate 
#  homeostasis model. It aims at being used by medical students but also
#  researchers. See https://divadnojnarg.github.io for more informations
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  #------------------------------------------------------------------------- 
  #  useful datas: initialization. These datq are not in global.R since
  #  they are some time reloaded by the program. In global.R they would not
  #  be reloaded, which would corrupt the new session
  #  
  #-------------------------------------------------------------------------
  
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
  }
  
  # below is needed to handle treatments events
  treatment_choices <- c(
    "PTX",
    "D3_inject",
    "Ca_food",
    "Ca_inject",
    "P_food",
    "P_inject",
    "cinacalcet"
  )
  
  # initialization of event parameters
  t_start <- NULL
  t_stop <- NULL
  
  # inititalization of the timer
  minutes_time <- 15 # the application will stop in 15 minutes
  start_time <- Sys.time()
  end_time <- start_time + minutes_time * 60
  
  #------------------------------------------------------------------------- 
  #  Store times, state and parameters in reactive values that can
  #  react to user inputs
  #  
  #-------------------------------------------------------------------------
  
  # Basic reactive expressions needed by the solver
  times <- reactive({seq(0,input$tmax, by = 1)})
  
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
      "D3_inact" = ifelse(patient_disease == "hypoD3", 0, 2.5e-005)
    )
  })
  
  
  # make a vector of disease related parameters, 
  # fixed_parameters and parameters related to events
  parameters <- reactive({
    c(parameters_disease(), parameters_fixed, parameters_event()) 
  }) 
  
  #------------------------------------------------------------------------- 
  #  Render Patient boxes: patient_info, 
  #  medical_history and the timeline events
  #  
  #-------------------------------------------------------------------------
  
  # patient info box
  output$patient_info <- renderUI({
    boxPlus(
      width = 12, 
      solidHeader = FALSE, 
      status = "primary", 
      collapsible = TRUE,
      boxProfile(
        src = patient_datas$picture,
        title = patient_datas$name,
        subtitle = NULL,
        boxProfileItemList(
          bordered = FALSE,
          boxProfileItem(
            title = "Age",
            description = patient_datas$age
          ),
          boxProfileItem(
            title = "Height",
            description = patient_datas$height
          ),
          boxProfileItem(
            title = "Weight",
            description = patient_datas$weight
          )
        ),
        column(
          width = 12,
          align = "center",
          actionBttn(
            inputId = "diagnosis",
            size = "lg",
            label = "Diagnose patient!",
            style = "fill",
            color = "primary",
            icon = icon("search")
          )
        )
      )
    )
  })
  
  # the user notebook
  output$user_notebook <- renderUI({
    
    comments <- comments$history
    len <- nrow(comments)
  
    socialBox(
      width = 12, 
      style = "overflow-y: auto; max-height: 400px;",
      title = paste0(input$user_name, "'s notebook"),
      subtitle = start_time,
      src = "https://image.flaticon.com/icons/svg/305/305983.svg",
      textAreaInput(
        inputId = "user_comment", 
        label = "My Comment", 
        value = "I enter here all my observations!"
      ),
      column(
        width = 12,
        align = "center",
        actionBttn(
          inputId = "user_add_comment",
          size = "xs",
          icon = icon("plus"),
          style = "material-circle",
          color = "success"
        )
      ),
      hr(),
      comments = if (len > 0) {
        tagList(
          lapply(1:len, FUN = function(i) {
            boxComment(
              src = "https://image.flaticon.com/icons/svg/305/305983.svg",
              title = paste0("Comment ", i),
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
  })
  
  # patient medical history
  output$patient_history <- renderUI({
    medical_history <- patient_datas$medical_history
    len <- length(medical_history$pathologies)
    
    boxPlus(
      width = 12, 
      solidHeader = FALSE, 
      status = "primary",
      title = "Medical History", 
      collapsible = TRUE,
      enable_label = TRUE,
      label_text = len,
      label_status = "danger",
      
      lapply(1:len, FUN = function(i){
        userPost(
          id = i,
          src = medical_history$doctors_avatars[[i]],
          author = medical_history$doctors[[i]],
          description = strong(medical_history$pathologies[[i]]),
          paste(medical_history$disease_description[[i]]),
          if (!is.null(medical_history$disease_image[[i]])) {
            userPostMedia(
              src = medical_history$disease_image[[i]],
              width = "300", 
              height = "300"
            )
          },
          userPostToolItemList(
            userPostToolItem(
              dashboardLabel(
                medical_history$examination_dates[[i]], 
                status = "warning"
              ), 
              side = "right"
            )
          ),
          br()
        )
      })
    )
  })
  
  
  # Event to be added in the timeLine
  output$recent_events <- renderUI({
    len <- nrow(events$history)
    name <- events$history$event
    start_time <- events$history$real_time
    rate <- events$history$rate
    plasma_values <- plasma_analysis$history
    
    withMathJax(
      boxPlus(
        width = 12, 
        solidHeader = FALSE, 
        status = "primary",
        collapsible = TRUE,
        enable_label = TRUE,
        label_text = len,
        label_status = "danger",
        style = "overflow-y: scroll;",
        title = "Recent Events",
        
        # treatments input are
        # in the event box
        prettyCheckboxGroup(
          inputId = "treatment_selected",
          label = "Select a new treatment:",
          choices = c(
            "parathyroid surgery" = "PTX",
            "D3 iv injection" = "D3_inject",
            "Ca supplementation" = "Ca_food",
            "Ca iv injection" = "Ca_inject",
            "Pi iv injection" = "P_inject",
            "Pi supplementation" = "P_food",
            "cinacalcet" = "cinacalcet"
          ),
          thick = TRUE,
          inline = TRUE,
          animation = "pulse"
        ),
        uiOutput(outputId = "sliderInject"),
        hr(),
        
        if (len > 0) {
          timelineBlock(
            style = "height: 400px;",
            timelineStart(color = "danger"),
            br(),
            lapply(1:len, FUN = function(i){
              tagAppendAttributes(
                timelineItem(
                  title = name[[i]],
                  icon = "medkit",
                  color = "orange",
                  time = dashboardLabel(
                    style = "default", 
                    status = "warning", 
                    start_time[[i]]
                  ),
                  timelineItemMedia(
                    src = if (name[[i]] %in% c("D3_inject", "Ca_inject", "P_inject")) {
                      "treatments_img/syringe.svg"
                    } else if (name[[i]] %in% c("Ca_food", "P_food")) {
                      "treatments_img/medicine.svg"
                    } else if (name[[i]] == "PTX") {
                      "treatments_img/surgery.svg"
                    } else if (name[[i]] == "cinacalcet") {
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
                      paste0("$$[Ca^{2+}_p] = ", round(plasma_values[i, "Ca_p"], 2), " mM [1.1-1.3 mM]$$"),
                      paste0("$$[P_i] = ", round(plasma_values[i, "PO4_p"], 2), " mM [0.8-1.5 mM]$$"),
                      paste0("$$[PTH_p] = ", round(plasma_values[i, "PTH_p"]*100), " pM [8-51 pM]$$"),
                      paste0("$$[D3_p] = ", round(plasma_values[i, "D3_p"]), " pM [80-700 pM]$$"),
                      paste0("$$[FGF23_p] = ", round(plasma_values[i, "FGF_p"], 2), " pM [12-21 pM]$$")
                    )
                  },
                  footer = if (!is.null(name[[i]])) {
                    if (name[[i]] != "PTX") 
                      if (!(name[[i]] %in% c("PTX", "plasma analysis"))) {
                        dashboardLabel(status = "danger", rate[[i]])
                      }
                    else NULL
                  }
                ),
                align = "middle"
              )
            }),
            br(),
            timelineEnd(color = "gray")
          )
        }
      )
    )
  })
  
  
  #------------------------------------------------------------------------- 
  #  Javascript alerts: to give instructions to users, handle when the
  #  game ends
  #  
  #-------------------------------------------------------------------------
  
  # counter to trigger sweetAlert R
  counter <- reactiveValues(alert = 0)
  
  
  # set up a timer during which user have to finish the game
  # and generate the related progress bar
  countdown <- reactive({
    invalidateLater(1000, session)
    countdown <- as.numeric(round(end_time - Sys.time()))
  })
  
  percent_countdown <- reactive({
    countdown() / minutes_time * 100
  })
    
  # render the progress bar for countdown
  output$currentTime <- renderUI({
    countdown <- countdown()
    percent_countdown <- percent_countdown()
    statusClass <- if (66 < percent_countdown & percent_countdown <= 100) {
      "success"
    } else if (30 < percent_countdown & percent_countdown <= 66) {
      "warning"
    } else {
      "danger"
    }
    progressBar(
      id = "countdown", 
      value = percent_countdown, 
      status = statusClass,
      striped = TRUE,
      size = "xs",
      title = paste0("End in ", countdown, " min")
    )
  })
  
  # When the counter is equal to 0, each time the session is opened, 
  # show the how to start sweetAlert
  # I set up a delay of 5 seconds so that the alert is not displayed before
  # the page is fully loaded (in case we use preloaders in the dashboardPagePlus
  # the preloader lasts around 3s...)
  observe({
    if (counter$alert == 0) {
      shinyjs::delay(
        5000,
        confirmSweetAlert(
          session, 
          inputId = "register_user",
          title = "How to start?",
          text = tagList(
            "Welcome to the virtual CaPO4 patient simulator.
            A random patient was selected for you. The goal of 
            the game is to find the corresponding disease and treat
            the patient correctly. Before starting enter your 
            name and the date.",
            hr(),
            column(
              align = "center",
              width = 12,
              textInput("user_name", "Your name:")
            )
          ),
          btn_labels = c(NULL, "Confirm"),
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
  
  # shift stop when countdown is 0
  observe({
    if (countdown() == 0) 
      events$stop <- TRUE
  })
  
  # When the timer is 0 the game is over if the user has no diagnosis
  # and treatment
  observe({
    if (is.null(input$close_app)) {
      if (events$stop) {
        confirmSweetAlert(
          inputId = "close_app",
          danger_mode = TRUE,
          session, 
          title = "This is the end!",
          text = HTML(
            paste(
              "It seems that the game is finished. 
              You can restart or close the game."
            )
          ),
          btn_labels = c("Restart", "Stop"),
          type = "error",
          html = TRUE
        )
      }
    }
  })
  
  # Handle what happens when the user close or restart the app
  observeEvent(input$close_app, {
      if (input$close_app) {
        sendSweetAlert(
          session, 
          title = "Stop in 5 seconds...", 
          type = "error"
        )
        shinyjs::delay(5000, {
          js$closeWindow()
          stopApp()
        })
      } else {
        session$reload()
      }
  })
  
  
  # init the directory where user datas will be saved
  observeEvent(input$register_user, {
    user_folder <- paste0(getwd(), "/www/users_datas/")
    if (input$register_user) {
      # create the new folder
      dir.create(paste0(user_folder, input$user_name, "-", start_time))
    }
  })
  
  # save user events whenever export button is pressed
  observeEvent(input$export, {
    user_folder <- paste0(
      getwd(), "/www/users_datas/", 
      input$user_name, "-", start_time
    )
    
    # save only if the event table contains elements
    if (nrow(events$history) > 0) {
      saveRDS(
        object = events$history, 
        file = paste0(user_folder, "/user_timeline.rds")
      )
      # otherwise explain the user what to do
    } else {
      sendSweetAlert(
        session,
        title = "Your timeline is currently empty, 
        please trigger events before saving!",
        type = "error"
      )
    }
  })
  
  # save user comments whenever export button is pressed
  observeEvent(input$export, {
    user_folder <- paste0(
      getwd(), "/www/users_datas/", 
      input$user_name, "-", start_time
    )
    
    # save user comments in a separate file
    if (nrow(comments$history) > 0) {
      saveRDS(
        object = comments$history, 
        file = paste0(user_folder, "/user_comments.rds")
      )
      # otherwise explain the user what to do
    } else {
      sendSweetAlert(
        session,
        title = "You do not have any comments!",
        type = "error"
      )
    }
  })
  
  # save user plasma analysis history whenever export button is pressed
  observeEvent(input$export, {
    user_folder <- paste0(
      getwd(), "/www/users_datas/", 
      input$user_name, "-", start_time
    )
    
    # save user comments in a separate file
    if (nrow(plasma_analysis$history) > 0) {
      saveRDS(
        object = plasma_analysis$history, 
        file = paste0(user_folder, "/user_plasma_analysis.rds")
      )
      # otherwise explain the user what to do
    } else {
      sendSweetAlert(
        session,
        title = "You do not have any plasma analysis results!",
        type = "error"
      )
    }
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
    inputSweetAlert(
      session,
      inputId = "diagnosis_answer",
      title = "What is the disease of this patient?",
      btn_labels = c("Send"),
      placeholder = "Be careful about word spelling before submitting!",
      type = "warning"
    )
  })
  
  # treat the diagnosis answer
  observeEvent(input$diagnosis_answer, {
    user_answer <- input$diagnosis_answer
    if (user_answer != "") {
      test <- str_detect(answer, regex(paste0("\\b", user_answer, "\\b"), ignore_case = TRUE))
      if (test) {
        events$answered <- TRUE
        sendSweetAlert(
          session,
          title = paste0("Congratulations ", input$user_name, " !"),
          text = "It seems that you discovered this patient disease. 
          However, it would be better to treat him now. Remember you have
          15 minutes to complete this activity.",
          type = "success"
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
      user_folder <- paste0(
        getwd(), "/www/users_datas/", 
        input$user_name, "-", start_time
      )
      saveRDS(
        object = events$answered, 
        file = paste0(user_folder, "/user_answer.rds")
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
       "Successful Diagnosis" 
     else "Unsuccessful diagnosis"
   } else {
     "No diagnosis yet"
   }
   div(
     style = "margin-top: 7.5px; margin-left: 10px;",
     class = "diagnosis-badge",
     dashboardLabel(
       game_text, 
       status = game_status, 
       style = "square"
     )
   )
  })
  
  
  # clean users datas from empty folders when the user close the session
  session$onSessionEnded(function() {
    user_folder <- paste0(getwd(), "/www/users_datas/")
    dir_list <- list.dirs(user_folder)
    if (length(dir_list) > 1) {
      lapply(2:length(dir_list), FUN = function(i) {
        temp_dir <- dir_list[[i]]
        temp_file_list <- list.files(temp_dir)
        if (length(temp_file_list) == 0) unlink(x = temp_dir, recursive = TRUE)
      })
    }
  })
  
  #------------------------------------------------------------------------- 
  # sidebar User panel: print name and date
  #  
  #-------------------------------------------------------------------------
  
  output$user_panel <- renderUI({
    # use invalidate later to simulate a clock
    invalidateLater(1000)
    sidebarUserPanel(
      input$user_name, 
      subtitle = Sys.time(), 
      image = "https://image.flaticon.com/icons/svg/305/305983.svg"
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
    PTX = FALSE
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
      if (input$treatment_selected != "PTX" &
          input$treatment_selected != "cinacalcet") {
        if (nrow(events$history) == 0) {
          temp_event <- data.frame(
            id = events$counter,
            real_time = Sys.time(),
            event = input$treatment_selected,
            rate = input[[paste(input$treatment_selected)]],
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
            rate = input[[paste(input$treatment_selected)]],
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
    shinyjs::delay(5000, {
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
  #  to make plots or other stuffs
  #
  #-------------------------------------------------------------------------
  
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
          # populated with the last simulated final condition. 
          y = if (is_empty(states$val)) {
            patient_state_0
          } else {
            states$val[[length(states$val)]]
          }, 
          times = times, 
          func = calcium_phosphate_core, 
          parms = parameters
        )
      )
    })
  })
  
  
  # update initial conditions to the last state of the system each time an event
  # has occured. Need to delayed by the time needed for computation before updating
  observe({
    input$run
    shinyjs::delay(5000, {
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
        title = tagList(
          "Overview of your patient",
          tags$button(
            type = "button",
            class = "btn btn-default pull-right",
            `data-dismiss` = "modal",
            icon("close"),
            "Dismiss"
          )
        ),
        fluidRow(
          column(
            width = 12, 
            align = "center",
            tabsetPanel(
              type = "tabs",
              tabPanel("Plot", "plot"),
              tabPanel("Summary", "summary"),
              tabPanel("Table", "table")
            )
          )
        ),
        size = "m",
        footer = NULL
      ) 
    )
  })
  
  out_summary <- eventReactive(input$summary, {
    if (nrow(events$history) >= 2) {
      times <- as.list(events$history[, "real_time"])
      len <- length(times)
      delta_t <- lapply(2:len, FUN = function(i) {
        difftime(
          time1 = times[[i]], 
          time2 = times[[i - 1]], 
          units = c("secs"), 
          tz = Sys.timezone(location = TRUE)
        )
      })
      
      
      parameters <- parameters()
      times <- times()
      as.data.frame(
        ode(
          y = patient_state_0, 
          times = times, 
          func = calcium_phosphate_core, 
          parms = parameters
        )
      )
      
    }
  })
  
  observe({
    print(out_summary())
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
        initRedraw = "
          function() {
            this.moveTo({scale:0.6});
        }"
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
    tagList(
      knobInput(
        "size_organs", 
        "Organs", 
        min = 50, 
        max = 100, 
        value = 70, 
        step = 5,
        displayPrevious = TRUE,
        fgColor = "#A9A9A9", 
        inputColor = "#A9A9A9",
        skin = "tron",
        width = "100px", 
        height = "100px"
      )
    )
  })
  
  output$size_nodes_hormones <- renderUI({
    tagList(
      knobInput(
        "size_hormones", 
        "Hormones", 
        min = 20, 
        max = 60, 
        value = 40, 
        step = 5,
        displayPrevious = TRUE,
        fgColor = "#A9A9A9", 
        inputColor = "#A9A9A9",
        skin = "tron",
        width = "100px", 
        height = "100px"
      )
    )
  })
  
  # control width of arrows
  output$width_arrows_organs <- renderUI({
    tagList(
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
        width = "100px", 
        height = "100px"
      )
    )
  })
  
  output$width_arrows_hormones <- renderUI({
    tagList(
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
        width = "100px", 
        height = "100px"
      )
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
      condition = !is.na(input$tmax),
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
  
  # Custom footer
  # output$dynamicFooter <- renderFooter({ 
  #   generate_dynamicFooter() 
  # })
})