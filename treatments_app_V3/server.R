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
  
  # answer
  if (patient_disease == "php1") answer <- "primary hyperparathyroidism" 
  
  # initial conditions
  state <- c("PTH_g" = 1288.19, "PTH_p" = 0.0687, 
             "D3_p" = 564.2664, "FGF_p" = 16.78112, 
             "Ca_p" = 1.2061,"Ca_f" = 1.8363, "Ca_b" = 250, 
             "PO4_p" = 1.4784, "PO4_f" = 0.7922, "PO4_b" = 90, 
             "PO4_c" = 2.7719,"CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, 
             "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, 
             "CaProt_p" = 1.4518, "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, 
             "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0)
  
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
  Ca_inject <- NULL
  Ca_food <- NULL
  P_inject <- NULL
  P_food <- NULL
  D3_inject <- NULL
  
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
    name = "start_case")
  
  # storing parameters event from the data frame to a reactive list
  parameters_event <- reactive({generate_event_parms(event$table)})
  
  # Create parameters sets for all diseases and treatments
  parameters_disease <- reactive({
    c("k_prod_PTHg" = ifelse(
      patient_disease == "php1", 300*4.192, 
      ifelse(patient_disease == "hypopara", 0, 4.192)
      ), 
      "D3_inact" = ifelse(patient_disease == "hypoD3", 0, 2.5e-005),
      # once PTX is done, it is forever!
      "PTX_coeff" = ifelse(
        is.element("parathyroid surgery", event$table$event), 0, 
        ifelse(is.element("parathyroid surgery", input$treatment_selected), 0, 1)
      ),
      "k_inject_Ca" = ifelse(
        is.element("Ca iv injection", event$table$event), 
        input$Ca_inject, 0
      ), 
      "Ca_food" = ifelse(
        is.element("Ca supplementation", event$table$event), 
        input$Ca_food, 2.2e-003
      ),
      "k_inject_D3" = ifelse(
        is.element("vitamin D3 iv injection", event$table$event), 
        input$D3_inject, 0
      ),
      "k_inject_P" = ifelse(
        is.element("PO4 iv injection", event$table$event), 
        input$P_inject, 0
      ),
      "P_food" = ifelse(
        is.element("PO4 supplementation", event$table$event), 
        input$P_food, 1.55e-003
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
    
    comments <- comments$table
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
    len <- nrow(event$table)
    name <- event$table$event
    start_time <- event$table$real_time
    rate <- event$table$rate
    plasma_values <- plasma_analysis$history

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
                  withMathJax(
                    tagList(
                      br(),
                      paste0("[Ca^{2+}_p] = ", round(plasma_values[i, "Ca_p"]), " mM"),
                      br(),
                      paste0("[P_i] = ", round(plasma_values[i, "PO4_p"]), " mM"),
                      br(),
                      paste0("[PTH_p] = ", round(plasma_values[i, "PTH_p"]*100), " pM"),
                      br(),
                      paste0("[D3_p] = ", round(plasma_values[i, "D3_p"]), " pM"),
                      br(),
                      paste0("[FGF23_p] = ", round(plasma_values[i, "FGF_p"]), " pM"),
                      br()
                    )
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
      title = paste0("This game will end in ", countdown, " min")
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
      event$stop <- TRUE
  })
  
  # When the timer is 0 the game is over if the user has no diagnosis
  # and treatment
  observe({
    if (is.null(input$close_app)) {
      if (event$stop) {
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
  observeEvent(input$register_user,{
    user_folder <- paste0(getwd(), "/www/users_datas/")
    if (input$register_user) {
      # create the new folder
      dir.create(paste0(user_folder, input$user_name, "-", start_time))
    }
  })
  
  # save user events whenever export button is pressed
  observeEvent(input$export,{
    user_folder <- paste0(
      getwd(), "/www/users_datas/", 
      input$user_name, "-", start_time
    )
    
    # save only if the event table contains elements
    if (nrow(event$table) > 0) {
      saveRDS(
        object = event$table, 
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
  observeEvent(input$export,{
    user_folder <- paste0(
      getwd(), "/www/users_datas/", 
      input$user_name, "-", start_time
    )
    
    # save user comments in a separate file
    if (nrow(comments$table) > 0) {
      saveRDS(
        object = comments$table, 
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
  observeEvent(input$export,{
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
    test <- str_detect(answer, regex(user_answer, ignore_case = TRUE))
    if (test) {
      event$answered <- TRUE
      sendSweetAlert(
        session,
        title = paste0("Congratulations ", input$user_name, " !"),
        text = "It seems that you discovered this patient disease. 
        However, it would be better to treat him now. Remember you have
        15 minutes to complete this activity.",
        type = "success"
      ) 
    } else {
      event$answered <- FALSE
      sendSweetAlert(
        session,
        title = "Wasted!",
        text = paste0(input$user_name, ", it seems that your answer is wrong!"),
        type = "error"
      )
    }
  })
  
  # prevent the user from resubmitting an answer if he correctly guessed
  # the patient disease
  observe({
    if (!is.null(event$answered)) {
      if (event$answered) {
        shinyjs::disable("diagnosis") 
      } 
    }
  })
  
  # a label to indicate the user whether the diagnosis is ok or not
  # in the header
  output$user_game_status <- renderUI({
   game_status <- if (!is.null(event$answered)) {
     if (event$answered) "success" else "danger"
   } else {
     "warning"
   }
   game_text <- if (!is.null(event$answered)) {
     if (event$answered) 
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
    table = data.frame(
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
      comments$table <- rbind(comments$table, temp_comment) 
    }
  })
  
  
  #------------------------------------------------------------------------- 
  #  This part handle events, plasma analysis, triggered by the user
  #  as well as the export function to save the timeline Event
  #  
  #-------------------------------------------------------------------------
  
  # Set events parameters in reactiveValues so as to modify them later
  event <<- reactiveValues(
    table = data.frame(
      id = NULL,
      event = NULL,
      rate = NULL,
      start_time = NULL,
      stop_time = NULL,
      status = NULL,
      stringsAsFactors = FALSE
    ),
    counter = 1,
    stop = FALSE,
    answered = NULL
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
  
  # generate the slider corresponding to a given treatment
  output$sliderInject <- renderUI({
    req(input$treatment_selected)
    generate_slider_events(input)
  })
  
  # plasma analysis events
  observeEvent(input$current_node_id, {
    node_id <- input$current_node_id
    if (node_id == 2) {
      temp_event <- data.frame(
        id = event$counter,
        real_time = Sys.time(),
        event = "plasma analysis",
        rate = "undefined",
        start_time = "undefined",
        stop_time = "undefined",
        status = "active",
        stringsAsFactors = FALSE
      )
      event$table <- rbind(event$table, temp_event)
      event$counter <- event$counter + 1
    }
  })
  
  # Add treatments to the event list
  observeEvent(input$add_treatment,{
    # the same treatment can be added
    # multiple times. However, parathyroidectomy
    # cannot be performed more than once
      if (input$treatment_selected != "PTX" &
          input$treatment_selected != "cinacalcet") {
        if (nrow(event$table) == 0) {
          temp_event <- data.frame(
            id = event$counter,
            real_time = Sys.time(),
            event = input$treatment_selected,
            rate = input[[paste(input$treatment_selected)]],
            start_time = 0,
            stop_time = input$t_stop,
            status = "active",
            stringsAsFactors = FALSE
          )
        } else {
          #old_time <- event$table$real_time[nrow(event$table)]
          temp_event <- data.frame(
            id = event$counter,
            real_time = Sys.time() + input$t_stop,
            event = input$treatment_selected,
            rate = input[[paste(input$treatment_selected)]],
            start_time = 0,
            stop_time = input$t_stop,
            status = "active",
            stringsAsFactors = FALSE
          )
        }
        event$table <- rbind(event$table, temp_event)
        event$counter <- event$counter + 1
      } else {
        if (is.na(match("PTX", event$table$event))) {
          temp_event <- data.frame(
            id = event$counter,
            real_time = Sys.time(),
            event = input$treatment_selected,
            rate = "undefined", 
            start_time = "undefined",
            stop_time = "undefined",
            status = "active",
            stringsAsFactors = FALSE
          )
          event$table <- rbind(event$table, temp_event)
          event$counter <- event$counter + 1
        } else {
          showNotification("Cannot perform parathyroidectomy more than once!",
                           type = "error", closeButton = TRUE)
        }
      }
  })
  
  observe({
    #print(event$table)
    #print(plasma_analysis$history)
    #print(input$t_stop)
    #print(patient_datas)
    #print(patient_state_0)
    #print(input$treatment_selected)
  })
  
  # Render the event table
  output$event_table <- renderTable({event$table})
  
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
      patient_state_0
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
  # has occured. Do not update if parathyroid surgery is performed twice
  observe({
    shinyjs::delay(
      5000,
      isolate({
        req(input$treatment_selected)
        if (is.na(match("parathyroid surgery", event$table$event))) {
          out <- out()
          temp_state <- c(
            "PTH_g" = out[nrow(out),"PTH_g"], "PTH_p" = out[nrow(out),"PTH_p"],
            "D3_p" = out[nrow(out),"D3_p"], "FGF_p" = out[nrow(out),"FGF_p"],
            "Ca_p" = out[nrow(out),"Ca_p"], "Ca_f" = out[nrow(out),"Ca_f"],
            "Ca_b" = out[nrow(out),"Ca_b"], "PO4_p" = out[nrow(out),"PO4_p"],
            "PO4_f" = out[nrow(out),"PO4_f"], "PO4_b" = out[nrow(out),"PO4_b"],
            "PO4_c" = out[nrow(out),"PO4_c"], "CaHPO4_p" = out[nrow(out),"CaHPO4_p"],
            "CaH2PO4_p" = out[nrow(out),"CaH2PO4_p"], "CPP_p" = out[nrow(out),"CPP_p"],
            "CaHPO4_f" = out[nrow(out),"CaHPO4_f"], "CaH2PO4_f" = out[nrow(out),"CaH2PO4_f"],
            "CaProt_p" = out[nrow(out),"CaProt_p"],"NaPO4_p" = out[nrow(out),"NaPO4_p"],
            "Ca_tot" = out[nrow(out),"Ca_tot"], "PO4_tot" = out[nrow(out),"PO4_tot"],
            "EGTA_p" = out[nrow(out),"EGTA_p"], "CaEGTA_p" = out[nrow(out),"CaEGTA_p"]
          )
          states$counter <- states$counter + 1
          states$val[[states$counter]] <- temp_state
          states$name <- input$treatment_selected
        } else {
          NULL
        }
      })
    )
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
    
    generate_network(input, nodes = nodes_Ca, edges = edges_Ca, usephysics = TRUE) %>%
      # simple click event to allow graph ploting
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}") %>% 
      # unselect node event
      visEvents(deselectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', 'null');
                ;}") %>%
      # add the doubleclick function to handle zoom views
      visEvents(doubleClick = "function(nodes) {
                Shiny.onInputChange('current_node_bis_id', nodes.nodes);
                }") %>%  
      visEvents(selectEdge = "function(edges) {
                Shiny.onInputChange('current_edge_id', edges.edges);
                ;}") %>%
      # very important: change the whole graph position after drawing
      visEvents(type = "on", stabilized = "function() {
                this.moveTo({ position: {x:0, y:-13.43},
                offset: {x: 0, y:0} })}") %>%
      # very important: allow to detect the web browser used by client
      # use before drawing the network. Works with find_navigator.js
      visEvents(type = "on", initRedraw = "function() {
                 this.moveTo({scale:0.6})}") # to set the initial zoom (1 by default)
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
  #  Notification events to explain the user how to play with the app
  #
  #-------------------------------------------------------------------------
  
  # help animation with introjs
  # options are provided to control the size of the help
  # Do not forget to wrap the event content in I('my_function')
  # otherwise it will fail
  observeEvent(input$help,{
    introjs(
      session, 
      options = list(
        "nextLabel" = "Next step!",
        "prevLabel" = "Did you forget something?",
        "tooltipClass" = "newClass",
        "showProgress" = TRUE,
        "showBullets" = FALSE
      ),
      events = list(# reset the session to hide sliders and back/next buttons
        "oncomplete" = I('history.go(0)')
      )
    )
    #   "onbeforechange" = I('
    #                                  if (targetElement.getAttribute("data-step")==="2") {
    #                        $(".newClass").css("max-width", "800px").css("min-width","800px");  
    # } else {
    #                        $(".newClass").css("max-width", "500px").css("min-width","500px");
    # }'),
  })
  
  #------------------------------------------------------------------------- 
  #  
  #  Toggle each sidebar when a user press the help button
  #
  #-------------------------------------------------------------------------
  
  observe({
    shinyjs::toggleClass(
      id = "controlbar", 
      class = "control-sidebar-open",
      condition = input$help
    )
  })
  
  # observeEvent(input$help,{
  #   shinyjs::removeClass(selector = "body", 
  #                        class = "sidebar-collapse")
  # })
  
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
 
  
  # # automatically uncheck the treatment checkboxGroup 
  # # when add_treatment is activated
  # observe({
  #   req(input$add_treatment)
  #   if (input$add_treatment > 0) {
  #     shinyjs::delay(
  #       500,
  #       updatePrettyCheckboxGroup(
  #         session,
  #         inputId = "treatment_selected",
  #         choices = c("parathyroid surgery",
  #                     "vitamin D3 iv injection",
  #                     "Ca supplementation",
  #                     "Ca iv injection",
  #                     "PO4 supplementation",
  #                     "PO4 iv injection",
  #                     "cinacalcet"),
  #         selected = NULL,
  #         prettyOptions = list(thick = TRUE, animation = "pulse", status = "info")
  #       )
  #     )
  #   }
  # })
  

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
  
  
  # reset all the values of box inputs as well as graphs
  observeEvent(input$resetAll,{
    reset("treatment_selected")
    reset("tmax")
    reset("t_now")
    
    edges_Ca <- edges_Ca()
    
    visNetworkProxy("network_Ca") %>%
      visUpdateEdges(edges = edges_Ca)
  })
  
})