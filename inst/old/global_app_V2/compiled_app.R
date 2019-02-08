library(shiny)
library(plotly)
library(deSolve)
require(visNetwork)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjqui)
library(bsplus)
library(sweetalertR)
library(rintrojs)
library(purrr)
library(stringr)
library(shinyMenus)

# initial conditions
state <- c("PTH_g" = 1288.19, "PTH_p" = 0.0687, 
           "D3_p" = 564.2664, "FGF_p" = 16.78112, 
           "Ca_p" = 1.2061,"Ca_f" = 1.8363, "Ca_b" = 250, 
           "PO4_p" = 1.4784, "PO4_f" = 0.7922, "PO4_b" = 90, 
           "PO4_c" = 2.7719,"CaHPO4_p" = 0.1059, "CaH2PO4_p" = 0.0038, 
           "CPP_p" = 0.0109, "CaHPO4_f" = 0.0864, "CaH2PO4_f" = 0.0031, 
           "CaProt_p" = 1.4518, "NaPO4_p" = 0.9135, "Ca_tot" = 2.4914, 
           "PO4_tot" = 2.8354, "EGTA_p" = 0, "CaEGTA_p" = 0)

# some parameters
source("cap_fixed_parameters.R")

# times
times <- seq(0, 1000, by = 1) 

# compile the C code containing equations
system("R CMD SHLIB compiled_core.c")
dyn.load(paste("compiled_core", .Platform$dynlib.ext, sep = ""))


# Shiny app
ui <- fluidPage(
  titlePanel("Test with compiled CaPO4"),
  
  
  
  sliderInput("k_prod_PTHg", 
              "PTH synthesis rate constant (μmol/min)",
              min = 0, 
              max = 100, 
              value = 1,
              step = 1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetPTHsynthesis",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  
  sliderInput("beta_exo_PTHg", 
              "Maximal PTH secretion from parathyroid 
                         glands (1/min)", 
              min = 0, 
              max = 2, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetPTHexocytosis",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  
  sliderInput("gamma_exo_PTHg", 
              "Maximal inhibition of PTH 
                         secretion from parathyroid glands 
                         by calcium (1/min)", 
              min = 0, 
              max = 1, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetPTHexocytosisinhib",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  
  sliderInput("D3_inact", 
              "Plasma concentration of 25(OH)D (nM)", 
              min = 0, 
              max = 100, 
              value = 1, 
              step = 1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetD3inact",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("k_deg_D3", 
              "Rate constant for vitamin D3 
                                degradation (1/min)", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetD3deg",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("k_prod_FGF", 
              "Minimal rate of FGF23 synthesis (fM/min)", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetFGFsynth",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  
  sliderInput("I_Ca", 
              "Calcium intake (μmol/min)", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetCaintake",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("I_P", 
              "Phosphate intake (μmol/min)", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetPintake",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("k_p_Ca", 
              "Ca transfer from plasma to fast bone pool (1/min)", 
              min = 0, 
              max = 2, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetkpCa",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("k_f_Ca", 
              "Ca transfer from fast bone pool to plasma (1/min)", 
              min = 0, 
              max = 2, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetkfCa",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("k_p_P", 
              "PO4 transfer from plasma to fast bone pool (1/min)", 
              min = 0, 
              max = 2, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetkpP",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("k_f_P", 
              "PO4 transfer from fast bone pool to plasma (1/min)", 
              min = 0, 
              max = 2, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetkfP",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("Lambda_ac_Ca", 
              "rate constant of Ca flux into bone (1/min)", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetacCa",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("Lambda_res_min", 
              "Minimal resorption rate (μmol/min)", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetresmin",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("delta_res_max", 
              "Maximal resorption rate (μmol/min)", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetresmax",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  
  sliderInput("k_pc", 
              "PO4 transfer from plasma pool to intracellular (1/min)", 
              min = 0, 
              max = 2, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetkpc",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("k_cp", 
              "PO4 transfer from intracellular pool to plasma (1/min)", 
              min = 0, 
              max = 2, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("undo") %>%
        actionBttn(inputId = "resetkcp",
                   label = "", 
                   color = "danger", 
                   size = "xs")),
  
  sliderInput("k_fet", 
              "$$ k_f^{fetA} $$", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "Fetuin-A binding to CaHPO4 and 
                                           CaH2PO4+ (1/min)")),
  
  sliderInput("k_c_CPP", 
              "$$ k_c^{CPP} $$", 
              min = 0, 
              max = 100, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "CPP degradation rate constant (1/min)")),
  
  sliderInput("Na", 
              "$$ [Na^+]_p $$", 
              min = 0, 
              max = 10, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "Sodium plasma concentration (mM)")),
  
  sliderInput("Prot_tot_p", 
              "$$ [Prot^{tot}]_p $$", 
              min = 0.25, 
              max = 2, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "Concentration of proteins in plasma (mM)")),
  
  sliderInput("Vp", 
              "$$ V_p $$", 
              min = 0.7, 
              max = 1.3, 
              value = 1, 
              step = 0.1) %>% # + - 30% variation
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "Plasma volume (mL)")),
  
  sliderInput("GFR", 
              "$$ GFR $$", 
              min = 0, 
              max = 1.5, 
              value = 1, 
              step = 0.1) %>%
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "glomerular filtration rate (mL/min)")),
  
  
  
  
  fluidRow(
    verbatimTextOutput("table"),
    plotOutput("plot")
  )
)


server <- function(input, output, session) {
  
  # Parameters: multiply the real parameter value by the user input. 
  # By default, user input = 1 so that parameters are well defined
  reactive_parms <- reactive({ 
    
    c("k_prod_PTHg" = 4.192*input$k_prod_PTHg, 
      "beta_exo_PTHg" = 5.9e-002*input$beta_exo_PTHg,
      "gamma_exo_PTHg" = 5.8e-002*input$gamma_exo_PTHg, 
      "D3_inact" = 2.5e-005*input$D3_inact, 
      "k_deg_D3" = 1e-003*input$k_deg_D3,
      "k_prod_FGF" = 6.902e-011*input$k_prod_FGF, 
      "I_Ca" = 2.2e-003*input$I_Ca, 
      "Lambda_ac_Ca" = 5.5e-004*input$Lambda_ac_Ca,
      "Lambda_ac_P" = 2.75e-004*input$Lambda_ac_Ca, 
      "Lambda_res_min" = 1e-004*input$Lambda_res_min, 
      "delta_res_max" = 6e-004*input$delta_res_max,
      "k_p_Ca" = 0.44*input$k_p_Ca, 
      "k_f_Ca" = 2.34e-003*input$k_f_Ca, 
      "I_P" = 1.55e-003*input$I_P, 
      "k_pc" = 0.1875*input$k_pc,
      "k_cp" = 1e-003*input$k_cp, 
      "k_p_P" = 13.5*input$k_p_P, 
      "k_f_P" = 0.25165*input$k_f_P, 
      "k_fet" = 0.3*input$k_fet,
      "k_c_CPP" = 3*input$k_c_CPP, 
      "Na" = 142*input$Na, 
      "Prot_tot_p" = 0.6*input$Prot_tot_p, 
      "Vp" = 0.01*input$Vp,
      "GFR" = 2e-003*input$GFR) 
    
  })
  
  # make a vector of input$parameters, fixed_parameters and calculated parameters
  parameters <- reactive({ c(reactive_parms(), parameters_fixed) })
  
  
  out <- reactive({
    parameters <- parameters()
    
    temp <- as.data.frame(ode(y = state,
                              times = times,
                              func = "derivs",
                              parms = parameters,
                              dllname = "compiled_core",
                              initfunc = "initmod",
                              nout = 33,
                              outnames = c("U_Ca", "U_PO4", "Abs_int_Ca", 
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
                                           "Reabs_PT_PO4_PTH", "Reabs_PT_PO4_FGF")))
    temp
  })
  
  output$table <- renderPrint({out()[,1:2]})
  
  output$plot <- renderPlot({
    out <- out()
    plot(x = out[,"time"], y = out[,"PTH_g"], type = "l")
  })
  
}

shinyApp(ui = ui, server = server)