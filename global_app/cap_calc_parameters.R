parameters_calc <- function (input){
  pKa <- parameters_fixed["pKa"]
  param_calc <- c("r" = 10^(input$pH - pKa), "a" = 10^(input$pH - pKa)/(1+10^(input$pH- pKa)), "b" = 1/(1+10^(input$pH - pKa)))
}
