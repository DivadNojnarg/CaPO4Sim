# *------------------------------------------------------------------
# | PROGRAM NAME: all_plots.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains all plots of php1, hypopara and
# |           hypoD3. Calls generate_slidersteady(input) to generate
# |           the slider corresponding to the running simulation
# |*------------------------------------------------------------------
# | DATA USED:  php1.csv, hypopara.csv, hypoD3.csv, iv_Ca.csv,
# |             iv_PO4.csv, gav_PO4.csv
# |
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  PHP1
# |  PART 2:  HYPOPARA
# |  PART 3:  HYPOD3
# |  PART 4: Ca inject
# |  PART 5: PO4 inject
# |  PART 6: PO4 gavage
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)          
# |
# |
# *------------------------------------------------------------------


# define color palettes
colfuncCa <- colorRampPalette(c("darkblue", "lightblue", "green"))(20)
colfuncPO4 <- colorRampPalette(c("darkred", "pink", "yellow"))(20)
# plot(rep(1,20),col=colfuncPO4,pch=19,cex=3)

# .... #
# php1 #
# .... #

# load path to data
path_to_php1 <- paste0(getwd(), "/www/datas/php1.csv") 
#path_to_php1 <- "/srv/shiny-server/capApp/case_studies_app/www/php1.csv" 
php1_table <- read.csv(path_to_php1)
# create the sequence of PTH production rate
php1_vec <- 4.192 * seq(1, 300,by = 10) 
names(php1_vec) <- paste("k_prod_PTHg =", php1_vec)

make_plot_php1 <- function(input) {
  
  slidersteady_value <- generate_slidersteady(input)
  
  # define x and y ranges
  xvar <- list(
    title = "PTH synthesis fold increase", 
    range = c(min(php1_vec / php1_vec[1]), 
              max(php1_vec) / php1_vec[1])
  )
  yvar1 <- list(title = "Normalized concentrations", range = c(0, 2))
  yvar2 <- list(title = "Normalized concentrations", range = c(0, 7))
  yvar3 <- list(title = "Normalized Ca fluxes", range = c(0, 3.5))
  yvar4 <- list(title = "Normalized Pi fluxes", range = c(0, 2))
  
  # plot Ca and PO4 variables
  plot_CaP_php1 <- plot_ly(
    php1_table, x = php1_vec / php1_vec[1], 
    y = php1_table[,"Ca_p"] / php1_table[1,"Ca_p"],
    type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
    line = list(color = 'rgb(27, 27, 244)', width = 2), 
    showlegend = FALSE
    ) %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"PO4_p"] / php1_table[1,"PO4_p"], 
      line = list(color = 'rgb(244, 27, 27)', width = 2), 
      name = "<b>[Pi]p</b>",
      showlegend = FALSE
      ) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 1.8), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    add_annotations(
      x = 400, y = 2.6, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", 
      showarrow = TRUE) %>%
    add_annotations(
      x = 400, y = 0.3, xref = "x", yref = "y",text = "<b>[Pi]p</b>", 
      showarrow = TRUE) %>%
    layout(xaxis = NULL, yaxis = yvar1)
  
  # plot PTH, D3 and FGF23 concentrations
  plot_hormones_php1 <- plot_ly(
    php1_table, x = php1_vec / php1_vec[1], 
    y = php1_table[,"PTH_p"] / php1_table[1,"PTH_p"],
    type = "scatter", mode = "lines", name = "<b>[PTH]p</b>",
    line = list(color = 'black', width = 2, dash = "dash"), 
    showlegend = FALSE
    ) %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"D3_p"] / php1_table[1,"D3_p"], 
      name = "<b>[D3]p</b>",
      line = list(color = 'black', width = 2, dash = "dot"), 
      showlegend = FALSE
      ) %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"FGF_p"] / php1_table[1,"FGF_p"], 
      name = "<b>[FGF23]p</b>",
      line = list(color = 'black', width = 2, dash = "solid"), 
      showlegend = FALSE
      ) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 6), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    add_annotations(
      x = 200, y = 5.2, xref = "x2", yref = "y2", 
      text = "<b>[PTH]p</b>", showarrow = TRUE, ax = 20, ay = 25
      ) %>%
    add_annotations(
      x = 400, y = 10, xref = "x2", yref = "y2", text = "<b>[D3]p</b>", 
      showarrow = TRUE) %>%
    add_annotations(
      x = 600, y = 2, xref = "x2", yref = "y2", text = "<b>[FGF23]p</b>", 
      showarrow = TRUE) %>%
    layout(xaxis = NULL, yaxis = yvar2)
  
  # plot Ca fluxes: resorption, intestinal absorption, urinary excretion 
  # and bone storage
  plot_Ca_fluxes_php1 <- plot_ly(
    php1_table, x = php1_vec / php1_vec[1], 
    y = php1_table[,"U_Ca"] / php1_table[1,"U_Ca"],
    type = "scatter", mode = "lines", 
    line = list(color = colfuncCa[1], width = 2, dash = "solid"), 
    showlegend = FALSE, name = "Urinary Excretion"
    ) %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"Abs_int_Ca"] / php1_table[1,"Abs_int_Ca"], 
      line = list(color = colfuncCa[8], width = 2, dash = "solid"), 
      showlegend = FALSE, name = "Intestinal absorption") %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"Res_Ca"] / php1_table[1,"Res_Ca"], 
      line = list(color = colfuncCa[12], width = 2, dash = "solid"), 
      showlegend = FALSE, name = "Bone resorption") %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"Ac_Ca"] / php1_table[1,"Ac_Ca"], 
      line = list(color = colfuncCa[17], width = 2, dash = "solid"), 
      showlegend = FALSE, name = "FLux into bone") %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6),
      showlegend = FALSE, name = "PHP1 severity") %>%
    layout(xaxis = xvar, yaxis = yvar3)
  
  # plot PO4 fluxes: resorption, intestinal absorption, urinary excretion 
  # and bone storage
  plot_PO4_fluxes_php1 <- plot_ly(
    php1_table, x = php1_vec / php1_vec[1], 
    y = php1_table[,"U_PO4"] / php1_table[1,"U_PO4"],
    type = "scatter", mode = "lines", 
    line = list(color = colfuncPO4[1], width = 2, dash = "solid"), 
    name = "Urinary Excretion", showlegend = FALSE
    ) %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"Abs_int_PO4"] / php1_table[1,"Abs_int_PO4"], 
      line = list(color = colfuncPO4[6], width = 2, dash = "solid"), 
      name = "Intestinal absorption", showlegend = FALSE) %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"Res_PO4"] / php1_table[1,"Res_PO4"], 
      line = list(color = colfuncPO4[10], width = 2, dash = "solid"), 
      name = "Bone resorption", showlegend = FALSE) %>%
    add_lines(
      x = php1_vec / php1_vec[1], 
      y = php1_table[,"Ac_PO4"] / php1_table[1,"Ac_PO4"], 
      line = list(color = colfuncPO4[13], width = 2, dash = "solid"), 
      name = "FLux into bone", showlegend = FALSE) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 2.5), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6),
      name = "PHP1 severity", showlegend = FALSE) %>%
    layout(xaxis = xvar, yaxis = yvar4)
  # to display a legend see below
  #layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'h', x = 100, y = -0.2))
  
  # gather all subplots
  plot_php1 <- subplot(
    plot_CaP_php1, plot_hormones_php1, plot_Ca_fluxes_php1, 
    plot_PO4_fluxes_php1, 
    titleX = TRUE, titleY = TRUE, nrows = 2, 
    margin = c(0.07, 0.07, 0.07, 0.07), 
    heights = c(0.5, 0.5)
  ) %>%
    config(displayModeBar = FALSE)
  
  plot_php1$elementId <- NULL
  
  plot_php1
}

# .......#
# hypoD3 #
# .......#

# load path to data
path_to_hypoD3 <- paste0(getwd(), "/www/datas/hypoD3.csv")
#path_to_hypoD3 <- "/srv/shiny-server/capApp/case_studies_app/www/hypoD3.csv"
hypoD3_table <- read.csv(path_to_hypoD3)
# create the sequence of D3 inact and reverse the vector
hypoD3_vec <- rev(2.5e-005 * seq(0, 1, by = 0.01)) 
names(hypoD3_vec) <- paste("D3_inact =", hypoD3_vec)

make_plot_hypoD3 <- function(input) {
  
  slidersteady_value <- generate_slidersteady(input)
  
  # define x and y ranges
  xvar <- list(
    title = "25(OH)D stock fold decrease", 
    range = c(max(hypoD3_vec / hypoD3_vec[1]), 
              min(hypoD3_vec) / hypoD3_vec[1]),
    autorange = FALSE, autorange = "reversed"
  )
  xvar_bis <- list(
    title = "",
    range = c(max(hypoD3_vec / hypoD3_vec[1]),
              min(hypoD3_vec) / hypoD3_vec[1]),
    autorange = FALSE, autorange = "reversed"
  )
  yvar1 <- list(title = "Normalized concentrations", range = c(0, 1.1))
  yvar2 <- list(title = "Normalized concentrations", range = c(0, 4))
  yvar3 <- list(title = "Normalized Ca fluxes", range = c(0, 1.2))
  yvar4 <- list(title = "Normalized Pi fluxes", range = c(0, 1.1))
  
  # plot Ca and PO4 variables
  plot_CaP_hypoD3 <- plot_ly(
    hypoD3_table, x = hypoD3_vec / hypoD3_vec[1], 
    y = hypoD3_table[,"Ca_p"] / hypoD3_table[1,"Ca_p"],
    type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
    line = list(color = 'rgb(27, 27, 244)', width = 2), 
    showlegend = FALSE
  ) %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"PO4_p"] / hypoD3_table[1,"PO4_p"], 
      line = list(color = 'rgb(244, 27, 27)', width = 2), 
      name = "<b>[Pi]p</b>",
      showlegend = FALSE
      ) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    add_annotations(
      x = 0.7, y = 0.95, xref = "x", 
      yref = "y",text = "<b>[Ca2+]p</b>", 
      showarrow = TRUE, ax = -20, ay = 40
      ) %>%
    add_annotations(
      x = 0.4, y = 1.1, xref = "x", 
      yref = "y",text = "<b>[Pi]p</b>", 
      showarrow = TRUE, ax = -20, ay = -20
      ) %>%
    layout(xaxis = xvar_bis, yaxis = yvar1)
  
  # plot PTH, D3 and FGF23 concentrations
  plot_hormones_hypoD3 <- plot_ly(
    hypoD3_table, x = hypoD3_vec / hypoD3_vec[1], 
    y = hypoD3_table[,"PTH_p"] / hypoD3_table[1,"PTH_p"],
    type = "scatter", mode = "lines", name = "<b>[PTH]p</b>",
    line = list(color = 'black', width = 2, dash = "dash"), 
    showlegend = FALSE
  ) %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"D3_p"] / hypoD3_table[1,"D3_p"], 
      name = "<b>[D3]p</b>",
      line = list(color = 'black', width = 2, dash = "dot"), 
      showlegend = FALSE
      ) %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"FGF_p"] / hypoD3_table[1,"FGF_p"], 
      name = "<b>[FGF23]p</b>",
      line = list(color = 'black', width = 2, dash = "solid"), 
      showlegend = FALSE
      ) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    add_annotations(
      x = 0, y = 2.3, xref = "x2", yref = "y2", 
      text = "<b>[PTH]p</b>", showarrow = TRUE
      ) %>%
    add_annotations(
      x = 0.9, y = 0.9, xref = "paper", yref = "y2", 
      text = "<b>[D3]p</b>", showarrow = TRUE
      ) %>%
    add_annotations(
      x = 0.9, y = 0.4, xref = "paper", yref = "y2", 
      text = "<b>[FGF23]p</b>", showarrow = TRUE, 
      ax = -20, ay = 30
      ) %>%
    layout(xaxis = xvar_bis, yaxis = yvar2)
  
  # plot Ca fluxes: resorption, intestinal absorption, urinary excretion 
  # and bone storage
  plot_Ca_fluxes_hypoD3 <- plot_ly(
    hypoD3_table, x = hypoD3_vec / hypoD3_vec[1], 
    y = hypoD3_table[,"U_Ca"] / hypoD3_table[1,"U_Ca"],
    type = "scatter", mode = "lines", 
    line = list(color = colfuncCa[1], width = 2, dash = "solid"), 
    name = "Urinary Excretion", showlegend = FALSE
  ) %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"Abs_int_Ca"] / hypoD3_table[1,"Abs_int_Ca"], 
      line = list(color = colfuncCa[8], width = 2, dash = "solid"), 
      showlegend = FALSE,
      name = "Intestinal absorption") %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"Res_Ca"] / hypoD3_table[1,"Res_Ca"], 
      line = list(color = colfuncCa[12], width = 2, dash = "solid"), 
      showlegend = FALSE,
      name = "Bone resorption") %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"Ac_Ca"] / hypoD3_table[1,"Ac_Ca"], 
      line = list(color = colfuncCa[17], width = 2, dash = "solid"), 
      showlegend = FALSE,
      name = "Flux into bone") %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    layout(xaxis = xvar, yaxis = yvar3)
  
  # plot PO4 fluxes: resorption, intestinal absorption, urinary excretion 
  # and bone storage
  plot_PO4_fluxes_hypoD3 <- plot_ly(
    hypoD3_table, x = hypoD3_vec / hypoD3_vec[1], 
    y = hypoD3_table[,"U_PO4"] / hypoD3_table[1,"U_PO4"],
    type = "scatter", mode = "lines", 
    line = list(color = colfuncPO4[1], width = 2, dash = "solid"),
    name = "Urinary Excretion", showLegend = FALSE
  ) %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"Abs_int_PO4"] / hypoD3_table[1,"Abs_int_PO4"], 
      line = list(color = colfuncPO4[6], width = 2, dash = "solid"), 
      name = "Intestinal absorption", showlegend = FALSE) %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"Res_PO4"] / hypoD3_table[1,"Res_PO4"], 
      line = list(color = colfuncPO4[10], width = 2, dash = "solid"), 
      name = "Bone resorption", showlegend = FALSE) %>%
    add_lines(
      x = hypoD3_vec / hypoD3_vec[1], 
      y = hypoD3_table[,"Ac_PO4"] / hypoD3_table[1,"Ac_PO4"], 
      line = list(color = colfuncPO4[13], width = 2, dash = "solid"), 
      name = "Flux into bone", showlegend = FALSE) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6),
      name = "HypoD3 severity", showlegend = FALSE
      ) %>%
    layout(xaxis = xvar, yaxis = yvar4)
    # layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'h', x = 100, y = -0.2))
  
  # gather all subplots
  plot_hypoD3 <- subplot(
    plot_CaP_hypoD3, plot_hormones_hypoD3, plot_Ca_fluxes_hypoD3, 
    plot_PO4_fluxes_hypoD3, titleX = TRUE, titleY = TRUE,
    nrows = 2, margin = 0.07, heights = c(0.5, 0.5)
  ) %>%
    config(displayModeBar = FALSE)
  
  plot_hypoD3$elementId <- NULL
  
  plot_hypoD3
}

# ........ #
# hypopara #
# ........ #

# path to data
path_to_hypopara <- paste0(getwd(),"/www/datas/hypopara.csv")
#path_to_hypopara <- "/srv/shiny-server/capApp/case_studies_app/www/hypopara.csv"
hypopara_table <- read.csv(path_to_hypopara)
hypopara_vec <- rev(4.192 * seq(0, 1, by = 0.01)) # create the sequence of PTH production rate
names(hypopara_vec) <- paste("k_prod_PTHg =",  hypopara_vec)

make_plot_hypopara <- function(input) {
  
  slidersteady_value <- generate_slidersteady(input)
  
  # define x and y ranges
  xvar <- list(
    title = "PTH synthesis fold decrease",
    range = c(max(hypopara_vec / hypopara_vec[1]),
              min(hypopara_vec) / hypopara_vec[1]),
    autorange = FALSE, autorange = "reversed"
  )
  xvar_bis <- list(
    title = "",
    range = c(max(hypopara_vec / hypopara_vec[1]),
              min(hypopara_vec) / hypopara_vec[1]),
    autorange = FALSE, autorange = "reversed"
  )
  yvar1 <- list(title = "Normalized concentrations", range = c(0, 1.4))
  yvar2 <- list(title = "Normalized concentrations", range = c(0, 1))
  yvar3 <- list(title = "Normalized Ca fluxes", range = c(0, 1))
  yvar4 <- list(title = "Normalized Pi fluxes", range = c(0, 1.4))
  
  # plot Ca and PO4 variables
  plot_CaP_hypopara <- plot_ly(
    hypopara_table, x = hypopara_vec / hypopara_vec[1], 
    y = hypopara_table[,"Ca_p"] / hypopara_table[1,"Ca_p"],
    type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
    line = list(color = 'rgb(27, 27, 244)', width = 2), 
    showlegend = FALSE
  ) %>%
    add_lines(
      x = hypopara_vec/hypopara_vec[1], 
      y = hypopara_table[,"PO4_p"] / hypopara_table[1,"PO4_p"], 
      line = list(color = 'rgb(244, 27, 27)', width = 2), 
      name = "<b>[Pi]p</b>",
      showlegend = FALSE
    ) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
    ) %>%
    add_annotations(
      x = 0.5, y = 0.85, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", 
      showarrow = TRUE, ax = -20, ay = 40
    ) %>%
    add_annotations(
      x = 0.1, y = 1.7, xref = "x", yref = "y",text = "<b>[Pi]p</b>", 
      showarrow = TRUE
    ) %>%
    layout(xaxis = xvar_bis, yaxis = yvar1)
  
  # plot PTH, D3 and FGF23 concentrations
  plot_hormones_hypopara <- plot_ly(
    hypopara_table, x = hypopara_vec / hypopara_vec[1], 
    y = hypopara_table[,"PTH_p"] / hypopara_table[1,"PTH_p"],
    type = "scatter", mode = "lines", name = "<b>[PTH]p</b>",
    line = list(color = 'black', width = 2, dash = "dash"), 
    showlegend = FALSE
  ) %>%
    add_lines(
      x = hypopara_vec / hypopara_vec[1], 
      y = hypopara_table[,"D3_p"] / hypopara_table[1,"D3_p"], 
      name = "<b>[D3]p</b>",
      line = list(color = 'black', width = 2, dash = "dot"), 
      showlegend = FALSE
    ) %>%
    add_lines(
      x = hypopara_vec/hypopara_vec[1], 
      y = hypopara_table[,"FGF_p"]/hypopara_table[1,"FGF_p"], 
      name = "<b>[FGF23]p</b>",
      line = list(color = 'black', width = 2, dash = "solid"), 
      showlegend = FALSE
    ) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
    ) %>%
    add_annotations(
      x = 0.9, y = 0.2, xref = "paper", yref = "y2", text = "<b>[PTH]p</b>", 
      showarrow = TRUE, ax = -20, ay = 40
    ) %>%
    add_annotations(
      x = 0.9, y = 0.67, xref = "paper", yref = "y2", text = "<b>[D3]p</b>", 
      showarrow = TRUE, ax = 15, ay = -30
    ) %>%
    add_annotations(
      x = 0.88, y = 0.5, xref = "paper", yref = "y2", text = "<b>[FGF23]p</b>", 
      showarrow = TRUE, ax = -50, ay = 10
    ) %>%
    layout(xaxis = xvar_bis, yaxis = yvar2)
  
  # plot Ca fluxes: resorption, intestinal absorption, urinary excretion 
  # and bone storage
  plot_Ca_fluxes_hypopara <- plot_ly(
    hypopara_table, x = hypopara_vec / hypopara_vec[1], 
    y = hypopara_table[,"U_Ca"] / hypopara_table[1,"U_Ca"],
    type = "scatter", mode = "lines", 
    line = list(color = colfuncCa[1], width = 2, dash = "dash"), 
    name = "Urinary Excretion", showlegend = FALSE
  ) %>%
    add_lines(
      x = hypopara_vec / hypopara_vec[1], 
      y = hypopara_table[,"Abs_int_Ca"] / hypopara_table[1,"Abs_int_Ca"], 
      line = list(color = colfuncCa[8], width = 2, dash = "dot"), 
      name = "Intestinal absorption", showlegend = FALSE
    ) %>%
    add_lines(
      x = hypopara_vec / hypopara_vec[1], 
      y = hypopara_table[,"Res_Ca"] / hypopara_table[1,"Res_Ca"], 
      line = list(color = colfuncCa[12], width = 2, dash = "dashdot"), 
      name = "Bone resorption", showlegend = FALSE
    ) %>%
    add_lines(
      x = hypopara_vec / hypopara_vec[1], 
      y = hypopara_table[,"Ac_Ca"] / hypopara_table[1,"Ac_Ca"], 
      line = list(color = colfuncCa[17], width = 2, dash = "solid"), 
      name = "Flux into bone", showlegend = FALSE
    ) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
    ) %>%
    layout(xaxis = xvar, yaxis = yvar3)
  
  # plot PO4 fluxes: resorption, intestinal absorption, urinary excretion 
  # and bone storage
  plot_PO4_fluxes_hypopara <- plot_ly(
    hypopara_table, x = hypopara_vec / hypopara_vec[1], 
    y = hypopara_table[,"U_PO4"] / hypopara_table[1,"U_PO4"],
    type = "scatter", mode = "lines", 
    line = list(color = colfuncPO4[1], width = 2, dash = "dash"),
    name = "Urinary Excretion", showlegend = FALSE
  ) %>%
    add_lines(
      x = hypopara_vec / hypopara_vec[1], 
      y = hypopara_table[,"Abs_int_PO4"] / hypopara_table[1,"Abs_int_PO4"], 
      line = list(color = colfuncPO4[6], width = 2, dash = "dot"),
      name = "Intestinal absorption", showlegend = FALSE
    ) %>%
    add_lines(
      x = hypopara_vec / hypopara_vec[1], 
      y = hypopara_table[,"Res_PO4"] / hypopara_table[1,"Res_PO4"], 
      line = list(color = colfuncPO4[10], width = 2, dash = "dashdot"),
      name = "Bone resorption", showlegend = FALSE
    ) %>%
    add_lines(
      x = hypopara_vec / hypopara_vec[1], 
      y = hypopara_table[,"Ac_PO4"] / hypopara_table[1,"Ac_PO4"], 
      line = list(color = colfuncPO4[13], width = 2, dash = "solid"),
      name = "Flux into bone", showlegend = FALSE
    ) %>%
    add_lines(
      x = slidersteady_value, y = c(0, 4), 
      line = list(size = 6, color = 'orange', dash = "solid", width = 6),
      name = "Hypopara severity", showlegend = FALSE
    ) %>%
    layout(xaxis = xvar, yaxis = yvar4)
  # layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'h', x = 100, y = -0.2))
  
  # gather all subplots
  plot_hypopara <- subplot(
    plot_CaP_hypopara, plot_hormones_hypopara, 
    plot_Ca_fluxes_hypopara, plot_PO4_fluxes_hypopara, 
    titleX = TRUE, titleY = TRUE,
    nrows = 2, margin = 0.07, heights = c(0.5, 0.5)
  ) %>%
    config(displayModeBar = FALSE)
  
  plot_hypopara$elementId <- NULL
  
  plot_hypopara
  
}


# .... #
# Caiv #
# .... #

# path to data
path_to_Ca_iv <- paste0(getwd(),"/www/datas/iv_Ca.csv")
#path_to_Ca_iv <- "/srv/shiny-server/capApp/case_studies_app/www/iv_Ca.csv"
Ca_iv_table <- read.csv(path_to_Ca_iv)

# # define x and y ranges and events
# injectevents <- data.frame(
#   times = c(0, 60, 65, 70, 80, 90, 100, 110, 120), 
#   Ca_val = 1 / 1.35 * c(1.35, 1.45, 1.30, 1.20, 1.15, 1.10, 1.10, 1.00, 1.05),
#   PTH_val = 1 / 65 * c(65, 10, 190, 260, 300, 260, 240, 290, 310), 
#   err_Ca = 1 / 1.35 * 2 * c(0.02, 0.04, 0.04, 0.06, 0.04, 0.06,0.06, 0.07, 0.04),
#   err_PTH = 1 / 65 * c(20, 0, 70, 100, 70, 70, 50, 70, 110)
# )



make_plot_Ca_inject <- function(input){
  
  # axis ranges
  xvar <- list(title = "time (min)", range = c(0, max(Ca_iv_table[, 1]) + 10))
  yvar1 <- list(title = "Normalized [Ca2+]p", range = c(0, 2))
  yvar2 <- list(title = "Normalized [PTH]p", range = c(0, 10))
  
  # plot Ca concentration
  p1 <- plot_ly(
    Ca_iv_table, x = Ca_iv_table[, 1], 
    y = Ca_iv_table[,"Ca_p"] / Ca_iv_table[1,"Ca_p"], 
    type = "scatter", mode = "lines", 
    line = list(color = 'rgb(27, 27, 244)', width = 2)
  ) %>%
    # add_markers(x = injectevents$times, 
    #             y = injectevents$Ca_val, mode = 'markers', 
    #             symbols = "o", marker = list(size = 10, color = 'black'),
    #             error_y = list(array = injectevents$err_Ca, color = 'black'), 
    #             line = list(color = 'white')) %>%
    add_lines(
      x = Ca_iv_table[, 1], 
      y = Ca_iv_table[,"Ca_p"] / Ca_iv_table[1,"Ca_p"], 
      type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
      line = list(color = 'rgb(27, 27, 244)', width = 2)
    ) %>%
    add_lines(
      x = input$tmaxCainj, y = c(0, 2),
      line = list(size = 6, color = 'orange', 
                  dashed = "dashdot", width = 6)
    ) %>%
    add_annotations(
      x = 70, y = 1.5, xref = "x", yref = "y", 
      text = "<b>Ca injection</b>", showarrow = F
    ) %>%
    add_annotations(
      x = 200, y = 1.5, xref = "x", yref = "y", 
      text = "<b>EGTA injection</b>", showarrow = F
    ) %>% 
    layout(xaxis = xvar, yaxis = yvar1)
  
  # plot PTH concentration
  p2 <- plot_ly(
    data = Ca_iv_table, x = Ca_iv_table[, 1], 
    y = Ca_iv_table[,"PTH_p"] / Ca_iv_table[1,"PTH_p"], 
    type = "scatter", mode = "lines",
    line = list(color = 'black', width = 2)
  ) %>%
    # add_trace(x = injectevents$times, 
    #           y = injectevents$PTH_val, mode = 'markers', symbols = "o", 
    #           marker = list(size = 10, color = 'black'),
    #           error_y = list(array = injectevents$err_PTH, color = 'black'), 
    #           line = list(color = 'white')) %>%
    add_lines(
      x = Ca_iv_table[, 1], 
      y = Ca_iv_table[,"PTH_p"] / Ca_iv_table[1,"PTH_p"], 
      type = "scatter", mode = "lines", name = "<b>[PTH]p</b>",
      line = list(color = 'black', width = 2)
    ) %>%
    add_lines(
      x = input$tmaxCainj, y = c(0, 10), 
      line = list(size = 6, color = 'orange', 
                  dashed = "dashdot", width = 6)
    ) %>%
    layout(xaxis = xvar, yaxis = yvar2)
  
  # gather all subplots
  p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE, nrows = 1, margin = 0.05) %>%
    config(displayModeBar = FALSE)
  
  p$elementId <- NULL
  p
  hide_legend(p)
  
}

# .... #
# PO4iv#
# .... #

# path to data
path_to_PO4_iv <- paste0(getwd(),"/www/datas/iv_PO4.csv")
#path_to_PO4_iv <- "/srv/shiny-server/capApp/case_studies_app/www/iv_PO4.csv"
PO4_iv_table <- read.csv(path_to_PO4_iv)

# # define x and y ranges and events
# injectevents <- data.frame(
#   times = c(0, 10, 25, 40, 55, 70, 130, 190, 250), 
#   PO4_val = c(2.86, 5.84, 5.59, 4.69, 4.24, 3.83, 3.08, 3.27, 3.26),
#   Ca_val = 1 / 2.4 * c(2.4, 1.92, 1.97, 1.93, 1.86, 1.76, 1.89, 2.03, 2.02),
#   PTH_val = c(1,8.72, 7.80, 7, 7.64, 9.34, 9.74, 7.12, 6), 
#   err_PO4 = c(0.3, 0.33, 0.33, 0.26, 0.24, 0.13, 0.15, 0.21, 0.39),
#   err_Ca = c(0.05, 0.04, 0.07, 0.04, 0.03, 0.04, 0.09, 0.04, 0.09),
#   err_PTH = c(0.53, 1.40, 1.34, 0.88, 0.97, 0.93, 1.47, 1.42, 0.87)
# )

make_plot_PO4_inject <- function(input){
  
  # axis ranges
  xvar <- list(title = "time (min)", range = c(0, max(PO4_iv_table[, 1])))
  yvar1 <- list(title = "[Pi]p (mM)", range = c(0, 8))
  yvar2 <- list(title = "Normalized [Ca2+]p", range = c(0, 2))
  yvar3 <- list(title = "Normalized [PTH]p", range = c(0, 20))
  
  # plot PO4 total concentration
  p1 <- plot_ly(
    PO4_iv_table, x = PO4_iv_table[, 1], 
    y = PO4_iv_table[,"PO4_tot"], type = "scatter", mode = "lines", 
    line = list(color = 'rgb(244, 27, 27)', width = 2),
    name = "<b>[Pi]p</b>"
  ) %>%
    # add_markers(x = injectevents$times, 
    #             y = injectevents$PO4_val, mode = 'markers', symbols = "o", 
    #             marker = list(size = 10, color = 'black'),
    #             error_y = list(array = injectevents$err_PO4, color = 'black'), 
    #             line = list(color = 'white')) %>%
    add_lines(
      x = PO4_iv_table[, 1], 
      y = PO4_iv_table[,"PO4_tot"], type = "scatter", mode = "lines", 
      line = list(color = 'rgb(244, 27, 27)', width = 2)
    ) %>%
    add_lines(
      x = input$tmaxPO4inj, y = c(0, 8), 
      line = list(size = 6, color = 'orange', 
                  dashed = "dashdot", width = 6)
    ) %>%
    layout(xaxis = NULL, yaxis = yvar1)
  
  # Plot Ca concnentration
  p2 <- plot_ly(
    PO4_iv_table, x = PO4_iv_table[, 1], 
    y = PO4_iv_table[,"Ca_tot"] / PO4_iv_table[1,"Ca_tot"], 
    type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
    line = list(color = 'rgb(27, 27, 244)', width = 2)
  ) %>%
    # add_markers(x = injectevents$times, 
    #             y = injectevents$Ca_val, mode = 'markers', symbols = "o", 
    #             marker = list(size = 10, color = 'black'),
    #             error_y = list(array = injectevents$err_Ca, color = 'black'), 
    #             line = list(color = 'white')) %>%
    add_lines(
      x = PO4_iv_table[, 1], 
      y = PO4_iv_table[,"Ca_tot"] / PO4_iv_table[1,"Ca_tot"], 
      type = "scatter", mode = "lines", 
      line = list(color = 'rgb(27, 27, 244)', width = 2)
    ) %>%
    add_lines(
      x = input$tmaxPO4inj, y = c(0, 2), 
      line = list(size = 6, color = 'orange', 
                  dashed = "dashdot", width = 6)
    ) %>%
    
    layout(xaxis = xvar, yaxis = yvar2)
  
  # plot PTH concentration
  p3 <- plot_ly(
    data = PO4_iv_table, x = PO4_iv_table[, 1], 
    y = PO4_iv_table[,"PTH_p"] / PO4_iv_table[1,"PTH_p"],
    type = "scatter", mode = "lines", name = "<b>[PTH]p</b>",
    line = list(color = 'black', width = 2)
  ) %>%
    # add_trace(x = injectevents$times, 
    #           y = injectevents$PTH_val, mode = 'markers', symbols = "o", 
    #           marker = list(size = 10, color = 'black'),
    #           error_y = list(array = injectevents$err_PTH, color = '#000000'), 
    #           line = list(color = 'white')) %>%
    add_lines(
      x = PO4_iv_table[, 1], 
      y = PO4_iv_table[,"PTH_p"] / PO4_iv_table[1,"PTH_p"], 
      type = "scatter", mode = "lines", 
      line = list(color = 'black', width = 2)
    ) %>%
    add_lines(
      x = input$tmaxPO4inj, y = c(0, 20), 
      line = list(size = 6, color = 'orange', 
                  dashed = "dashdot", width = 6)
    ) %>%
    layout(xaxis = xvar, yaxis = yvar3)
  
  # gather all subplots
  p <- subplot(p1, p2, p3, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.05) %>%
    config(displayModeBar = FALSE)
  
  p$elementId <- NULL
  p
  hide_legend(p)
  
}


# .... #
# PO4iv#
# .... #

# path to data
path_to_PO4_gav <- paste0(getwd(),"/www/datas/gav_PO4.csv")
#path_to_PO4_gav <- "/srv/shiny-server/capApp/case_studies_app/www/gav_PO4.csv"
PO4_gav_table <- read.csv(path_to_PO4_gav)

# # define x and y ranges and events
# gavevents <- data.frame(
#   times = c(0, 10, 25, 40, 55, 70, 130, 190, 250), 
#   PO4_val = c(2.87, 2.91, 3.30, 3.20, 3.10, 3.23, 2.97, 2.72, 2.89),
#   Ca_val = 1 / 2.08 * c(2.08,1.80,1.99,1.96,1.91,1.83,1.75,1.74,1.79),
#   PTH_val = c(1, 1.60, 2.17, 2.14, 2.12, 1.77, 1.95, 1.69, 1.80), 
#   err_PO4 = c(0.14, 0.1, 0.07, 0.2, 0.1, 0.09, 0.13, 0.1, 0.19),
#   err_Ca = c(0.03, 0.07, 0.03, 0.07, 0.04, 0.06, 0.03, 0.03, 0.04),
#   err_PTH = c(0.16, 0.34, 0.40, 0.20, 0.29, 0.12, 0.19, 0.19, 0.14)
# )

make_plot_PO4_gav <- function(input){
  
  # axis ranges
  xvar <- list(title = "time (min)", range = c(0, max(PO4_gav_table[,1])))
  yvar1 <- list(title = "[Pi]p (mM)", range = c(0, 8))
  yvar2 <- list(title = "Normalized [Ca2+]p", range = c(0, 2))
  yvar3 <- list(title = "Normalized [PTH]p", range = c(0, 20))
  
  # plot total PO4 concentration
  p1 <- plot_ly(
    PO4_gav_table, x = PO4_gav_table[, 1], 
    y = PO4_gav_table[,"PO4_tot"], type = "scatter", mode = "lines", 
    line = list(color = 'rgb(244, 27, 27)', width = 2),
    name = "<b>[Pi]p</b>"
  ) %>%
    #add_markers(x = gavevents$times, 
    #            y = gavevents$PO4_val, mode = 'markers', symbols = "o", 
    #            marker = list(size = 10, color = 'black'),
    #            error_y = list(array = gavevents$err_PO4, color = 'black'), 
    #            line = list(color = 'white')) %>%
    add_lines(
      x = PO4_gav_table[, 1], y = PO4_gav_table[,"PO4_tot"], 
      type = "scatter", mode = "lines", 
      line = list(color = 'rgb(244, 27, 27)', width = 2)
    ) %>%
    add_lines(
      x = input$tmaxPO4gav, y = c(0, 8), 
      line = list(size = 6, color = 'orange', 
                  dashed = "dashdot", width = 6)
    ) %>%
    layout(xaxis = NULL, yaxis = yvar1)
  
  # plot total Ca concentration
  p2 <- plot_ly(
    PO4_gav_table, x = PO4_gav_table[, 1], 
    y = PO4_gav_table[,"Ca_tot"] / PO4_gav_table[1,"Ca_tot"], 
    type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
    line = list(color = 'rgb(27, 27, 244)', width = 2)
  ) %>%
    # add_markers(x = gavevents$times, 
    #             y = gavevents$Ca_val, mode = 'markers', symbols = "o", 
    #             marker = list(size = 10, color = 'black'),
    #             error_y = list(array = gavevents$err_Ca, color = 'black'), 
    #             line = list(color = 'white')) %>%
    add_lines(
      x = PO4_gav_table[, 1], 
      y = PO4_gav_table[,"Ca_tot"] / PO4_gav_table[1,"Ca_tot"], 
      type = "scatter", mode = "lines", 
      line = list(color = 'rgb(27, 27, 244)', width = 2)
    ) %>%
    add_lines(
      x = input$tmaxPO4gav, y = c(0, 2), 
      line = list(size = 6, color = 'orange', 
                  dashed = "dashdot", width = 6)
    ) %>%
    layout(xaxis = xvar, yaxis = yvar2)
  
  # plot PTH concentration
  p3 <- plot_ly(
    data = PO4_gav_table, x = PO4_gav_table[, 1], 
    y = PO4_gav_table[,"PTH_p"] / PO4_gav_table[1,"PTH_p"], type = "scatter", 
    mode = "lines", line = list(color = 'black', width = 2), name = "<b>[PTH]p</b>"
  ) %>%
    # add_trace(x = gavevents$times, 
    #           y = gavevents$PTH_val, mode = 'markers', symbols = "o", 
    #           marker = list(size = 10, color = 'black'),
    #           error_y = list(array = gavevents$err_PTH, color = 'black'), 
    #           line = list(color = 'white')) %>%
    add_lines(
      x = PO4_gav_table[, 1], 
      y = PO4_gav_table[,"PTH_p"] / PO4_gav_table[1,"PTH_p"], 
      type = "scatter", mode = "lines", 
      line = list(color = 'black', width = 2)
    ) %>%
    add_lines(
      x = input$tmaxPO4gav, y = c(0, 20), 
      line = list(size = 6, color = 'orange', 
                  dashed = "dashdot", width = 6)
    ) %>%
    layout(xaxis = xvar, yaxis = yvar3)
  
  # gather all subplots
  p <- subplot(p1, p2, p3, titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.05) %>%
    config(displayModeBar = FALSE)
  
  p$elementId <- NULL
  p
  hide_legend(p)
  
}
