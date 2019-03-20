# define color palettes
colfuncCa <- colorRampPalette(c("darkblue", "lightblue", "green"))(20)
colfuncPO4 <- colorRampPalette(c("darkred", "pink", "yellow"))(20)
# plot(rep(1,20),col=colfuncPO4,pch=19,cex=3)

# .... #
# php1 #
# .... #

#' @title Produce plots related to primary hyperparathyroidism (php1)
#'
#' @description Use inside the \link{plotBox} module.
#'
#' @param sliderVal Shiny slider input related to the current disease severity.
#' See \link{plotBox}.
#' @param isMobile Shiny input useful to scale elements based on the device screen size.
#'
#' @importFrom utils read.csv
#'
#' @export
make_plot_php1 <- function(sliderVal, isMobile) {

  # load path to data
  path_to_php1 <- system.file("extdata", "php1.csv", package = "CaPO4Sim")
  php1_table <- read.csv(path_to_php1)
  # create the sequence of PTH production rate
  php1_vec <- 4.192 * seq(1, 300,by = 10)
  names(php1_vec) <- paste("k_prod_PTHg =", php1_vec)

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
  plot_CaP_php1 <- plotly::plot_ly(
    php1_table, x = php1_vec / php1_vec[1],
    y = php1_table[,"Ca_p"] / php1_table[1,"Ca_p"],
    type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
    line = list(color = 'rgb(27, 27, 244)', width = 2),
    showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"PO4_p"] / php1_table[1,"PO4_p"],
      line = list(color = 'rgb(244, 27, 27)', width = 2),
      name = "<b>[Pi]p</b>",
      showlegend = FALSE
      ) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 1.8),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    plotly::add_annotations(
      x = 400, y = 2.6, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>",
      showarrow = TRUE) %>%
    plotly::add_annotations(
      x = 400, y = 0.3, xref = "x", yref = "y",text = "<b>[Pi]p</b>",
      showarrow = TRUE) %>%
    plotly::layout(xaxis = NULL, yaxis = yvar1)

  # plot PTH, D3 and FGF23 concentrations
  plot_hormones_php1 <- plotly::plot_ly(
    php1_table, x = php1_vec / php1_vec[1],
    y = php1_table[,"PTH_p"] / php1_table[1,"PTH_p"],
    type = "scatter", mode = "lines", name = "<b>[PTH]p</b>",
    line = list(color = 'black', width = 2, dash = "dash"),
    showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"D3_p"] / php1_table[1,"D3_p"],
      name = "<b>[D3]p</b>",
      line = list(color = 'black', width = 2, dash = "dot"),
      showlegend = FALSE
      ) %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"FGF_p"] / php1_table[1,"FGF_p"],
      name = "<b>[FGF23]p</b>",
      line = list(color = 'black', width = 2, dash = "solid"),
      showlegend = FALSE
      ) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 6),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    plotly::add_annotations(
      x = 200, y = 5.2, xref = "x2", yref = "y2",
      text = "<b>[PTH]p</b>", showarrow = TRUE, ax = 20, ay = 25
      ) %>%
    plotly::add_annotations(
      x = 400, y = 10, xref = "x2", yref = "y2", text = "<b>[D3]p</b>",
      showarrow = TRUE) %>%
    plotly::add_annotations(
      x = 600, y = 2, xref = "x2", yref = "y2", text = "<b>[FGF23]p</b>",
      showarrow = TRUE) %>%
    plotly::layout(xaxis = NULL, yaxis = yvar2)

  # plot Ca fluxes: resorption, intestinal absorption, urinary excretion
  # and bone storage
  plot_Ca_fluxes_php1 <- plotly::plot_ly(
    php1_table, x = php1_vec / php1_vec[1],
    y = php1_table[,"U_Ca"] / php1_table[1,"U_Ca"],
    type = "scatter", mode = "lines",
    line = list(color = colfuncCa[1], width = 2, dash = "solid"),
    showlegend = FALSE, name = "Urinary Excretion"
    ) %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"Abs_int_Ca"] / php1_table[1,"Abs_int_Ca"],
      line = list(color = colfuncCa[8], width = 2, dash = "solid"),
      showlegend = FALSE, name = "Intestinal absorption") %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"Res_Ca"] / php1_table[1,"Res_Ca"],
      line = list(color = colfuncCa[12], width = 2, dash = "solid"),
      showlegend = FALSE, name = "Bone resorption") %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"Ac_Ca"] / php1_table[1,"Ac_Ca"],
      line = list(color = colfuncCa[17], width = 2, dash = "solid"),
      showlegend = FALSE, name = "FLux into bone") %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6),
      showlegend = FALSE, name = "PHP1 severity") %>%
    plotly::layout(xaxis = xvar, yaxis = yvar3)

  # plot PO4 fluxes: resorption, intestinal absorption, urinary excretion
  # and bone storage
  plot_PO4_fluxes_php1 <- plotly::plot_ly(
    php1_table, x = php1_vec / php1_vec[1],
    y = php1_table[,"U_PO4"] / php1_table[1,"U_PO4"],
    type = "scatter", mode = "lines",
    line = list(color = colfuncPO4[1], width = 2, dash = "solid"),
    name = "Urinary Excretion", showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"Abs_int_PO4"] / php1_table[1,"Abs_int_PO4"],
      line = list(color = colfuncPO4[6], width = 2, dash = "solid"),
      name = "Intestinal absorption", showlegend = FALSE) %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"Res_PO4"] / php1_table[1,"Res_PO4"],
      line = list(color = colfuncPO4[10], width = 2, dash = "solid"),
      name = "Bone resorption", showlegend = FALSE) %>%
    plotly::add_lines(
      x = php1_vec / php1_vec[1],
      y = php1_table[,"Ac_PO4"] / php1_table[1,"Ac_PO4"],
      line = list(color = colfuncPO4[13], width = 2, dash = "solid"),
      name = "FLux into bone", showlegend = FALSE) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 2.5),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6),
      name = "PHP1 severity", showlegend = FALSE) %>%
    plotly::layout(xaxis = xvar, yaxis = yvar4)
  # to display a legend see below
  #layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'h', x = 100, y = -0.2))

  # gather all subplots
  plot_php1 <- plotly::subplot(
    plot_CaP_php1, plot_hormones_php1, plot_Ca_fluxes_php1,
    plot_PO4_fluxes_php1,
    titleX = TRUE,
    titleY = TRUE,
    nrows = if (isMobile) 4 else 2,
    margin = if (isMobile) rep(1, 4) else rep(0.07, 4),
    heights = if (isMobile) rep(0.25, 4) else rep(0.5, 2)
  ) %>%
    plotly::config(displayModeBar = FALSE)

  plot_php1$elementId <- NULL

  plot_php1
}

# .......#
# hypoD3 #
# .......#


#' @title Produce plots related to vitamin D3 deficiency (hypoD3)
#'
#' @description Use inside the \link{plotBox} module.
#'
#' @param sliderVal Shiny slider input related to the current disease severity.
#' See \link{plotBox}.
#' @param isMobile Shiny input useful to scale elements based on the device screen size.
#'
#' @importFrom utils read.csv
#'
#' @export
make_plot_hypoD3 <- function(sliderVal, isMobile) {

  # load path to data
  path_to_hypoD3 <- system.file("extdata", "hypoD3.csv", package = "CaPO4Sim")
  hypoD3_table <- read.csv(path_to_hypoD3)
  # create the sequence of D3 inact and reverse the vector
  hypoD3_vec <- rev(2.5e-005 * seq(0, 1, by = 0.01))
  names(hypoD3_vec) <- paste("D3_inact =", hypoD3_vec)

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
  plot_CaP_hypoD3 <- plotly::plot_ly(
    hypoD3_table, x = hypoD3_vec / hypoD3_vec[1],
    y = hypoD3_table[,"Ca_p"] / hypoD3_table[1,"Ca_p"],
    type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
    line = list(color = 'rgb(27, 27, 244)', width = 2),
    showlegend = FALSE
  ) %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"PO4_p"] / hypoD3_table[1,"PO4_p"],
      line = list(color = 'rgb(244, 27, 27)', width = 2),
      name = "<b>[Pi]p</b>",
      showlegend = FALSE
      ) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    plotly::add_annotations(
      x = 0.7, y = 0.95, xref = "x",
      yref = "y",text = "<b>[Ca2+]p</b>",
      showarrow = TRUE, ax = -20, ay = 40
      ) %>%
    plotly::add_annotations(
      x = 0.4, y = 1.1, xref = "x",
      yref = "y",text = "<b>[Pi]p</b>",
      showarrow = TRUE, ax = -20, ay = -20
      ) %>%
    plotly::layout(xaxis = xvar_bis, yaxis = yvar1)

  # plot PTH, D3 and FGF23 concentrations
  plot_hormones_hypoD3 <- plotly::plot_ly(
    hypoD3_table, x = hypoD3_vec / hypoD3_vec[1],
    y = hypoD3_table[,"PTH_p"] / hypoD3_table[1,"PTH_p"],
    type = "scatter", mode = "lines", name = "<b>[PTH]p</b>",
    line = list(color = 'black', width = 2, dash = "dash"),
    showlegend = FALSE
  ) %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"D3_p"] / hypoD3_table[1,"D3_p"],
      name = "<b>[D3]p</b>",
      line = list(color = 'black', width = 2, dash = "dot"),
      showlegend = FALSE
      ) %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"FGF_p"] / hypoD3_table[1,"FGF_p"],
      name = "<b>[FGF23]p</b>",
      line = list(color = 'black', width = 2, dash = "solid"),
      showlegend = FALSE
      ) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    plotly::add_annotations(
      x = 0, y = 2.3, xref = "x2", yref = "y2",
      text = "<b>[PTH]p</b>", showarrow = TRUE
      ) %>%
    plotly::add_annotations(
      x = 0.9, y = 0.9, xref = "paper", yref = "y2",
      text = "<b>[D3]p</b>", showarrow = TRUE
      ) %>%
    plotly::add_annotations(
      x = 0.9, y = 0.4, xref = "paper", yref = "y2",
      text = "<b>[FGF23]p</b>", showarrow = TRUE,
      ax = -20, ay = 30
      ) %>%
    plotly::layout(xaxis = xvar_bis, yaxis = yvar2)

  # plot Ca fluxes: resorption, intestinal absorption, urinary excretion
  # and bone storage
  plot_Ca_fluxes_hypoD3 <- plotly::plot_ly(
    hypoD3_table, x = hypoD3_vec / hypoD3_vec[1],
    y = hypoD3_table[,"U_Ca"] / hypoD3_table[1,"U_Ca"],
    type = "scatter", mode = "lines",
    line = list(color = colfuncCa[1], width = 2, dash = "solid"),
    name = "Urinary Excretion", showlegend = FALSE
  ) %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"Abs_int_Ca"] / hypoD3_table[1,"Abs_int_Ca"],
      line = list(color = colfuncCa[8], width = 2, dash = "solid"),
      showlegend = FALSE,
      name = "Intestinal absorption") %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"Res_Ca"] / hypoD3_table[1,"Res_Ca"],
      line = list(color = colfuncCa[12], width = 2, dash = "solid"),
      showlegend = FALSE,
      name = "Bone resorption") %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"Ac_Ca"] / hypoD3_table[1,"Ac_Ca"],
      line = list(color = colfuncCa[17], width = 2, dash = "solid"),
      showlegend = FALSE,
      name = "Flux into bone") %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
      ) %>%
    plotly::layout(xaxis = xvar, yaxis = yvar3)

  # plot PO4 fluxes: resorption, intestinal absorption, urinary excretion
  # and bone storage
  plot_PO4_fluxes_hypoD3 <- plotly::plot_ly(
    hypoD3_table, x = hypoD3_vec / hypoD3_vec[1],
    y = hypoD3_table[,"U_PO4"] / hypoD3_table[1,"U_PO4"],
    type = "scatter", mode = "lines",
    line = list(color = colfuncPO4[1], width = 2, dash = "solid"),
    name = "Urinary Excretion", showLegend = FALSE
  ) %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"Abs_int_PO4"] / hypoD3_table[1,"Abs_int_PO4"],
      line = list(color = colfuncPO4[6], width = 2, dash = "solid"),
      name = "Intestinal absorption", showlegend = FALSE) %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"Res_PO4"] / hypoD3_table[1,"Res_PO4"],
      line = list(color = colfuncPO4[10], width = 2, dash = "solid"),
      name = "Bone resorption", showlegend = FALSE) %>%
    plotly::add_lines(
      x = hypoD3_vec / hypoD3_vec[1],
      y = hypoD3_table[,"Ac_PO4"] / hypoD3_table[1,"Ac_PO4"],
      line = list(color = colfuncPO4[13], width = 2, dash = "solid"),
      name = "Flux into bone", showlegend = FALSE) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6),
      name = "HypoD3 severity", showlegend = FALSE
      ) %>%
    plotly::layout(xaxis = xvar, yaxis = yvar4)
    # layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'h', x = 100, y = -0.2))

  # gather all subplots
  plot_hypoD3 <- plotly::subplot(
    plot_CaP_hypoD3,
    plot_hormones_hypoD3,
    plot_Ca_fluxes_hypoD3,
    plot_PO4_fluxes_hypoD3,
    titleX = TRUE,
    titleY = TRUE,
    nrows = if (isMobile) 4 else 2,
    margin = if (isMobile) rep(1, 4) else rep(0.07, 4),
    heights = if (isMobile) rep(0.25, 4) else rep(0.5, 2)
  ) %>%
    plotly::config(displayModeBar = FALSE)

  plot_hypoD3$elementId <- NULL

  plot_hypoD3
}

# ........ #
# hypopara #
# ........ #


#' @title Produce plots related to hypoparathyroidism (hypopara)
#'
#' @description Use inside the \link{plotBox} module.
#'
#' @param sliderVal Shiny slider input related to the current disease severity.
#' See \link{plotBox}.
#' @param isMobile Shiny input useful to scale elements based on the device screen size.
#'
#' @importFrom utils read.csv
#'
#' @export
make_plot_hypopara <- function(sliderVal, isMobile) {

  # path to data
  path_to_hypopara <- system.file("extdata", "hypopara.csv", package = "CaPO4Sim")
  hypopara_table <- read.csv(path_to_hypopara)
  hypopara_vec <- rev(4.192 * seq(0, 1, by = 0.01)) # create the sequence of PTH production rate
  names(hypopara_vec) <- paste("k_prod_PTHg =",  hypopara_vec)

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
  plot_CaP_hypopara <- plotly::plot_ly(
    hypopara_table, x = hypopara_vec / hypopara_vec[1],
    y = hypopara_table[,"Ca_p"] / hypopara_table[1,"Ca_p"],
    type = "scatter", mode = "lines", name = "<b>[Ca2+]p</b>",
    line = list(color = 'rgb(27, 27, 244)', width = 2),
    showlegend = FALSE
  ) %>%
    plotly::add_lines(
      x = hypopara_vec/hypopara_vec[1],
      y = hypopara_table[,"PO4_p"] / hypopara_table[1,"PO4_p"],
      line = list(color = 'rgb(244, 27, 27)', width = 2),
      name = "<b>[Pi]p</b>",
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
    ) %>%
    plotly::add_annotations(
      x = 0.5, y = 0.85, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>",
      showarrow = TRUE, ax = -20, ay = 40
    ) %>%
    plotly::add_annotations(
      x = 0.1, y = 1.7, xref = "x", yref = "y",text = "<b>[Pi]p</b>",
      showarrow = TRUE
    ) %>%
    plotly::layout(xaxis = xvar_bis, yaxis = yvar1)

  # plot PTH, D3 and FGF23 concentrations
  plot_hormones_hypopara <- plotly::plot_ly(
    hypopara_table, x = hypopara_vec / hypopara_vec[1],
    y = hypopara_table[,"PTH_p"] / hypopara_table[1,"PTH_p"],
    type = "scatter", mode = "lines", name = "<b>[PTH]p</b>",
    line = list(color = 'black', width = 2, dash = "dash"),
    showlegend = FALSE
  ) %>%
    plotly::add_lines(
      x = hypopara_vec / hypopara_vec[1],
      y = hypopara_table[,"D3_p"] / hypopara_table[1,"D3_p"],
      name = "<b>[D3]p</b>",
      line = list(color = 'black', width = 2, dash = "dot"),
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = hypopara_vec/hypopara_vec[1],
      y = hypopara_table[,"FGF_p"]/hypopara_table[1,"FGF_p"],
      name = "<b>[FGF23]p</b>",
      line = list(color = 'black', width = 2, dash = "solid"),
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
    ) %>%
    plotly::add_annotations(
      x = 0.9, y = 0.2, xref = "paper", yref = "y2", text = "<b>[PTH]p</b>",
      showarrow = TRUE, ax = -20, ay = 40
    ) %>%
    plotly::add_annotations(
      x = 0.9, y = 0.67, xref = "paper", yref = "y2", text = "<b>[D3]p</b>",
      showarrow = TRUE, ax = 15, ay = -30
    ) %>%
    plotly::add_annotations(
      x = 0.88, y = 0.5, xref = "paper", yref = "y2", text = "<b>[FGF23]p</b>",
      showarrow = TRUE, ax = -50, ay = 10
    ) %>%
    plotly::layout(xaxis = xvar_bis, yaxis = yvar2)

  # plot Ca fluxes: resorption, intestinal absorption, urinary excretion
  # and bone storage
  plot_Ca_fluxes_hypopara <- plotly::plot_ly(
    hypopara_table, x = hypopara_vec / hypopara_vec[1],
    y = hypopara_table[,"U_Ca"] / hypopara_table[1,"U_Ca"],
    type = "scatter", mode = "lines",
    line = list(color = colfuncCa[1], width = 2, dash = "dash"),
    name = "Urinary Excretion", showlegend = FALSE
  ) %>%
    plotly::add_lines(
      x = hypopara_vec / hypopara_vec[1],
      y = hypopara_table[,"Abs_int_Ca"] / hypopara_table[1,"Abs_int_Ca"],
      line = list(color = colfuncCa[8], width = 2, dash = "dot"),
      name = "Intestinal absorption", showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = hypopara_vec / hypopara_vec[1],
      y = hypopara_table[,"Res_Ca"] / hypopara_table[1,"Res_Ca"],
      line = list(color = colfuncCa[12], width = 2, dash = "dashdot"),
      name = "Bone resorption", showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = hypopara_vec / hypopara_vec[1],
      y = hypopara_table[,"Ac_Ca"] / hypopara_table[1,"Ac_Ca"],
      line = list(color = colfuncCa[17], width = 2, dash = "solid"),
      name = "Flux into bone", showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6)
    ) %>%
    plotly::layout(xaxis = xvar, yaxis = yvar3)

  # plot PO4 fluxes: resorption, intestinal absorption, urinary excretion
  # and bone storage
  plot_PO4_fluxes_hypopara <- plotly::plot_ly(
    hypopara_table, x = hypopara_vec / hypopara_vec[1],
    y = hypopara_table[,"U_PO4"] / hypopara_table[1,"U_PO4"],
    type = "scatter", mode = "lines",
    line = list(color = colfuncPO4[1], width = 2, dash = "dash"),
    name = "Urinary Excretion", showlegend = FALSE
  ) %>%
    plotly::add_lines(
      x = hypopara_vec / hypopara_vec[1],
      y = hypopara_table[,"Abs_int_PO4"] / hypopara_table[1,"Abs_int_PO4"],
      line = list(color = colfuncPO4[6], width = 2, dash = "dot"),
      name = "Intestinal absorption", showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = hypopara_vec / hypopara_vec[1],
      y = hypopara_table[,"Res_PO4"] / hypopara_table[1,"Res_PO4"],
      line = list(color = colfuncPO4[10], width = 2, dash = "dashdot"),
      name = "Bone resorption", showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = hypopara_vec / hypopara_vec[1],
      y = hypopara_table[,"Ac_PO4"] / hypopara_table[1,"Ac_PO4"],
      line = list(color = colfuncPO4[13], width = 2, dash = "solid"),
      name = "Flux into bone", showlegend = FALSE
    ) %>%
    plotly::add_lines(
      x = sliderVal, y = c(0, 4),
      line = list(size = 6, color = 'orange', dash = "solid", width = 6),
      name = "Hypopara severity", showlegend = FALSE
    ) %>%
    plotly::layout(xaxis = xvar, yaxis = yvar4)
  # layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'h', x = 100, y = -0.2))

  # gather all subplots
  plot_hypopara <- plotly::subplot(
    plot_CaP_hypopara,
    plot_hormones_hypopara,
    plot_Ca_fluxes_hypopara,
    plot_PO4_fluxes_hypopara,
    titleX = TRUE,
    titleY = TRUE,
    nrows = if (isMobile) 4 else 2,
    margin = if (isMobile) rep(1, 4) else rep(0.07, 4),
    heights = if (isMobile) rep(0.25, 4) else rep(0.5, 2)
  ) %>%
    plotly::config(displayModeBar = FALSE)

  plot_hypopara$elementId <- NULL

  plot_hypopara

}
