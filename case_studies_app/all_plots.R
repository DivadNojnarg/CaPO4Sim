#------------------------------------------------------------------------- 
#  This codes contains all elements regarding PHP1, hypoD3, hypopara
#  and dynamic simulations plots
#
#-------------------------------------------------------------------------

# .... #
# php1 #
# .... #

make_plot_php1 <- function() {
  
  # load path to data
  path_to_php1 <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/php1.csv" 
  #path_to_php1 <- "/srv/shiny-server/capApp/case_studies_app/www/php1.csv" 
  php1_table <- read.csv(path_to_php1)
  php1_vec <- 4.192*seq(1,300,by = 10) # create the sequence of PTH production rate
  names(php1_vec) <- paste("k_prod_PTHg =", php1_vec)
  
  # define x and y ranges
  xvar <- list(title = "k_prod_PTHg fold increase", 
               range = c(min(php1_vec/php1_vec[1]), 
                         max(php1_vec)/php1_vec[1]))
  yvar1 <- list(title = "Normalized concentrations", range = c(0, 2))
  yvar2 <- list(title = "Normalized concentrations", range = c(0,7))
  yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,5))
  yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,3))
  
  # plot Ca and PO4 variables
  plot_CaP_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], 
                           y = php1_table[,"Ca_p"]/php1_table[1,"Ca_p"],
                           type = "scatter", mode = "lines", 
                           line = list(color = 'rgb(27, 27, 244)', width = 2), 
                           showlegend = F) %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"PO4_p"]/php1_table[1,"PO4_p"], 
              line = list(color = 'rgb(244, 27, 27)', width = 2), showlegend = F) %>%
    add_annotations(x = 400, y = 2.6, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", showarrow = T) %>%
    add_annotations(x = 400, y = 0.3, xref = "x", yref = "y",text = "<b>[PO4]p</b>", showarrow = T) %>%
    layout(xaxis = NULL, yaxis = yvar1)
  
  # plot PTH, D3 and FGF23 concentrations
  plot_hormones_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], 
                                y = php1_table[,"PTH_p"]/php1_table[1,"PTH_p"],
                                type = "scatter", mode = "lines", 
                                line = list(color = 'black', width = 2, dash = "dash"), 
                                showlegend = F) %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"D3_p"]/php1_table[1,"D3_p"], 
              line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"FGF_p"]/php1_table[1,"FGF_p"], 
              line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
    add_annotations(x = 200, y = 5.2, xref = "x2", yref = "y2", text = "<b>[PTH]p</b>", showarrow = T, ax = 20, ay = 25) %>%
    add_annotations(x = 400, y = 10, xref = "x2", yref = "y2", text = "<b>[D3]p</b>", showarrow = T) %>%
    add_annotations(x = 600, y = 2, xref = "x2", yref = "y2", text = "<b>[FGF23]p</b>", showarrow = T) %>%
    layout(xaxis = NULL, yaxis = yvar2)
  
  # plot Ca fluxes: resorption, intestinal absorption, urinary excretion 
  # and bone storage
  plot_Ca_fluxes_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], 
                                 y = php1_table[,"U_Ca"]/php1_table[1,"U_Ca"],
                                 type = "scatter", mode = "lines", 
                                 line = list(color = 'black', width = 2, dash = "dash"), 
                                 showlegend = F) %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Abs_int_Ca"]/php1_table[1,"Abs_int_Ca"], 
              line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Res_Ca"]/php1_table[1,"Res_Ca"], 
              line = list(color = 'black', width = 2, dash = "dashdot"), showlegend = F) %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Ac_Ca"]/php1_table[1,"Ac_Ca"], 
              line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
    layout(xaxis = xvar, yaxis = yvar3)
  
  # plot PO4 fluxes: resorption, intestinal absorption, urinary excretion 
  # and bone storage
  plot_PO4_fluxes_php1 <- plot_ly(php1_table, x = php1_vec/php1_vec[1], 
                                  y = php1_table[,"U_PO4"]/php1_table[1,"U_PO4"],
                                  type = "scatter", mode = "lines", 
                                  line = list(color = 'black', width = 2, dash = "dash"), 
                                  name = "Urinary Excretion") %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Abs_int_PO4"]/php1_table[1,"Abs_int_PO4"], 
              line = list(color = 'black', width = 2, dash = "dot"), 
              name = "Intestinal absorption") %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Res_PO4"]/php1_table[1,"Res_PO4"], 
              line = list(color = 'black', width = 2, dash = "dashdot"), 
              name = "Bone resorption") %>%
    add_lines(x = php1_vec/php1_vec[1], y = php1_table[,"Ac_PO4"]/php1_table[1,"Ac_PO4"], 
              line = list(color = 'black', width = 2, dash = "solid"), 
              name = "FLux into bone") %>%
    layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'v', x = 0.05, y = 0.3))
  
  # gather all subplots
  plot_php1 <- subplot(plot_CaP_php1, plot_hormones_php1, plot_Ca_fluxes_php1, plot_PO4_fluxes_php1, 
                       titleX = TRUE, titleY = TRUE, nrows = 2, margin = 0.07, 
                       heights = c(0.5,0.5))
  
}

# .......#
# hypoD3 #
# .......#

make_plot_hypoD3 <- function() {

# load path to data
path_to_hypoD3 <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/hypoD3.csv"
#path_to_hypoD3 <- "/srv/shiny-server/capApp/case_studies_app/www/hypoD3.csv"
hypoD3_table <- read.csv(path_to_hypoD3)
hypoD3_vec <- rev(2.5e-005*seq(0, 1, by = 0.01)) # create the sequence of D3 inact and reverse the vector
names(hypoD3_vec) <- paste("D3_inact =", hypoD3_vec)


# define x and y ranges
xvar <- list(title = "D3_inact fold decrease", 
             range = c(max(hypoD3_vec/hypoD3_vec[1]), 
                       min(hypoD3_vec)/hypoD3_vec[1]),
             autorange = F, autorange = "reversed")
xvar_bis <- list(title = "", range = c(max(hypoD3_vec/hypoD3_vec[1]), 
                                       min(hypoD3_vec)/hypoD3_vec[1]),
                 autorange = F, autorange = "reversed")
yvar1 <- list(title = "Normalized concentrations", range = c(0, 1.1))
yvar2 <- list(title = "Normalized concentrations", range = c(0,4))
yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,1.2))
yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,1.1))

# plot Ca and PO4 variables
plot_CaP_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], 
                           y = hypoD3_table[,"Ca_p"]/hypoD3_table[1,"Ca_p"],
                           type = "scatter", mode = "lines", 
                           line = list(color = 'rgb(27, 27, 244)', width = 2), 
                           showlegend = F) %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"PO4_p"]/hypoD3_table[1,"PO4_p"], 
            line = list(color = 'rgb(244, 27, 27)', width = 2), showlegend = F) %>%
  add_annotations(x = 0.7, y = 0.95, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", 
                  showarrow = T, ax = -20, ay = 40) %>%
  add_annotations(x = 0.4, y = 1.1, xref = "x", yref = "y",text = "<b>[PO4]p</b>", 
                  showarrow = T, ax = -20, ay = -20) %>%
  layout(xaxis = xvar_bis, yaxis = yvar1)

# plot PTH, D3 and FGF23 concentrations
plot_hormones_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], 
                                y = hypoD3_table[,"PTH_p"]/hypoD3_table[1,"PTH_p"],
                                type = "scatter", mode = "lines", 
                                line = list(color = 'black', width = 2, dash = "dash"), 
                                showlegend = F) %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"D3_p"]/hypoD3_table[1,"D3_p"], 
            line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"FGF_p"]/hypoD3_table[1,"FGF_p"], 
            line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
  add_annotations(x = 0, y = 2.3, xref = "x2", yref = "y2", text = "<b>[PTH]p</b>", showarrow = T) %>%
  add_annotations(x = 0.9, y = 0.9, xref = "paper", yref = "y2", text = "<b>[D3]p</b>", showarrow = T) %>%
  add_annotations(x = 0.9, y = 0.4, xref = "paper", yref = "y2", text = "<b>[FGF23]p</b>", showarrow = T, 
                  ax = -20, ay = 30) %>%
  layout(xaxis = xvar_bis, yaxis = yvar2)

# plot Ca fluxes: resorption, intestinal absorption, urinary excretion 
# and bone storage
plot_Ca_fluxes_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], 
                                 y = hypoD3_table[,"U_Ca"]/hypoD3_table[1,"U_Ca"],
                                 type = "scatter", mode = "lines", 
                                 line = list(color = 'black', width = 2, dash = "dash"), 
                                 showlegend = F) %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Abs_int_Ca"]/hypoD3_table[1,"Abs_int_Ca"], 
            line = list(color = 'black', width = 2, dash = "dot"), showlegend = F) %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Res_Ca"]/hypoD3_table[1,"Res_Ca"], 
            line = list(color = 'black', width = 2, dash = "dashdot"), showlegend = F) %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Ac_Ca"]/hypoD3_table[1,"Ac_Ca"], 
            line = list(color = 'black', width = 2, dash = "solid"), showlegend = F) %>%
  layout(xaxis = xvar, yaxis = yvar3)

# plot PO4 fluxes: resorption, intestinal absorption, urinary excretion 
# and bone storage
plot_PO4_fluxes_hypoD3 <- plot_ly(hypoD3_table, x = hypoD3_vec/hypoD3_vec[1], 
                                  y = hypoD3_table[,"U_PO4"]/hypoD3_table[1,"U_PO4"],
                                  type = "scatter", mode = "lines", 
                                  line = list(color = 'black', width = 2, dash = "dash"),
                                  name = "Urinary Excretion") %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Abs_int_PO4"]/hypoD3_table[1,"Abs_int_PO4"], 
            line = list(color = 'black', width = 2, dash = "dot"), 
            name = "Intestinal absorption") %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Res_PO4"]/hypoD3_table[1,"Res_PO4"], 
            line = list(color = 'black', width = 2, dash = "dashdot"), 
            name = "Bone resorption") %>%
  add_lines(x = hypoD3_vec/hypoD3_vec[1], y = hypoD3_table[,"Ac_PO4"]/hypoD3_table[1,"Ac_PO4"], 
            line = list(color = 'black', width = 2, dash = "solid"), 
            name = "Flux into bone") %>%
  layout(xaxis = xvar, yaxis = yvar4, legend = list(orientation = 'v', x = 0.01, y = 0.05))

# gather all subplots
plot_hypoD3 <- subplot(plot_CaP_hypoD3, plot_hormones_hypoD3, plot_Ca_fluxes_hypoD3, 
                       plot_PO4_fluxes_hypoD3, titleX = TRUE, titleY = TRUE,
                       nrows = 2, margin = 0.07, heights = c(0.5,0.5))

}

# ........ #
# hypopara #
# ........ #

make_plot_hypopara <- function() {

# path to data
path_to_hypopara <- "/Users/macdavidgranjon/Documents//WebApp_CaP_homeostasis/case_studies_app/www/hypopara.csv"
#path_to_hypopara <- "/srv/shiny-server/capApp/case_studies_app/www/hypopara.csv"
hypopara_table <- read.csv(path_to_hypopara)
hypopara_vec <- rev(4.192*seq(0, 1, by = 0.01)) # create the sequence of PTH production rate
names(hypopara_vec) <- paste("k_prod_PTHg =",  hypopara_vec)

# define x and y ranges
xvar <- list(title = "k_prod_PTHg fold decrease", 
             range = c(max(hypopara_vec/hypopara_vec[1]), 
                       min(hypopara_vec)/hypopara_vec[1]),
             autorange = F, autorange = "reversed")
xvar_bis <- list(title = "", 
                 range = c(max(hypopara_vec/hypopara_vec[1]), 
                           min(hypopara_vec)/hypopara_vec[1]),
                 autorange = F, autorange = "reversed")
yvar1 <- list(title = "Normalized concentrations", range = c(0, 1.4))
yvar2 <- list(title = "Normalized concentrations", range = c(0,1))
yvar3 <- list(title = "Normalized Ca fluxes", range = c(0,1))
yvar4 <- list(title = "Normalized PO4 fluxes", range = c(0,1.4))

# plot Ca and PO4 variables
plot_CaP_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], 
                             y = hypopara_table[,"Ca_p"]/hypopara_table[1,"Ca_p"],
                             type = "scatter", mode = "lines", 
                             line = list(color = 'rgb(27, 27, 244)', width = 2), 
                             showlegend = F) %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"PO4_p"]/hypopara_table[1,"PO4_p"], 
            line = list(color = 'rgb(244, 27, 27)', width = 2), 
            showlegend = F) %>%
  add_annotations(x = 0.5, y = 0.85, xref = "x", yref = "y",text = "<b>[Ca2+]p</b>", 
                  showarrow = T, ax = -20, ay = 40) %>%
  add_annotations(x = 0.1, y = 1.7, xref = "x", yref = "y",text = "<b>[PO4]p</b>", 
                  showarrow = T) %>%
  layout(xaxis = xvar_bis, yaxis = yvar1)

# plot PTH, D3 and FGF23 concentrations
plot_hormones_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], 
                                  y = hypopara_table[,"PTH_p"]/hypopara_table[1,"PTH_p"],
                                  type = "scatter", mode = "lines", 
                                  line = list(color = 'black', width = 2, dash = "dash"), 
                                  showlegend = F) %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"D3_p"]/hypopara_table[1,"D3_p"], 
            line = list(color = 'black', width = 2, dash = "dot"), 
            showlegend = F) %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"FGF_p"]/hypopara_table[1,"FGF_p"], 
            line = list(color = 'black', width = 2, dash = "solid"), 
            showlegend = F) %>%
  add_annotations(x = 0.9, y = 0.2, xref = "paper", yref = "y2", text = "<b>[PTH]p</b>", 
                  showarrow = T, ax = -20, ay = 40) %>%
  add_annotations(x = 0.9, y = 0.67, xref = "paper", yref = "y2", text = "<b>[D3]p</b>", 
                  showarrow = T, ax = 15, ay = -30) %>%
  add_annotations(x = 0.88, y = 0.5, xref = "paper", yref = "y2", text = "<b>[FGF23]p</b>", 
                  showarrow = T, ax = -50, ay = 10) %>%
  layout(xaxis = xvar_bis, yaxis = yvar2)

# plot Ca fluxes: resorption, intestinal absorption, urinary excretion 
# and bone storage
plot_Ca_fluxes_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], 
                                   y = hypopara_table[,"U_Ca"]/hypopara_table[1,"U_Ca"],
                                   type = "scatter", mode = "lines", 
                                   line = list(color = 'black', width = 2, dash = "dash"), 
                                   showlegend = F) %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"Abs_int_Ca"]/hypopara_table[1,"Abs_int_Ca"], 
            line = list(color = 'black', width = 2, dash = "dot"), 
            showlegend = F) %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"Res_Ca"]/hypopara_table[1,"Res_Ca"], 
            line = list(color = 'black', width = 2, dash = "dashdot"), 
            showlegend = F) %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"Ac_Ca"]/hypopara_table[1,"Ac_Ca"], 
            line = list(color = 'black', width = 2, dash = "solid"), 
            showlegend = F) %>%
  layout(xaxis = xvar, yaxis = yvar3)

# plot PO4 fluxes: resorption, intestinal absorption, urinary excretion 
# and bone storage
plot_PO4_fluxes_hypopara <- plot_ly(hypopara_table, x = hypopara_vec/hypopara_vec[1], 
                                    y = hypopara_table[,"U_PO4"]/hypopara_table[1,"U_PO4"],
                                    type = "scatter", mode = "lines", 
                                    line = list(color = 'black', width = 2, dash = "dash"),
                                    name = "Urinary Excretion") %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"Abs_int_PO4"]/hypopara_table[1,"Abs_int_PO4"], 
            line = list(color = 'black', width = 2, dash = "dot"),
            name = "Intestinal absorption") %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"Res_PO4"]/hypopara_table[1,"Res_PO4"], 
            line = list(color = 'black', width = 2, dash = "dashdot"),
            name = "Bone resorption") %>%
  add_lines(x = hypopara_vec/hypopara_vec[1], 
            y = hypopara_table[,"Ac_PO4"]/hypopara_table[1,"Ac_PO4"], 
            line = list(color = 'black', width = 2, dash = "solid"),
            name = "Flux into bone") %>%
  layout(xaxis = xvar, yaxis = yvar4, 
         legend = list(orientation = 'v', x = 0.01, y = 0.05))

# gather all subplots
plot_hypopara <- subplot(plot_CaP_hypopara, plot_hormones_hypopara, 
                         plot_Ca_fluxes_hypopara, plot_PO4_fluxes_hypopara, 
                         titleX = TRUE, titleY = TRUE,
                         nrows = 2, margin = 0.07, heights = c(0.5,0.5))

}
