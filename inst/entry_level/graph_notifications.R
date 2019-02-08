# *------------------------------------------------------------------
# | PROGRAM NAME: graph_notifications.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the datas to generate all
# |           notifications in the graph part. Needed by the
# |           function generate_notification() of model_utils.R
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

graph_notification_list <- list(
  php1 = paste(
    "Graphs display  \\([Ca^{2+}]_p\\), \\([P_i]_p\\), \\([PTH]_p\\),
    \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and
    phosphate fluxes at equilibrium", "<mark><font color=\"#FF0000\"><b>", 
    "normalized", "</b></font></mark>", "by their value for a baseline healthy case.
    Under normal physiological conditions, all normalized concentrations and 
    fluxes are equal to 1. Values >1 and <1 denote up- and 
    down-regulations, respectively.", "<hr>",
    "To reproduce the effect of", "<mark><font color=\"#FF0000\"><b>", "PHP1", 
    "</b></font></mark>", "and PTH 
    hyperactivity, you may modulate the normalized PTH 
    synthesis rate using the orange slider below. As above, normal PTH 
    synthesis is equal to 1. On each graph, normalized concentrations and 
    fluxes corresponding to retained PTH mRNA synthesis level are shown by the 
    vertical orange bar", sep = " "
  ),
  
  
  hypopara = paste(
    "Graphs display  \\([Ca^{2+}]_p\\), \\([P_i]_p\\), \\([PTH]_p\\),
    \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and
    phosphate fluxes at equilibrium", "<mark><font color=\"#FF0000\"><b>", 
    "normalized", "</b></font></mark>", "by their value for a baseline healthy case.
    Under normal physiological conditions, all normalized concentrations and 
    fluxes are equal to 1. Values >1 and <1 denote up- and 
    down-regulations, respectively.", "<hr>",
    "To reproduce the effect of",  "<mark><font color=\"#FF0000\"><b>", 
    "hypoparathyroidism", "</b></font></mark>", "you may modulate the normalized PTH 
    synthesis rate using the orange slider below. As above, normal PTH 
    synthesis is equal to 1. On each graph, normalized concentrations and 
    fluxes corresponding to retained PTH mRNA synthesis level are shown by the 
    vertical orange bar", sep = " "
  ),
  
  hypoD3 = paste(
    "Graphs display  \\([Ca^{2+}]_p\\), \\([P_i]_p\\), \\([PTH]_p\\),
    \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and
    phosphate fluxes at equilibrium", "<mark><font color=\"#FF0000\"><b>", 
    "normalized", "</b></font></mark>", "by their value for a baseline healthy case.
    Under normal physiological conditions, all normalized concentrations and 
    fluxes are equal to 1. Values >1 and <1 denote up- and 
    down-regulations, respectively.", "<hr>",
    "To reproduce the effect of",  "<mark><font color=\"#FF0000\"><b>", 
    "vitamin D3 deficiency", "</b></font></mark>", "you may modulate the 
    normalized concentration of 25(OH)D (vitamin D3 storage form)
    using the orange slider below. As above, normal 25(OH)D concentration
    is equal to 1. On each graph, normalized concentrations and 
    fluxes corresponding to retained 25(OH)D concentration are shown by the 
    vertical orange bar", sep = " "
  )
)