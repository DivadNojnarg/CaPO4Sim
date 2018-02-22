graph_notification_list <- list(
  php1 = paste("The coefficient of PTH production may be 
               <b>increased</b> several times its base case value (until 300 times).
               This can be achieved using the", "<mark><font color=\"#FF0000\"><b>",
               "orange slider", "</b></font></mark>", "below the graph window.
               \\([Ca^{2+}]_p\\), \\([P_i]_p\\), \\([PTH]_p\\), 
               \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and 
               phosphate fluxes are calculated at <b>equilibrium</b> and", 
               "<mark><font color=\"#FF0000\"><b>", "normalized", 
               "</b></font></mark>", "compared to their initial equilibrium value 
               (namely when PTH production is normal). The case study starts with 
               the slider set to <b>0</b>. Therefore, all normalized 
               concentrations and fluxes are equal to 1.", sep = " "),
  
  
  hypopara = paste("This is a simulation of hypoparathyroidism in the rat. 
                   The coefficient of PTH production is <b>decreased</b> several 
                   times its base case value (until 0). This can be achieved using the", 
                   "<mark><font color=\"#FF0000\"><b>", "orange slider", 
                   "</b></font></mark>", "below the graph window. \\([Ca^{2+}]_p\\),
                   \\([P_i]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and \\([FGF]_p\\), 
                   as well as calcium and phosphate fluxes are calculated at 
                   <b>equilibrium</b> and", "<mark><font color=\"#FF0000\"><b>", 
                   "normalized", "</b></font></mark>", "compared to their initial
                   equilibrium value (namely when PTH production is normal). 
                   What is displayed are all these concentations and fluxes as a 
                   function of the normalized PTH synthesis rate (and not as a
                   function of time). On the x-axis, <b>the value 1 represents 
                   the base-case</b>, while 0.5 corresponds to a division by a 
                   factor 2 of the PTH production rate and 0 represents a 
                   total inhition of PTH synthesis. The slider starts from the
                   base case, namely 1.", sep = " "),
  
  hypoD3 = paste("This is a simulation of vitamin D\\(_3\\) deficiency in 
                  the rat. The concentration of inactive vitamin D (25(OH)D) 
                  may be <b>decreased</b> several times its base case value (until 0). 
                  This can be achieved using the", "<mark><font color=\"#FF0000\"><b>", 
                 "orange slider", "</b></font></mark>", "below the graph window.
                  \\([Ca^{2+}]_p\\), \\([P_i]_p\\), \\([PTH]_p\\), 
                  \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and 
                  phosphate fluxes are calculated at equilibrium and
                  normalized compared to their initial equilibrium value 
                  (namely when PTH production is normal).
                  What is displayed are all these concentations and fluxes as 
                  a function of the normalized 25(OH)D concentration 
                  (and not as a function of time). On the x-axis, <b>the value 
                  1 represents the base-case</b>, while 0.5 corresponds to a 
                  division by a factor 2 of the 25(OH)D concentration
                  and 0 represents the absence of 25(OH)D. The slider starts from the
                  base case, namely 1.", sep = " ")
)