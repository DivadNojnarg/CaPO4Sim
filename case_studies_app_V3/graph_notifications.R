graph_notification_list <- list(
  php1 = paste(
    "The coefficient of PTH production may be
    <b>increased</b> several times its base case value (until 200 times).
    This can be achieved using the",
    "<mark><font color=\"#FF0000\"><b>",
    "orange slider",
    "</b></font></mark>",
    "below the graph window." ,
    "<hr>",
    "\\([Ca^{2+}]_p\\), \\([P_i]_p\\), \\([PTH]_p\\),
    \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and
    phosphate fluxes are calculated at <b>equilibrium</b> and",
    "<mark><font color=\"#FF0000\"><b>",
    "normalized",
    "</b></font></mark>",
    "compared to their initial equilibrium value
    (namely when PTH production is normal). What is displayed are all
    these concentations and fluxes as a
    function of the normalized PTH synthesis rate (and not as a
    function of time). On the x-axis, <b>the value 100 represents
    an increase of 100 of the PTH production rate</b>,
    while 200 corresponds to an increase of 200
    and 20 represents a increase of 20 (compared to the base case).
    The slider starts from the
    intermediate case, namely <b>100</b>.",
    sep = " "
  ),
  
  
  hypopara = paste(
    "This is a simulation of hypoparathyroidism in the rat.
    The coefficient of PTH production is <b>decreased</b> several
    times its base case value (until 0). This can be achieved using the",
    "<mark><font color=\"#FF0000\"><b>",
    "orange slider",
    "</b></font></mark>",
    "below the graph window.",
    "<hr>",
    "\\([Ca^{2+}]_p\\),
    \\([P_i]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and \\([FGF]_p\\),
    as well as calcium and phosphate fluxes are calculated at
    <b>equilibrium</b> and",
    "<mark><font color=\"#FF0000\"><b>",
    "normalized",
    "</b></font></mark>",
    "compared to their initial
    equilibrium value (namely when PTH production is normal).
    What is displayed are all these concentations and fluxes as a
    function of the normalized PTH synthesis rate (and not as a
    function of time). On the x-axis, <b>the value 0.5 represents
    a division by a factor 2 of the PTH production rate</b>,
    while 0.1 corresponds to a division by a factor 10  and 0 represents a
    total inhition of PTH synthesis. The slider starts from the
    worst case, namely 0.",
    sep = " "
  ),
  
  hypoD3 = paste(
    "This is a simulation of vitamin D\\(_3\\) deficiency in
    the rat. The concentration of inactive vitamin D (25(OH)D)
    may be <b>decreased</b> several times its base case value (until 0).
    This can be achieved using the",
    "<mark><font color=\"#FF0000\"><b>",
    "orange slider",
    "</b></font></mark>",
    "below the graph window.",
    "<hr>",
    "\\([Ca^{2+}]_p\\), \\([P_i]_p\\), \\([PTH]_p\\),
    \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and
    phosphate fluxes are calculated at equilibrium and
    normalized compared to their initial equilibrium value
    (namely when PTH production is normal).
    What is displayed are all these concentations and fluxes as
    a function of the normalized 25(OH)D concentration
    (and not as a function of time). On the x-axis, <b>the value 0.5
    represents a division by a factor 2 of the 25(OH)D concentration</b>,
    while 0.1 corresponds to a division by a factor 10 and 0 represents
    the absence of 25(OH)D. The slider starts from the
    worst case, namely 0.",
    sep = " "
  )
)