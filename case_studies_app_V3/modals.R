# Modal for primary hyperparathyroidism
modal_php1 <- modalDialog(
  title = "About Primary Hyperparathyroidism",
  withMathJax("This is a simulation of primary hyperparathyroidism in 
              the rat. The coefficient of PTH production will be 
              increased several times its base case value (until 300 times). 
              Then \\([Ca^{2+}]_p\\), \\([PO_4]_p\\), \\([PTH]_p\\), 
              \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and 
              phosphate fluxes are calculated at equilibrium and normalized 
              compared to their initial equilibrium value (namely when 
              PTH production is normal). What is displayed are all these 
              concentations and fluxes as a function of the normalized 
              PTH synthesis rate (and not as a function of time). A rat 
              model of primary hyperparathyroidism already exists
              and can be found here: 
              https://www.ncbi.nlm.nih.gov/pubmed/3591940"),
  size = "m"
)

# Modal for hypoparathyroidism
modal_hypopara <- modalDialog(
  title = "About hypoparathyroidism",
  withMathJax("This is a simulation of hypoparathyroidism in the rat. 
               The coefficient of PTH production will be decreased several 
               times its base case value (until 0 times). Then \\([Ca^{2+}]_p\\),
               \\([PO_4]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and \\([FGF]_p\\), 
               as well as calcium and phosphate fluxes are calculated at 
               equilibrium and normalized compared to their initial
               equilibrium value (namely when PTH production is normal). 
               What is displayed are all these concentations and fluxes as a 
               function of the normalized PTH synthesis rate (and not as a
               function of time). On the x-axis, the value 1 represents 
               the base-case, while 0.5 corresponds to a division by a 
               factor 2 of the PTH production rate and 0 represents a 
               total inhition of PTH synthesis."),
  # include image in modal is also possible
  img(src = "bone.png"), 
  size = "m"
)

# Modal for vitamin D3 deficiency
modal_hypoD3 <- modalDialog(
  title = "About vitamin D3 deficiency",
  withMathJax("This is a simulation of vitamin D\\(_3\\) deficiency in 
               the rat. The concentration of inactive vitamin D (25(OH)D) 
               will be decreased several times its base case value (until 0
               times). Then \\([Ca^{2+}]_p\\), \\([PO_4]_p\\), \\([PTH]_p\\), 
               \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and 
               phosphate fluxes are calculated at equilibrium and
               normalized compared to their initial equilibrium value 
               (namely when PTH production is normal).
               What is displayed are all these concentations and fluxes as 
               a function of the normalized 25(OH)D concentration 
               (and not as a function of time). On the x-axis, the value 
               1 represents the base-case, while 0.5 corresponds to a 
               division by a factor 2 of the 25(OH)D concentration
               and 0 represents the absence of 25(OH)D."),
  size = "m"
)

# Modal for Ca iv injection
modal_Ca_inject <- modalDialog(
  title = "About Calcium infusion and EGTA infusion",
  withMathJax("This is a simulation of induced hypercalcemia followed by 
               acute hypocalcemia in 250g male Wistar rats 
               (https://www.ncbi.nlm.nih.gov/pubmed/24801007). 
               Hypercalcemia is induced by continuously injecting a solution 
               containing calcium during 60 min, and followed by EGTA
               infusion (30 mM, rate: 3mL/min) during 60 min so that 
               ionized plasma calcium concentration is decreased by 0.3 mM. 
               On the y-axis are represented normalized \\([Ca^{2+}]_p\\) and
               \\([PTH]_p\\) as a function of time. Experimental 
               observations are shown as black dots"),
  size = "m"
)

# Modal for PO4 iv injection
modal_PO4_inject <- modalDialog(
  title = "About PO4 infusion",
  withMathJax("This is a simulation of intravenouse phosphate load in 
               250g-350g male wistar rats
               (https://www.ncbi.nlm.nih.gov/pubmed/28246304). 
               A solution containing 0.5 mmol \\(Na_2HPO_4\\)
               and \\(NaH_2PO_4\\) was infused during 2-3 minutes to rats. 
               On the y-axis are represented \\([PO_4]_{tot}\\) 
               (total plasma concentration of phosphate), normalized 
               \\([Ca^{2+}]_p\\) and \\([PTH]_p\\) as a function of time 
               (250 minutes). Experimental observations are shown as black
               dots."),
  size = "m"
)

# Modal for PO4 gavage
modal_PO4_gavage <- modalDialog(
  title = "About PO4 gavage",
  withMathJax("This is a simulation of phosphate gavage in 250g-350g male 
               wistar rats (https://www.ncbi.nlm.nih.gov/pubmed/28246304). 
               A solution containing 0.5 mmol \\(Na_2HPO_4\\) and 
               \\(NaH_2PO_4\\) was used. On the y-axis are represented 
               \\([PO_4]_{tot}\\) (total plasma concentration of phosphate), 
               normalized \\([Ca^{2+}]_p\\) and \\([PTH]_p\\) as a function of
               time (250 minutes). Experimental observations are shown as black 
               dots."),
  size = "m"
)