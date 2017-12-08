# Modal for primary hyperparathyroidism
modal_php1 <- modalDialog(
  title = "About Primary Hyperparathyroidism",
  withMathJax(
    "This is a simulation of primary hyperparathyroidism (PHP1) in the rat. 
    PHP1 is due to a tumor in parathyroid glands.",
    tags$br(), tags$br(),
    fluidRow(
      column(2, align = "left",
             tags$img(src = "php1_notif_1-1.svg")
      ),
      column(4,
             tags$i("Notice that in rats, there are only 2 parathyroid glands 
                    (PTHg), whereas humans have 4.")
      )
    ),
    tags$br(),
    
    fluidRow(
      column(4, align = "left",
             tags$i("PTH synthesis and secretion are tightly regulated. Whereas
                    1,25(OH) vitamin \\(D_3\\) inhibits PTH synthesis, PO4 is known to
                    increase PTH synthesis. Besides, \\([Ca^{2+}]_p\\) is able
                    to modulate PTH secretion through the calcium sensing receptor.
                    In case of hypercalcemia (elevated \\([Ca^{2+}]_p\\)),
                    PTH secretion is blunted. Inversely, during hypocalcemia,
                    PTH secretion is stimulated.")
      ),
      column(8, align = "right",
             tags$img(src = "php1_notif_1-2.svg")
      )
    ),
    "The relationship between PTH secretion and \\([Ca^{2+}]_p\\) is defined 
    by a sigmoidal decreasing curve, according to the figure below.",
    
    tags$br(),
    
    fluidRow(
      column(6, align = "left",
             tags$img(src = "php1_notif_1-3.svg")
      ),
      column(6,
            tags$i("During PHP1, \\([Ca^{2+}]_p\\) is expected to be raised, while
           \\([PO_4]_p\\) drops. PHP1 can be cured using cinacalcet which reduces
            PTH secretion, thereby blunting its effect on Ca and PO4 homeostasis. 
           Ultimately, parathyroid surgery can be a definitive treatment. 
           Yet is it possible (but rare) that the tumor reappears."),
           tags$br(),
           tags$br(),
           tags$i("In the following case study, you will 
           be explained all regulation mechanisms involved during PHP1. Note that
           a rat model of primary hyperparathyroidism already exists
           and can be found here."),
          tags$a("https://www.ncbi.nlm.nih.gov/pubmed/3591940")
      )
    )
  ),
  size = "m"
)

# Modal for hypoparathyroidism
modal_hypopara <- modalDialog(
  title = "About hypoparathyroidism",
  "This is the reverse situation of primary hyperparathyroidism. Therefore, it is
  characterized by a loss of function of parathyroid gland, which can be congenital,
  due to a surgery in the thyroid area (thyroidectomy), or to an immune disease. 
  Notice that, only one gland would be able to maintain normal PTH and calcium 
  levels. Hypoparathyroidism triggers hypocalcemia as well as hyperphosphatemia
  (or at the uper bound of the normal range). 
  Additionally, 1,25(OH)D3 levels are low. In some cases, hypoparathyroidism may
  lead to cardiac failure, mental dysfunctions. Treatments include intravenous
  calcium injection (calcium-gluconate, calcium-carbonate), as well as vitamin D3
  supplementation (1,25(OH) vitamin D3, also known as calcitriol is the active form
  and the faster). Besides, diuretics help in avoiding the hypercalciuria due to
  vitamin D3 supplementation. Indeed, PTH effect on renal calcium reabsorption is
  significantly lowered. Hyperphosphatemia is cured by reducing phosphate intake.",
  tags$img(src = "hypopara_notif_1-1.svg"),
  "In the following case-study, you will be explained why all these symptoms appear.
  Finally, be careful not to mingle hypoparathyroidism and pseudo-hypoparathyroidism,
  the later being associated with a resistance to the parathyroid hormone action.",
  "You can find extra informations here:",
  tags$br(),
  tags$a("http://www.nejm.org/doi/full/10.1056/NEJMcp0803050"),
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