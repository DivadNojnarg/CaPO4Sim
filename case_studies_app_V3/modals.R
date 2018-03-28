# Modal for primary hyperparathyroidism
modal_php1 <- modalDialog(
  title = "About Primary Hyperparathyroidism",
  withMathJax(
    
    tags$i(
      "As shown in the figure, parathyroid hormone (PTH)", 
      HTML(paste0("<b>synthesis</b>")), "and", HTML(paste0("<b>secretion</b>")), "are 
      tightly regulated. Known regulators of PTH synthesis include plasma 
      levels of 1,25(OH) vitamin  D3 (\\([D_3]_p\\)), 
      which inhibit PTH synthesis, and  plasma levels of phosphate (\\([P_i]_p\\)) 
      that promote it. Plasma levels of calcium (\\([Ca^{2+}]_p\\)) 
      further modulate (inhibit) PTH secretion into the blood stream through 
      the action of the calcium sensing receptor (CaSR). As a result PTH secretion 
      is blunted in hypercalcemia (elevated  \\([Ca^{2+}]_p\\)) while it is 
      stimulated in hypocalcemia. The relationship between PTH secretion 
      and \\([Ca^{2+}]_p\\) is defined by a sigmoidal decreasing curve, 
      according to the figure below."
    ),
    
    tags$br(), tags$br(),
    
    fluidRow(
      column(6, align = "left",
        tags$a(href = "/modal_php1/modal_php1_2.svg", target = "_blank",
          tags$img(src = "/modal_php1/modal_php1_2.svg")
        )
      ),
      column(6, align = "right",
        tags$a(href = "/modal_php1/modal_php1_3.svg", target = "_blank",
          tags$img(src = "/modal_php1/modal_php1_3.svg")
          )
      )
    ),
    
    tags$br(),
    
    fluidRow(
      column(2, align = "left",
        tags$a(href = "/modal_php1/modal_php1_1.svg", target = "_blank",
             tags$img(src = "/modal_php1/modal_php1_1.svg")
        )
      ),
      column(10,
             tags$i(
               "Contrary to humans who have 4 parathyroid glands 
                (PTHg), rats have only 2. Primary hyperparathyroidism
                (PHP1), is characterized by an oversecretion of PTH despite
                elevated \\([Ca^{2+}]_p\\) (which should normally abolish
                its secretion). PHP1 can be due to a solitary 
                adenoma in parathyroid glands, which is the
                case in 90-95% of clinical cases. However, this can be 
                sometimes due to familial genetic disorders 
                (5%) or parathyroid carcinoma (1%).")
      )
    ),
    
    tags$br(),
           tags$i("During PHP1, \\([Ca^{2+}]_p\\) is expected to rise, while
           \\([P_i]_p\\) drops. PHP1 can be cured using cinacalcet which reduces
           PTH secretion, thereby blunting its effect on Ca and Pi homeostasis. 
           Ultimately, parathyroid surgery can be a definitive treatment. 
           Yet is it possible (but rare) that the tumor reappears."),
           tags$br(),
           tags$br(),
           tags$i("In the following case study, you will be able to explore 
           the regulation mechanisms and consequences of PHP1. Note that
           a rat model of primary hyperparathyroidism already exists
           and can be found here."),
          tags$a("https://www.ncbi.nlm.nih.gov/pubmed/3591940")
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
  tags$img(src = "/modal_hypopara/modal_hypopara.svg"),
  "In the following case study, you will be able to explore the regulation 
  mechanisms and consequences of hypoparathyroidism.
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
  "As shown in the figure below, vitamin D3 synthesis's pathway is complex and has
  two main origins: a reaction between UV rays and skin and from the food (vitamin D3,
  vitamin D2). The storage form of vitamin D is 25(OH)D, also known as calcidiol, whose
  conversion into calcitriol (active form of vitamin D3) is extremely regulated.
  Vitamin D deficiency refers to a deficiency in the storage form, namely 25(OH)D,
  which may be due to a reduced skin synthesis, a decreased intake, liver failure, 
  chronic kidney disease, nephrotic syndrom.
  About 1 billion of people on earth have vitamin D deficiency.",
  tags$img(src = "/modal_hypoD3/modal_hypoD3.svg"),
  "Vitamin D deficiency causes hypocalcemia, hypophosphatemia as well as high PTH levels 
  (secondary hyperparathyroidism).
  This may result in bone diseases such as osteoporosis as well as osteopenia.
  To treat vitamin D deficiency, a common approach is to use vitamin D supplements.
  However, inadequat treatment may result in massive hypercalcemia, hypercalciuria,
  with the risk of kidney stones formation, mainly if the treatment is extended.
  In the following case study, you will be explained all mechanisms and consequences
  involved during vitamin D3 deficiency.",
  "You can find extra informations here:",
  tags$br(),
  tags$a("http://www.nejm.org/doi/full/10.1056/NEJMra070553"),
  tags$a("http://www.nejm.org/doi/pdf/10.1056/NEJMp1608005"),
  tags$a("http://www.nejm.org/doi/full/10.1056/NEJMc063341#t=article"),
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

# Modal for Pi iv injection
modal_PO4_inject <- modalDialog(
  title = "About Pi infusion",
  withMathJax("This is a simulation of intravenouse phosphate load in 
               250g-350g male wistar rats
               (https://www.ncbi.nlm.nih.gov/pubmed/28246304). 
               A solution containing 0.5 mmol \\(Na_2HP_i\\)
               and \\(NaH_2P_i\\) was infused during 2-3 minutes to rats. 
               On the y-axis are represented \\([P_i]_{tot}\\) 
               (total plasma concentration of phosphate), normalized 
               \\([Ca^{2+}]_p\\) and \\([PTH]_p\\) as a function of time 
               (250 minutes). Experimental observations are shown as black
               dots."),
  size = "m"
)

# Modal for Pi gavage
modal_PO4_gav <- modalDialog(
  title = "About Pi gavage",
  withMathJax("This is a simulation of phosphate gavage in 250g-350g male 
               wistar rats (https://www.ncbi.nlm.nih.gov/pubmed/28246304). 
               A solution containing 0.5 mmol \\(Na_2HP_i\\) and 
               \\(NaH_2P_i\\) was used. On the y-axis are represented 
               \\([P_i]_{tot}\\) (total plasma concentration of phosphate), 
               normalized \\([Ca^{2+}]_p\\) and \\([PTH]_p\\) as a function of
               time (250 minutes). Experimental observations are shown as black 
               dots."),
  size = "m"
)