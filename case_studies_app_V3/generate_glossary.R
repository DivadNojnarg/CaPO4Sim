generate_glossary <- function() {
  m <- data.frame(
    "abreviation" = c("Ca", "Pi", "PTH", "D3", "FGF23", 
                      "PTHg", "CaSR", "VDR", "PHP1"),
    "full name" = c("Ionized plasma calcium concentration",
                    "Total plasma phosphate concentration",
                    "Parathyroid hormone",
                    "1,25 dihydroxy vitamin D3 (calcitriol)",
                    "Fibroblast growth factor 23",
                    "Parathyroid glands",
                    "Calcium sensing receptor",
                    "Vitamin D receptor",
                    "Primary hyperparathyroidism"),
    "units" = c("mM (mmol/l)", "mM", "ng/l", rep("", 6)),
    "more informations (HSet)" = c(
      "<a href=\"https://kidneynccr.bio-med.ch/cms/Default?
      Page=23937&Menu=1079&backbar=0\" target=\"_blank\">More about Calcium</a>",
      
      "<a href=\"https://kidneynccr.bio-med.ch/cms/Default?Page=18891&Menu=1079&
      backbar=0\" target=\"_blank\">More about Phosphate</a>",
      
      "<a href=\"https://kidneynccr.bio-med.ch/cms/Default?Page=23466&Menu=1079&
      backbar=0\" target=\"_blank\">More about PTH</a>",
      
      "<a href=\"https://kidneynccr.bio-med.ch/cms/Default?Page=23484&Menu=1079&
      backbar=0\" target=\"_blank\">More about vitamin D3</a>",
      
      "<a href=\"https://kidneynccr.bio-med.ch/cms/Default?Page=23408&Menu=1079&
      backbar=0\" target=\"_blank\">More about FGF23</a>",
      
      "",
      "<a href=\"https://kidneynccr.bio-med.ch/cms/Default?Page=23495&Menu=1079&
      backbar=0\" target=\"_blank\">More about the CaSR</a>",
      rep("", 2)
    )
  )
  datatable(m, escape = c(rep(FALSE, 3), TRUE), options = list(dom = 't')) %>%
    formatStyle('full.name',  color = 'black', backgroundColor = 'orange', 
                fontWeight = 'bold')
}