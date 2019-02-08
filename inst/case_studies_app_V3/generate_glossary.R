# *------------------------------------------------------------------
# | PROGRAM NAME: generate_glossary.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the glossary of the dashboard 
# *-----------------------------------------------------------------
# | DATA USED:  links to HSet website    
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)          
# |
# |
# *------------------------------------------------------------------

generate_glossary <- function() {
  m <- data.frame(
    "abreviation" = c("Ca", "Pi", "PTH", "D3", "FGF23", 
                      "PTHg", "CaSR", "VDR", "PHP1"),
    "full name" = c(
      "Ionized plasma calcium concentration",
      "Total plasma phosphate concentration",
      "Parathyroid hormone",
      "1,25 dihydroxy vitamin D3 (calcitriol)",
      "Fibroblast growth factor 23",
      "Parathyroid glands",
      "Calcium sensing receptor",
      "Vitamin D receptor",
      "Primary hyperparathyroidism"),
    "units" = c("mM (mmol/l)", "mM", "ng/l", rep("", 6))
  )
  datatable(m, escape = c(rep(FALSE, 3), TRUE), options = list(dom = 't')) %>%
    formatStyle('full.name',  color = 'black', backgroundColor = 'orange', 
                fontWeight = 'bold')
}