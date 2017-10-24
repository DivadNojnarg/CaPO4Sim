notification_list <- list(
  php1 = c("PTH synthesis is increased in parathyroid gland, mainly caused by a tumor.
           Hover on PTH node in the diagram to have more details.
           You can click on the info button (on the right of each checkbox in the control center),
           to have more information about the current simulation. To launch the simulation,
           click on next button below, and on back if you want to go back.",
           
           "PTH increases bone resorption, Ca reabsorption (in TAL and DCT/CNT ) 
           and enhances vitamin D3 conversion into its active form. Furthermore, 
           it represses PO4 reabsorption in the proximal tubule",
           
           "1,25 vitamin D3 synthesis is enhanced by PTH, which stimulates 
           Cyp27b1 and inhibits Cyp24a1. Active vitamin D3 increases
           intestinal absorption of Ca and PO4, Ca reabsorption in kidney 
           (DCT/CNT) as well as FGF23 synthesis in bone. 
           Besides, it represses PTH synhtesis in parathyroid glands.",
           
           "FGF23 synthesis is mainly enhanced by vitamin D3 and to a 
           lesser extent by PO4. Its main role is to prevent vitamin D3 toxicity 
           by repressing its synthesis (activates 
           Cyp24a1 and represses Cyp27b1, so antagonist of PTH). 
           Additionally, it blunts PO4 reasbsorption in the proximal tubule. 
           The relationship between PTH and FGF23 are still controversial, to date.",
           
           "Ca represses PTH secretion via the calcium sensing receptor (CaSR) in
           parathyroid chief cells. It also reduces the synthesis of the active
           form of vitamin D3. Finally, it activates the CaSR in the kidney,
           thereby lowering the reabsorption of Ca in the TAL.",
           
           "Since plasma PO4 levels are decreased, the activation of PTH synthesis
            by PO4 is reduced, as well as the repression of vitamin D3 conversion and 
            the increase of FGF23 synthesis.",
           
           "Ionized plasma Ca concentration is widely increased during 
           primary hyperparathyroidism, mainly because of an higher intestinal 
           absorption, resorption and reabsorption in kidney. 
           Inversely, PO4 plasma concentration is significantly reduced as a consequence 
           of PTH and FGF23 effects on its renal reabsorption. Ca and PO4 are also known to
           regulate hormonal synthesis, for example via the CaSR in parathyroid glands."),
  
  hypopara = c("PTH synthesis is decreased in parathyroid gland, mainly caused 
               by a loss of function of parathyroid glands. 
               There may be several explanations such as after a thyroid surgery or 
               an immune-system disease. Hover on PTH node in the diagram to have more details. 
               You can click on the info button (on the right of each checkbox in the control center), 
               to have more information about the current simulation.",
               
               "Less vitamin D3 is converted into 1,25 vitamin D3, 
               resulting in a decrease of intestinal absorption of 
               both Ca and PO4, a decreased reabsorption of Ca.",
               
               "FGF23 synthesis is reduced due to lower vitamin D3 levels. 
               Moreover, the elevation of PO4 levels is not enough to 
               compensate the loss of vitamin D3.",
               
               "Ionized plasma Ca concentration is widely decreased since intestinal 
               absorption, resorption and reabsorption in kidney are substantially reduced. 
               Inversely, PO4 plasma concentration is significantly enhanced as a consequence 
               of PTH and FGF23 effects on its renal reabsorption, which are lower. 
               Besides, the PO4 rise is cannot compensate the loss of parathyroid function."),
  
  hypoD3 = c("Vitamin D3 deficiency can be due to a deficit in sun exposure 
             (especially in winter) as well as low vitamin D intake.
             Besides, several diseases are known to cause vitamin D3 
             deficiency such as chronic kidney disease. Hover on D3 node in 
             the diagram to have more details. You can click on the info button 
             (on the right of each checkbox in the control center), to have more 
             information about the current simulation.",
             
             "1,25 vitamin D3 effects on PTH synthesis, 
             FGF23 synthesis and kidney are reduced.",
             
             "Plasma PTH concentration increases because of the blunted repression by 
             1,25 vitamin D3, as well as the reduction of ionized plasma Ca concentration 
             via the calcium sensing receptor. Thus, resorption is increased in order 
             to compensate the reduced intestinal absorption of both Ca and PO4. Similarly, 
             the elevation of PTH levels aims at increasing Ca reabsorption as well as decreasing 
             PO4 reabsorption in kidney. Besides, the increase of PTH also slightly 
             counteract the decrease of vitamin D3 stocks.",
             
             "FGF23 synthesis is reduced as a result 
             of vitamin D3 deficiency.",
             
             "Ionized plasma Ca concentration and PO4 remain quite stable as 
             long as vitamin D3 stocks are not totally depleted. However, 
             they start to decrease as soon as the level of
             vitamin D3 is below a given critical threshold. 
             Consequently, all fluxes are significantly reduced.")
)