notification_list <- list(
  php1 = c(paste("Welcome on the", "<mark><font color=\"#FF0000\"><b>", 
                 "primary-hyperparathyroidism", "</b></font></mark>", "(PHP1) case study.
           To launch the simulation, click on <b>next</b> button above. At any time,
           you can go back to the previous step by clicking on <b>back</b>.", 
                 "You can <b>hover</b> on", "<mark><font color=\"#FF0000\"><b>", "intestine,", 
                 "</b></font></mark>", "<mark><font color=\"#FF0000\"><b>", "kidney,", 
                 "</b></font></mark>", "<mark><font color=\"#FF0000\"><b>", "bone,", 
                 "</b></font></mark>", "and", "<mark><font color=\"#FF0000\"><b>", "PTHg", 
                 "</b></font></mark>","nodes to display details.", "It will display 
           (when available) additional content. However, you need to have an HSeT account. 
           More informations here: ", tags$b(tags$a("https://hset.org/contact/")), 
                 sep = " "),
           
           paste("PTH increases <b>bone resorption</b>, Ca <b>reabsorption</b> (in TAL and DCT/CNT ) 
           and enhances vitamin D3 <b>conversion</b> into its active form. Furthermore, 
           it represses <b>PO4 reabsorption</b> in the proximal tubule.",
                 sep = " "),
           
           paste("1,25 vitamin D3 synthesis is enhanced by <b>PTH</b>, which stimulates 
           <b>Cyp27b1</b> and inhibits <b>Cyp24a1</b>. Active vitamin D3 increases
           <b>intestinal absorption</b> of Ca and PO4, <b>Ca reabsorption</b> in kidney 
           (DCT/CNT) as well as <b>FGF23 synthesis</b> in bone. 
           Besides, it represses <b>PTH synthesis</b> in parathyroid glands.", 
                 sep = " "),
           
           paste("FGF23 synthesis is mainly enhanced by vitamin D3 and to a 
           lesser extent by PO4. Its main role is to <b>prevent vitamin D3 toxicity</b>
           by repressing its synthesis (activates 
           Cyp24a1 and represses Cyp27b1, so antagonist of PTH). 
           Additionally, it blunts <b>PO4 reabsorption</b> in the proximal tubule. 
           The relationship between PTH and FGF23 are still controversial, to date."),
           
           paste("Ca <b>represses PTH secretion</b> via the <b>calcium sensing receptor</b> (CaSR) in
           parathyroid chief cells. It also reduces the synthesis of the active
           form of vitamin D3. Finally, it activates the CaSR in the kidney,
           thereby <b>lowering the reabsorption of Ca</b> in the TAL."),
           
           paste("Since plasma PO4 levels are decreased, the activation of PTH synthesis
            by PO4 is reduced, as well as the repression of vitamin D3 conversion and 
            the increase of FGF23 synthesis."),
           
           paste("Ionized plasma Ca concentration is", "<mark><font color=\"#FF0000\"><b>",
                 "widely increased", "</b></font></mark>", "during, PHP1, mainly because 
           of an higher intestinal absorption, resorption and reabsorption in kidney. 
           Inversely, PO4 plasma concentration is", "<mark><font color=\"#FF0000\"><b>",
                 "significantly reduced", "</b></font></mark>", "as a consequence 
           of PTH and FGF23 effects on its renal reabsorption. Ca and PO4 are also known to
           regulate hormonal synthesis, for example via the CaSR in parathyroid glands.", 
                 sep = " ")
  ),
  
  hypopara = c("PTH synthesis is decreased in parathyroid gland, mainly caused 
               by a loss of function of parathyroid glands. 
               There may be several explanations such as after a thyroid surgery or 
               an immune-system disease. Hover on PTH node in the diagram to have more details. 
               You can click on the info button (on the right of each checkbox in the control center), 
               to have more information about the current simulation.",
               
               "Bone resorption, Ca reabsorption (in TAL and DCT/CNT ) 
               and vitamin D3 conversion into its active form are decreased. 
               Furthermore, the repression of PO4 reabsorption in the proximal tubule
               is dampened",
               
               "Less vitamin D3 is converted into 1,25 vitamin D3, 
               resulting in a decrease of intestinal absorption of 
               both Ca and PO4, a decreased reabsorption of Ca.",
               
               "FGF23 synthesis is reduced due to lower vitamin D3 levels. 
               Moreover, the elevation of PO4 levels is not enough to 
               compensate the loss of vitamin D3.",
               
               "Because of the decreased levels of plasma Ca, PTH secretion is
               less repressed. Besides, more Ca is reabsorbed in the kidney
               since CaSR is less activated. The inhibitory effect of Ca on
               D3 synthesis is also blunted.",
               
               "Since plasma PO4 levels are increased, the activation of PTH synthesis
               by PO4 is increased, as well as the repression of vitamin D3 conversion and 
               the increase of FGF23 synthesis.",
               
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
             
             "The reduction of plasma Ca levels cause an increase of PTH exocytosis
             since CaSR is less activated. The decrease of Ca inhibitory effect on
             D3 synthesis, as well as elevated PTH levels are not enough significant 
             to compensate for 25(OH)D3 deficiency. Finally, CaSR is less activated in the
             kidney so as to enhance Ca reabsorption.",
             
             "Activation of PTH synthesis by PO4 is reduced, as well as its effect
             on D3 synthesis. Besides, FGF23 synthesis is less activated",
             
             "Ionized plasma Ca concentration and PO4 remain quite stable as 
             long as vitamin D3 stocks are not totally depleted. However, 
             they start to decrease as soon as the level of
             vitamin D3 is below a given critical threshold. 
             Consequently, all fluxes are significantly reduced.")
)