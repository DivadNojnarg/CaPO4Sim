notification_list <- list(
  php1 = c(
    paste(
      "Welcome to the",
      "<mark><font color=\"#FF0000\"><b>",
      "primary-hyperparathyroidism",
      "</b></font></mark>",
      "(PHP1) case study.",
      "Before going further, make sure that <b>regulations</b> are enabled,
      and that Ca and Pi homeostasis are selected.",
      "<hr>",
      "The <b>next</b> and <b>back</b> buttons will walk you through the 
      consequences of PTH overproduction and regulatory pathways involved.",
      "You can <b>hover</b> over",
      "<mark><font color=\"#FF0000\"><b>",
      "intestine,",
      "</b></font></mark>",
      "<mark><font color=\"#FF0000\"><b>",
      "kidney,",
      "</b></font></mark>",
      "<mark><font color=\"#FF0000\"><b>",
      "bone,",
      "</b></font></mark>",
      "and",
      "<mark><font color=\"#FF0000\"><b>",
      "PTHg",
      "</b></font></mark>",
      "nodes to display detailed views at the cellular levels.",
      "External links to related content from <b>HSeT</b> are also provided 
      where appropriate (HSeT log-in required)",
      sep = " "
    ),
    
    paste(
      "PTH increases <b>bone resorption</b>, Ca <b>reabsorption</b>
      (in TAL and DCT/CNT ) and enhances vitamin D3 <b>conversion</b>
      into its active form. Furthermore, it represses <b>Pi reabsorption</b>
      in the proximal tubule.",
      sep = " "
    ),
    
    paste(
      "1,25 vitamin D3 synthesis is enhanced by <b>PTH</b>, which
      stimulates <b>Cyp27b1</b> and inhibits <b>Cyp24a1</b>. Active
      vitamin D3 increases <b>intestinal absorption</b> of Ca and Pi,
      <b>Ca reabsorption</b> in kidney (DCT/CNT) as well as <b>FGF23
      synthesis</b> in bone. Besides, it represses <b>PTH
      synthesis</b> in parathyroid glands.",
      sep = " "
    ),
    
    paste(
      "FGF23 synthesis is mainly enhanced by vitamin D3 and to a
      lesser extent by Pi. Its main role is to <b>prevent vitamin D3
      toxicity</b> by repressing its synthesis (activates Cyp24a1 and
      represses Cyp27b1, so antagonist of PTH). Additionally, it blunts
      <b>Pi reabsorption</b> in the proximal tubule. The relationship
      between PTH and FGF23 are still controversial, to date."
    ),
    
    paste(
      "Ca <b>represses PTH secretion</b> via the <b>calcium sensing
      receptor</b> (CaSR) in parathyroid chief cells. It also reduces
      the synthesis of the active form of vitamin D3. Finally, it
      activates the CaSR in the kidney, thereby <b>lowering the
      reabsorption of Ca</b> in the TAL."
    ),
    
    paste(
      "Since plasma Pi levels are decreased, the activation of PTH
      synthesis by Pi is reduced, as well as the repression of
      vitamin D3 conversion and the increase of FGF23 synthesis."
    ),
    
    paste(
      "Ionized plasma Ca concentration is",
      "<mark><font color=\"#FF0000\"><b>",
      "widely increased",
      "</b></font></mark>",
      "during PHP1,
      mainly because of a higher intestinal absorption, bone resorption
      and reabsorption in kidney. Inversely, Pi plasma concentration is",
      "<mark><font color=\"#FF0000\"><b>",
      "significantly reduced",
      "</b></font></mark>",
      "as a consequence of PTH and FGF23 effects
      on its renal reabsorption. Ca and Pi are also known to regulate
      hormonal synthesis, for example via the CaSR in parathyroid glands.",
      sep = " "
    )
  ),
  
  hypopara = c(
    paste(
      "Welcome to the",
      "<mark><font color=\"#FF0000\"><b>",
      "hypoparathyroidism",
      "</b></font></mark>",
      "(PHP1) case study.",
      "Before going further, make sure that <b>regulations</b> are enabled,
      and that Ca and Pi homeostasis are selected.",
      "<hr>",
      "The <b>next</b> and <b>back</b> buttons will walk you through the 
      consequences of PTH deficiency and regulatory pathways involved.",
      "You can <b>hover</b> over",
      "<mark><font color=\"#FF0000\"><b>",
      "intestine,",
      "</b></font></mark>",
      "<mark><font color=\"#FF0000\"><b>",
      "kidney,",
      "</b></font></mark>",
      "<mark><font color=\"#FF0000\"><b>",
      "bone,",
      "</b></font></mark>",
      "and",
      "<mark><font color=\"#FF0000\"><b>",
      "PTHg",
      "</b></font></mark>",
      "nodes to display detailed views at the cellular levels.",
      "External links to related content from <b>HSeT</b> are also provided 
      where appropriate (HSeT log-in required)",
      sep = " "
    ),
    
    paste(
      "Bone <b>resorption</b>, Ca <b>reabsorption</b> (in TAL and
      DCT/CNT) and <b>vitamin D3 conversion</b> into its active
      form are <b>decreased</b>. Furthermore, the repression of
      <b>Pi reabsorption</b> in the proximal tubule is dampened."
    ),
    
    paste(
      "Less vitamin D3 is converted into 1,25 vitamin D3,
    resulting in a <b>decrease of intestinal absorption</b> of
    both Ca and Pi, a <b>decreased reabsorption</b> of Ca."
    ),
    
    paste(
      "FGF23 synthesis is <b>reduced</b> due to lower vitamin D3 levels.
    Moreover, the elevation of Pi levels is <b>not enough to
    compensate</b> the loss of vitamin D3 (effect on FGF23 synthesis)."
    ),
    
    paste(
      "Because of the <b>decreased levels of plasma Ca</b>, PTH secretion is
    <b>less repressed</b>. Besides, more Ca is reabsorbed in the kidney
    since CaSR is <b>less activated</b>. The inhibitory effect of Ca on
    D3 synthesis is also <b>blunted</b>."
    ),
    
    paste(
      "Since plasma Pi levels are <b>increased</b>, the
    activation of PTH synthesis by Pi is <b>increased</b>,
    as well as the repression of vitamin D3 conversion and
    the increase of FGF23 synthesis."
    ),
    
    paste(
      "Ionized plasma Ca concentration is",
      "<mark><font color=\"#FF0000\"><b>",
      "widely decreased",
      "</b></font></mark>",
      "since intestinal
    absorption, resorption and reabsorption in kidney are
    <b>substantially reduced</b>. Inversely, Pi plasma
    concentration is",
      "<mark><font color=\"#FF0000\"><b>",
      "significantly enhanced",
      "</b></font></mark>",
      "as a consequence of PTH and FGF23 effects on its renal
    reabsorption, which are lower. Besides, the Pi rise is
    cannot compensate the loss of parathyroid function.",
      sep = " "
    )
  ),
  
  hypoD3 = c(
    paste(
      "Welcome to the",
      "<mark><font color=\"#FF0000\"><b>",
      "vitamin D3 deficiency",
      "</b></font></mark>",
      "(PHP1) case study.",
      "Before going further, make sure that <b>regulations</b> are enabled,
      and that Ca and Pi homeostasis are selected.",
      "<hr>",
      "The <b>next</b> and <b>back</b> buttons will walk you through the 
      consequences of vitamin D3 deficiency and regulatory pathways involved.",
      "You can <b>hover</b> over",
      "<mark><font color=\"#FF0000\"><b>",
      "intestine,",
      "</b></font></mark>",
      "<mark><font color=\"#FF0000\"><b>",
      "kidney,",
      "</b></font></mark>",
      "<mark><font color=\"#FF0000\"><b>",
      "bone,",
      "</b></font></mark>",
      "and",
      "<mark><font color=\"#FF0000\"><b>",
      "PTHg",
      "</b></font></mark>",
      "nodes to display detailed views at the cellular levels.",
      "External links to related content from <b>HSeT</b> are also provided 
      where appropriate (HSeT log-in required)",
      sep = " "
    ),
    
    paste(
      "1,25 vitamin D3 effects on PTH synthesis,
      FGF23 synthesis and kidney are <b>reduced</b>."
    ),
    
    paste(
      "Plasma PTH concentration <b>increases</b> because of the
      blunted repression by 1,25 vitamin D3, as well as the
      <b>reduction</b> of ionized plasma Ca concentration
      via the calcium sensing receptor. Thus, resorption is
      <b>increased</b> in order to compensate the <b>reduced intestinal
      absorption</b> of both Ca and Pi. Similarly, the elevation of
      PTH levels aims at <b>increasing Ca reabsorption</b> as well
      as decreasing Pi reabsorption in kidney. Besides, the increase
      of PTH also <b>slightly counteract</b> the decrease of
      vitamin D3 stocks."
    ),
    
    paste(
      "FGF23 synthesis is <b>reduced</b> as a result
      of vitamin D3 deficiency."
    ),
    
    paste(
      "The reduction of plasma Ca levels cause an <b>increase of
      PTH exocytosis</b> since CaSR is less activated. The decrease
      of Ca inhibitory effect on D3 synthesis, as well as elevated
      PTH levels are not enough significant to compensate for
      25(OH)D3 deficiency. Finally, CaSR is less activated in the
      kidney so as to <b>enhance Ca reabsorption</b>."
    ),
    
    paste(
      "Activation of PTH synthesis by Pi is <b>reduced</b>, as well
      as its effect on D3 synthesis. Besides, FGF23 synthesis is
      <b>less activated</b>"
    ),
    
    paste(
      "In summary, ionized plasma Ca concentration and Pi",
      "<mark><font color=\"#FF0000\"><b>",
      "remain quite stable",
      "</b></font></mark>",
      "as long as vitamin D3 stocks are not
      totally depleted. However, they start to",
      "<mark><font color=\"#FF0000\"><b>",
      "decrease",
      "</b></font></mark>",
      "as soon as the level of vitamin D3 is
      below a given",
      "<mark><font color=\"#FF0000\"><b>",
      "critical
      threshold.",
      "</b></font></mark>",
      "Consequently, all fluxes
      are significantly reduced."
    )
  )
)