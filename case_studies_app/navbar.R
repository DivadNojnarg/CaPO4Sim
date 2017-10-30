#-------------------------------------------------------------------------
#  This code contains the navbar menu. It is modified compared
#  to classic header. Indeed, some buttons to save, load, reset, download are
#  inserted in the header bar. Moreover, users can change the global theme
#  clicking on the theme selector.
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------

navbar <- introBox(
  
  smNavBar("testMenu", "", full.width = TRUE, fixed = FALSE, 
           # enable notifications or not since this can be boring
           actionBttn(inputId = "help", 
                      label = "Help?",
                      color = "danger", 
                      size = "lg",
                      style = "simple"),
           # notification for diagram and graph part
           switchInput(inputId = "notif2_switch", 
                       label = "Notifications?",
                       onStatus = "success", 
                       offStatus = "danger", 
                       value = TRUE, 
                       size = "mini"),
           # show background or not 
           awesomeCheckboxGroup(inputId = "background_choice", 
                                label = "Choose your background", 
                                choices = c("rat","human"), 
                                selected = "", 
                                inline = TRUE, 
                                status = "primary"),
           # select the network
           awesomeCheckboxGroup(inputId = "network_Ca_choice", 
                                label = "Choose your network", 
                                choices = c("Ca","PO4"), 
                                selected = "Ca", 
                                inline = TRUE, 
                                status = "primary"),
           # selector for hormonal regulation
           switchInput(inputId = "network_hormonal_choice", 
                       label = "Show regulations", 
                       value = FALSE,
                       onStatus = "success",
                       offStatus = "danger",
                       size = "mini"),
           smNavDropdown("Steady-state simulations",
                         
                         # Modal for primary hyperparathyroidism
                         modal_php1 <-
                           bs_modal(
                             id = "modal_php1",
                             title = "About Primary Hyperparathyroidism",
                             body = withMathJax("This is a simulation of primary hyperparathyroidism in the rat. The coefficient of PTH
                                                production will be increased several times its base case value (until 300 times). Then
                                                \\([Ca^{2+}]_p\\), \\([PO_4]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and \\([FGF]_p\\), as well as
                                                calcium and phosphate fluxes are calculated at equilibrium and normalized compared to their
                                                initial equilibrium value (namely when PTH production is normal). What is displayed are all
                                                these concentations and fluxes as a function of the normalized PTH synthesis rate (and not as a
                                                function of time). A rat model of primary hyperparathyroidism already exists in the rat and can
                                                be found here: https://www.ncbi.nlm.nih.gov/pubmed/3591940"),
                             size = "small"
                           ),

                         awesomeCheckbox("run_php1",
                                         "PHP1",
                                         value = FALSE,
                                         status = "primary",
                                         width = NULL) %>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_php1")),
                         
                         
                        
                         # awesomeCheckbox("run_php1", 
                         #                   "PHP1", 
                         #                   value = FALSE, 
                         #                   status = "primary", 
                         #                   width = NULL) %>%
                         #   shinyInput_label_embed(
                         #     shiny_iconlink() %>%
                         #       bs_embed_tooltip(
                         #         title = withMathJax("This is a simulation of primary hyperparathyroidism in the rat. The coefficient of PTH 
                         #                               production will be increased several times its base case value (until 300 times). Then 
                         #                               \\([Ca^{2+}]_p\\), \\([PO_4]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and \\([FGF]_p\\), as well as 
                         #                               calcium and phosphate fluxes are calculated at equilibrium and normalized compared to their 
                         #                               initial equilibrium value (namely when PTH production is normal). What is displayed are all 
                         #                               these concentations and fluxes as a function of the normalized PTH synthesis rate (and not as a 
                         #                               function of time). A model of primary hyperparathyroidism already exists in the rat and can 
                         #                               be found here: https://www.ncbi.nlm.nih.gov/pubmed/3591940"),
                         #         placement = "right"
                         #         
                         #         )
                         #   ),
                         
                         
                         smDivider(),
                         
                         # Modal for hypoparathyroidism
                         modal_hypopara <-
                           bs_modal(
                             id = "modal_hypopara",
                             title = "About hypoparathyroidism",
                             body = withMathJax("This is a simulation of hypoparathyroidism in the rat. The coefficient of PTH production will 
                                                be decreased several times its base case value (until 0 times). Then \\([Ca^{2+}]_p\\), 
                                                \\([PO_4]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and \\([FGF]_p\\), as well as calcium and 
                                                phosphate fluxes are calculated at equilibrium and normalized compared to their initial 
                                                equilibrium value (namely when PTH production is normal). What is displayed are all these 
                                                concentations and fluxes as a function of the normalized PTH synthesis rate (and not as a 
                                                function of time). On the x-axis, the value 1 represents the base-case, while 0.5 corresponds 
                                                to a division by a factor 2 of the PTH production rate and 0 represents a total inhition of 
                                                PTH synthesis."),
                             img(src = "bone.png"), # include image in modal
                             size = "medium"
                           ),
                         
                         awesomeCheckbox("run_hypopara", 
                                         "Hypoparathyroidism", 
                                         value = FALSE, 
                                         status = "primary", 
                                         width = NULL) %>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_hypopara")),
                         
                         smDivider(),
                         
                         # Modal for vitamin D3 deficiency
                         modal_hypoD3 <-
                           bs_modal(
                             id = "modal_hypoD3",
                             title = "About vitamin D3 deficiency",
                             body = withMathJax("This is a simulation of vitamin D\\(_3\\) deficiency in the rat. The concentration of 
                                                inactive vitamin D (25(OH)D) will be decreased several times its base case value (until 0 
                                                times). Then \\([Ca^{2+}]_p\\), \\([PO_4]_p\\), \\([PTH]_p\\), \\([D_3]_p\\) and 
                                                \\([FGF]_p\\), as well as calcium and phosphate fluxes are calculated at equilibrium and 
                                                normalized compared to their initial equilibrium value (namely when PTH production is normal). 
                                                What is displayed are all these concentations and fluxes as a function of the normalized 
                                                25(OH)D concentration (and not as a function of time). On the x-axis, the value 1 represents 
                                                the base-case, while 0.5 corresponds to a division by a factor 2 of the 25(OH)D concentration 
                                                and 0 represents the absence of 25(OH)D."),
                             size = "medium"
                           ),
                         
                         awesomeCheckbox("run_hypoD3", 
                                         "D3 deficiency", 
                                         value = FALSE, 
                                         status = "primary", 
                                         width = NULL) %>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_hypoD3"))),
           
           smNavDropdown("Dynamic simulations",
                         
                         # Modal for Ca iv injection
                         modal_Ca_inject <-
                           bs_modal(
                             id = "modal_Ca_inject",
                             title = "About Calcium infusion and EGTA infusion",
                             body = withMathJax("This is a simulation of induced hypercalcemia followed by acute hypocalcemia in 250g male 
                                                Wistar rats (https://www.ncbi.nlm.nih.gov/pubmed/24801007). Hypercalcemia is induced by 
                                                continuously injecting a solution containing calcium during 60 min, and followed by EGTA 
                                                infusion (30 mM, rate: 3mL/min) during 60 min so that ionized plasma calcium concentration is 
                                                decreased by 0.3 mM. On the y-axis are represented normalized \\([Ca^{2+}]_p\\) and 
                                                \\([PTH]_p\\) as a function of time. Experimental observations are shown as black dots"),
                             size = "medium"
                           ),
                         
                         awesomeCheckbox("run_Ca_inject", 
                                         "Ca IV injection", 
                                         value = FALSE, 
                                         status = "primary", 
                                         width = NULL) %>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_Ca_inject")),
                         
                         smDivider(),
                         
                         # Modal for PO4 iv injection
                         modal_PO4_inject <-
                           bs_modal(
                             id = "modal_PO4_inject",
                             title = "About PO4 infusion",
                             body = withMathJax("This is a simulation of intravenouse phosphate load in 250g-350g male wistar rats 
                                                (https://www.ncbi.nlm.nih.gov/pubmed/28246304). A solution containing 0.5 mmol \\(Na_2HPO_4\\) 
                                                and \\(NaH_2PO_4\\) was infused during 2-3 minutes to rats. On the y-axis are represented 
                                                \\([PO_4]_{tot}\\) (total plasma concentration of phosphate), normalized \\([Ca^{2+}]_p\\) and 
                                                \\([PTH]_p\\) as a function of time (250 minutes). Experimental observations are shown as black 
                                                dots."),
                             size = "medium"
                           ),
                         
                         awesomeCheckbox("run_PO4_inject", 
                                         "PO4 IV injection", 
                                         value = FALSE, 
                                         status = "primary",
                                         width = NULL) %>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_PO4_inject")),
                         
                         smDivider(),
                         
                         # Modal for PO4 iv injection
                         modal_PO4_gavage <-
                           bs_modal(
                             id = "modal_PO4_gavage",
                             title = "About PO4 gavage",
                             body = withMathJax("This is a simulation of phosphate gavage in 250g-350g male wistar rats 
                                                (https://www.ncbi.nlm.nih.gov/pubmed/28246304). A solution containing 0.5 mmol \\(Na_2HPO_4\\) 
                                                and \\(NaH_2PO_4\\) was used. On the y-axis are represented \\([PO_4]_{tot}\\) (total plasma 
                                                concentration of phosphate), normalized \\([Ca^{2+}]_p\\) and \\([PTH]_p\\) as a function of 
                                                time (250 minutes). Experimental observations are shown as black dots."),
                             size = "medium"
                           ),
                         
                         awesomeCheckbox("run_PO4_gav", 
                                         "PO4 gavage", 
                                         value = FALSE,
                                         status = "primary", 
                                         width = NULL) %>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_PO4_gavage")))
           
  ),
  
  data.step = 1,
  data.intro = help_text[1]
)