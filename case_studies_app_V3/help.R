help_text <- c(paste("This is the", "<mark><font color=\"#FF0000\"><b>", "navigation bar.", 
                     "</b></font></mark>", "You can control several options:", "<br>", "<ul>",
                     "<li>", icon("info-circle fa-2x"), "is the", "<font color=\"#0000FF\"><b>", 
                     "about", "</b></font>", "section containing informations
                     about the development staff.", "</li>", "<li>", 
                     icon("question-circle fa-2x"), "contains some", "<font color=\"#0000FF\"><b>", 
                     "help", "</b></font>", "text as well as a review of all 
                     features of this application you can trigger by pressing the 
                     help button.", "</li>", "<li>", icon("youtube-play fa-2x"), "contains", 
                     "<font color=\"#0000FF\"><b>", "video tutorial", "</b></font>", 
                     "showing how the application works.", "</li>", "<li>", icon("home fa-2x"), 
                     "is the", "<font color=\"#FF0000\"><b>", "main window", "</b></font>", 
                     "containing the application.", "</li>", "<li>", icon("map-o fa-2x"), 
                     "is dedicated to the", "<font color=\"#0000FF\"><b>", 
                     "educational content", "</b></font>", "of the app. Go here if you
                     want to select some case studies.", "</li>", "<li>", icon("sliders fa-2x"), 
                     "is where you handle the", "<font color=\"#FF0000\"><b>", 
                     "parameters", "</b></font>", "of this application 
                     such as changing the background, display regulation
                     mechanisms or not...", "</li>","</ul>", sep = " "),
               
               paste("This is the", "<mark><font color=\"#FF0000\"><b>", "central part", 
                     "</b></font></mark>", "of the app: the network.", "<br>",
                     "<ul>", "<li>","<img src=\"node_help.svg\">", "<br>", 
                     "<font color=\"#0000FF\"><b>", "Organs", "</b></font>", 
                     "are represented and displayed as they are in the body. 
                     Put the mouse above and you will see some extra informations.",
                     "</li>", "<li>", "<img src=\"arrow_help.svg\">", "<br>",
                     "Fluxes between all organs are depicted by black", 
                     "<font color=\"#0000FF\"><b>", "arrows", "</b></font>",
                     "which color may change as follows:",
                     "<br>", "<ul>", "<li>", "<img src=\"green_arrow_help.svg\">", 
                     "<br>", "if the flux is increased.", "</li>", 
                     "<li>", "<img src=\"red_arrow_help.svg\">", 
                     "<br>", "if the flux is decreased.", "</li>",
                     "</ul>", "By default, only calcium fluxes are displayed. This can be
                     modified in the", icon("sliders fa-2x"), "section of
                     the", "<mark><font color=\"#FF0000\"><b>", "navigation bar,", 
                     "</b></font></mark>", "by selecting calcium, phosphate or both.",
                     "</li>", "<li>","<img src=\"regulation_help.svg\">", "<br>", 
                     "<font color=\"#0000FF\"><b>", "Hormones", "</b></font>", 
                     "and regulator are represented by light green nodes. They
                     can be enabled in the", icon("sliders fa-2x"), "section of
                     the", "<mark><font color=\"#FF0000\"><b>", "navigation bar.", 
                     "</b></font></mark>",
                     "</li>", "<li>","<img src=\"dashed_arrow_help.svg\">", "<br>",
                     "<font color=\"#0000FF\"><b>", "Regulations", "</b></font>",
                     "are depicted by black dashed arrows", "<ul>", 
                     "<li>", "<img src=\"yellow_arrow_help.svg\">", 
                     "<br>", "They are highlighted in yellow, in response to a", 
                     "<font color=\"#0000FF\"><b>" ,"perturbation.", "</b></font>", 
                     "The size increases if it is a stimulation, 
                     an inversely.", "</li>", "</ul>", "Perturbations can be 
                     selected via the", icon("map-o fa-2x"), "section, in the", 
                     "<mark><font color=\"#FF0000\"><b>", "navigation bar.", 
                     "</b></font></mark>","</li>", "</ul>", sep = " "),
               
               paste("By clicking on next or back buttons, you can see animations
                     corresponding to the case studies available in the",
                     icon("map-o fa-2x"), "section of the", 
                     "<mark><font color=\"#FF0000\"><b>", "navigation bar.", 
                     "</b></font></mark>", "Be careful, clicking on Next/Back 
                     without having selected a case study will not trigger anything.",
                     sep = " "),
               
               paste("By moving this slider, you can control the current time 
                     of the simulation (orange vertical line in the graph). It only works
                     for", "<mark><font color=\"#FF0000\"><b>", "dynamic simulations", 
                     "</b></font></mark>", "and not steady-state ones. Recall that
                     dynamics simulations can be selected in the", icon("map-o fa-2x"), 
                     "section, in the", "<mark><font color=\"#FF0000\"><b>", 
                     "navigation bar.", "</b></font></mark>", "Besides, you 
                     can also use the back and next buttons to change the 
                     slider value.", sep = " "),
               
               paste("In this panel are displayed the graphs of each simulation. 
                      To see results, start by clicking on a case study in the",
                     icon("map-o fa-2x"), "section of the",
                      "<mark><font color=\"#FF0000\"><b>", "navigation bar.", 
                      "</b></font></mark>", sep = " "),
               
               paste("This slider allows the user to control the", 
                     "<font color=\"#0000FF\"><b>", "severity", "</b></font>", "of the
                     simulated disease, namely primary-hyperparathyroidism (PHP1), 
                     hypoparathyroidism and 25(OH) vitamin D3 deficiency.",
                     "Recall that diseases can be selected in the", icon("map-o fa-2x"), 
                     "section, in the", "<mark><font color=\"#FF0000\"><b>", 
                     "navigation bar.", "</b></font></mark>",
                     "For PHP1, when the slider is on 0, there is no disease.", "<br>",
                     "<img src=\"php1_min_help.svg\">", "<br>",
                     "Then, moving the slider to the right increases the severity 
                     of PHP1 (see below).", "<br>",
                     "<img src=\"php1_max_help.svg\">", "<br>",
                     "For hypoparathyroidism or 25(OH) vitamin D3 deficiency,
                     the slider starts on 1 (no disease) and 0 represents when
                     PTH synthesis is abolished and 25(OH) vitamin D3 stock is 0,
                     respectively. Below, an example for hypoparathyroidism.", "<br>",
                     "<img src=\"hypopara_min_help.svg\">", "<br>",
                     "<img src=\"hypopara_max_help.svg\">", sep = " ")
)
