# help text generation
# needed for the introjs help
# contains a vector of all instructions

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
                     "and regulators are represented by light green nodes. They
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
                     and inversely.", "</li>", "</ul>", "Perturbations can be 
                     selected via the", icon("map-o fa-2x"), "section, in the", 
                     "<mark><font color=\"#FF0000\"><b>", "navigation bar.", 
                     "</b></font></mark>","</li>", "</ul>", sep = " "),
               
               paste("In this panel are displayed the <b>concentrations</b> or 
                      <b>quantities</b> related to the selected compartment. 
                      To see the results, start by", "<mark><font color=\"#FF0000\"><b>", 
                      "clicking on a <b>node</b>", "</b></font></mark>","on the 
                      interactive network. The value of <b>tmax</b>, which is 
                      the maximum time of simulation, can be increased or decreased 
                      as required (but always higher than 0). 
                      Some graphs are <b>unavailable</b>, so nothing will appear 
                      by clicking on them (intestine, kidney and urine).", 
                     sep = " "),
               
               paste("In this panel are displayed the <b>fluxes</b> related 
                      to the selected arrow. To see the results, start by", 
                      "<mark><font color=\"#FF0000\"><b>", "clicking on an <b>edge</b>", 
                      "</b></font></mark>", "on the interactive 
                      diagram. Note that several edges <b>do not have</b> any graph associated.", 
                     sep = " "),
               
               paste("When a detailed view is not available, nothing is displayed.", 
                     "<mark><font color=\"#FF0000\"><b>", "You can select each edges 
                     of the detailed view in order to print the corresponding 
                     slider(s) on the right.", "</b></font></mark>", "If nothing 
                     happens, it basically mean that no parameter was associated
                     to this edge",
                     sep = " "),
               
               paste("<b>Sliders</b> can be moved from the right to the left and inversely. 
                      To", "<mark><font color=\"#FF0000\"><b>", "reset", 
                      "</b></font></mark>","the value of a slider, click on the 
                      <b>reset</b> button on the <b>right side</b> of the slider.", 
                     sep = " "),
               
               paste("Here you can change the global <b>theme</b> 
                     of the dashboard", sep = " ")
               
               # "Select edges specific to Ca or PO4 homeostasis.",
               # 
               # "Remove or allow the display of hormonal regulation in the graph",
               # 
               # "Enable/Disable the background",
               # 
               # "This corresponds to the maximum value of integration. You can increase
               # or decrease it but it has to be always positive.",
               # 
               # "By moving this slider, you can see the dynamics evolution of all
               # fluxes (denoted by red/green arrows) as a function of time, at any 
               # time after the begining of the simulation."
               
)

