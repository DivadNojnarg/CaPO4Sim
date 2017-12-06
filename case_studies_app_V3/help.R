help_text <- c(paste("This is the", "<mark><font color=\"#FF0000\"><b>", "navigation bar.", 
                     "</b></font></mark>", "You can control several options:", "<br>", "<ul>",
                     "<li>", icon("info-circle"), "is the", "<font color=\"#0000FF\"><b>", 
                     "about", "</b></font>", "section containing informations
                     about the development staff.", "</li>", "<li>", 
                     icon("question-circle"), "contains some", "<font color=\"#0000FF\"><b>", 
                     "help", "</b></font>", "text as well as a review of all 
                     features of this application you can trigger by pressing the 
                     help button.", "</li>", "<li>", icon("youtube-play"), "contains", 
                     "<font color=\"#0000FF\"><b>", "video tutorial", "</b></font>", 
                     "showing how the application works.", "</li>", "<li>", icon("home"), 
                     "is the", "<font color=\"#FF0000\"><b>", "main window", "</b></font>", 
                     "containing the application.", "</li>", "<li>", icon("map-o"), 
                     "is dedicated to the", "<font color=\"#0000FF\"><b>", 
                     "educational content", "</b></font>", "of the app. Go here if you
                     want to select some case studies.", "</li>", "<li>", icon("sliders"), 
                     "is where you handle the", "<font color=\"#FF0000\"><b>", 
                     "parameters", "</b></font>", "of this application 
                     such as changing the background, display regulation
                     mechanisms or not...", "</li>","</ul>", sep = " "),
               
               "This is the central part of the app: the network. 
               At start, only Ca arrow are displayed without any regulation. This 
               can be changed from the navigation bar above. It is also possible to
               select the model to use: either rat or human, with the corresponding
               background. Once a case study is selected initial perturbations are 
               shown in yellow. The arrow size increases if it is a stimulatory 
               effect and inversely. Fluxes are shown in red if they 
               decrease or in green if they are enhanced. Color corresponds to the final 
               state of the system (see graph on the right).",
               
               "By clicking on next or back buttons, you can see animations
               corresponding to the case studies available in the navigation bar.",
               
               "By moving this slider, you can control the current time 
               of the simulation (orange vertical line in the graph). It only works
               for dynamic simulations and not steady-state ones. Besides, you 
               can also use the back and next buttons of the diagram section, 
               which will move the slider.",
               
               "In this panel are displayed the graphs of each simulation. 
               To see results, start by clicking on a case study in the navigation bar,
               (navigation bar above)."
)