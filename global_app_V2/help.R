# help text generation
# needed for the introjs help
# contains a vector of all instructions

help_text <- c("In this panel is displayed the interactive network (see legend). 
               Basically, when a parameter is changed, initial perturbations are shown 
               in yellow. The arrow size increases if it is a stimulatory effect and 
               inversely. Fluxes are shown in red if they decrease or in green if 
               they are enhanced. Colors correspond to the final state of the system
               (which is the value of tmax in minutes). Some nodes can be 
               double-clicked (parathyroid glands, intestine, bone as well as kidneys) 
               in order to display a detailed view of what happens inside these nodes.",
               
               "In this panel are displayed the graph of CaPO4 homeostasis. 
               To see the results, start by clicking on a node on the interactive network. 
               The value of tmax, which is the maximum time of simulation, can be 
               increased or decreased as required (but always higher than 0). 
               Some graphs may be unavailable (for instance in feces, food,...).",
               
               "To see the results, start by clicking on an edge on the interactive diagram.
               Note that several edges do not have any graph associated.",
               
               "When a detailed view is not available, nothing is displayed. You
               can select each edges of the detailed view in order to print the 
               corresponding slider(s) on the right.",
               
               "Sliders can be moved from the right to the left and inversely. 
               To reset the value of a slider, click on the reset button on the 
               right side of the slider.",
               
               "In this navbar, you can print help, choose your network,
               show regulations or not in the network, display the background and
               control a lot of other options",
               
               "Select edges specific to Ca or PO4 homeostasis.",
               
               "Remove or allow the display of hormonal regulation in the graph",
               
               "Enable/Disable the background",
               
               "This corresponds to the maximum value of integration. You can increase
               or decrease it but it has to be always positive.",
               
               "By moving this slider, you can see the dynamics evolution of all
               fluxes (denoted by red/green arrows) as a function of time, at any 
               time after the begining of the simulation."
               
)

