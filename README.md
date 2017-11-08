# WebApp_CaP_homeostasis
Repository of NCCR project containing shiny apps of Calcium Phosphate homeostasis

## Changelog version 2.4 (global app)

- bone zoom image was updated to correct arrow position
- node size was significantly increased, especially for regulations
- a detailed view of PO4 reabsorption and its regulation by PTH and FGF23
in the proximal tubule is now available
- the shinycssloader (spinner when application is busy) has been updated

## Changelog version 2.2 (case studies app)

- the graph part was improved for steady-state simulations: 
orange vertical bars are displayed on each of the
4 subplot. They can be controled via a slider which correspond to the intensity
of the disease. It was already done for dynamic perturbation
- Notifications are now displayed next to the main graph. Moreover, they can
be dragged anywhere
- node size was also substancially increased

# **To do** list

## General

- set a load balancer
- set authentication
- use a compiled model instead of only deSolve core
- write a about section for the staff
- update the main website
- refine video tutorials
- make animations on visnetwork (blinking,...)


## Global App

- improve the navigation between detailed zoom in the main app
- Find another place to put the active slider(s), maybe in the navbar
so that they are always visible
- table of modified parameters to remind the user what is changed or not
- the size of the box should scale each screen (and file the whole half page, 
which is not the case now)
- decrease navbar height
- improve help navigation
- adjust the human background

## Case studies App

- check plot scaling when screen size is reduced
- the size of the box should scale each screen (and file the whole half page, 
which is not the case now)
- adjust the human background
- slider cursor on 1 by default for each simulation (need to set a reset)
- find a way to better user graphs
- include detailed zoom?
- change title "steady-state" and "dynamics" simulations (too computational)
- use "explanations"" instead of "notifications"




