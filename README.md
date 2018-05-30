# WebApp_CaP_homeostasis
Repository of NCCR project containing shiny apps of Calcium Phosphate homeostasis

### **Staff** ###
+ *Project manager*: [Dr. Diane de Zélicourt and Prof. Vartan Kurtcuoglu](http://interfacegroup.ch/people/)
+ *Experts in Physiology*: 
  - [Prof. François Verrey](https://www.physiol.uzh.ch/en/research/institutegroups/EpithelialTransports/TeamMembers/FVerrey.html),
  - [Prof. Olivier Bonny](https://www.unil.ch/dpt/fr/home/menuinst/recherche/groupe-bonny.html),
  - [Prof. Bernard Rossier](https://hset.org/organization/team/),
  - [Prof. Carsten Wagner](https://www.physiol.uzh.ch/en/research/institutegroups/Acidbasetransport/Grwagner/CWagner.html)
+ *Graphic designer*: [Tara von Grebel](https://www.uzh.ch/id/cl/iframe/org/index.php?id=tg)
+ *Animation*: [Janine Meyer](https://www.uzh.ch/id/cl/iframe/org/index.php?id=jnm)
+ *Model developer*: [Dr. David Granjon](https://divadnojnarg.github.io)
+ *Web/R developer*: Dr. David Granjon
+ *Server management*: Dr. David Granjon
+ *Server expert*: [Dr. Sergio Maffioletti](https://www.id.uzh.ch/en/scienceit/about/team.html)
+ *Consultants*: [Dr. Nathalie Debard](https://hset.org/organization/team/)

### **Aknowledgments** ###

![Figure 1-1](images/about_us.jpg)


I am also very grateful to the [Shiny](http://shiny.rstudio.com) developers 
and [RStudio](https://www.rstudio.com) team, who provide a lot of useful softwares
and packages. 

I finally thanks: Dean Attali ([shinyjs](https://deanattali.com/shinyjs/)), 
Karline Soetaert and Thomas Petzoldt ([deSolve](http://desolve.r-forge.r-project.org)),
Carson Sievert ([plotly](https://plot.ly/r/)), 
the datastorm team ([visNetwork](http://datastorm-open.github.io/visNetwork/)),
the dream R team, especially Victor Perrier ([shinyWidgets](https://dreamrs.github.io/shinyWidgets/index.html)),
Hadley Wickham and the Tidyverse team ([purrr and stringr](https://www.tidyverse.org/packages/)),
Eric Bailey ([shinyMenus and shinyBS](https://github.com/ebailey78)),
Andrew Sali ([shinycssloaders](https://github.com/andrewsali/shinycssloaders)),
Yang Tang ([shinyjqui](https://cran.r-project.org/web/packages/shinyjqui/vignettes/introduction.html)),
Ian Lyttle ([bsplus](http://ijlyttle.github.io/bsplus/)),
Kent Russell ([sweetalertR](http://timelyportfolio.github.io/buildingwidgets/week25/sweetalert_examples.html)),
Carl Ganz and Afshin Mehrabani ([rintrojs](https://carlganz.github.io/rintrojs/)),
Andy Merlino ([shinyFeedback](https://cran.r-project.org/web/packages/shinyFeedback/vignettes/shinyFeedback-intro.html)),
Guang Yang ([ygdashboard](https://github.com/gyang274/ygdashboard))


```R
library(shiny)
library(ygdashboard)
require(visNetwork)
library(shinyBS)
library(dplyr)
library(plotly)
library(deSolve)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjqui)
library(bsplus)
library(sweetalertR)
library(shinytoastr)
library(shinyMenus)
library(stringr)
library(purrr)
library(rintrojs)
library(shinyFeedback)
library(magrittr)
library(DT)
```

### **Contacts** ###

*david.granjon_at_uzh.ch*
(see the changelog file to access all versions)

## Changelog version 3.2 (case studies app)
+ Interface
  - add movies to the video section (Janine Meyer work)
  - fix the patient state issue in the header (and remove it from the help section)
  - disable human background (for the moment)
  - remove human background from options
  - rename « demo » to «  video »
+ CaPO4 Network
  - when a node is selected, its size is increased (and restored when deselected) 
  - same thing for edges
  - add a way to make edges blinking when a case study is selected
  - when a case study is selected, the corresponding node is blinking
    as long as the counter is equal to 1.
  - remove PTHp node in plasma and merge with PTHg (does not fit well in humans)
  - replace all png detailed pictures by svgs for all case studies
  - add captions to detailed pictures
  - fix the net flux issue between plasma/rapid bone pool and plasma/cells
  - add blinking to the next button in animations (when it was not clicked before)
  - nodes and edges only blink once in the CAPO4 interactive diagram (do not waste time)
  - set a progress bar for animations
  - add cellular views for baseline
  - remove Hset links in the CaPO4 interactive diagram
  - change the parathyroid gland svg file (include 3 PTH balls inside)
+ Graphs
  - change box size (plot area, height = 950px)
  - move slider at the bottom (plot area)
  - add a nice « getting started » section in the right panel, where the graph are displayed for case studies
  - change Trace elements in graphs (PTH, D3 and FGF23)
  - Add indications when case studies are selected (how to use the slider, …)
+ Educational Content
  - update all modals for php1, hypopara, hypoD3
  - update network notifications as well as graph notifications
  - disable dynamic case studies (will be released later)
  - trigger an alert when user finishes a case study (he can either restart or choose another case study)
  - set a close button in modals at the top (instead of bottom, better for small screens)
  - remove HSet links from the glossary
+ Patient/Rat Status box
  - change patient by rat 
  - change concentrations according to the rat
+ Others
  - remove `source("help.R")` from all other code, except global.R
  - some code syntax correction
  - indicates the user to enable regulations when he launches case studies
    if they are not already enabled (sweetAlert)
  - sort files in the www folder
  - reorder ui files
  - rewrite the dashboardControbar code in pure R
  - add hotjar tracking to recover datas of users
  - redo the whole comment section for each file
  - remove the first blue and yellow notifications when a case study is selected 
  - Update the help section (synchronization with the current interface)
+ Hardware
  - The server is going to switch from 8 cores/32 GB RAM to 16 cores and 64GB RAM to further support the load
  

## Changelog version 3.1 (global app)
+ Interface
  - correct a prettySwitch in the sidebar (slim = TRUE)
+ CaPO4 Network
  - when a node is selected, its size is increased (and restored when deselected) 
  - same thing for edges
+ Minor changes
  - fix a wrong file name for a node (rapid-bone png)
 
 

## Changelog version 3.1 (treatments app)
+ Interface
  - major update of the design: all options are now in the right sidebar
  - left sidebar is dedicated to menu
+ Patient file
  - add a new patient CV where it is possible to see its name, age, symptomes,
  history of treatments, ...
+ Disease Engine
  - complete rework of this section to match the patient file
+ Others
  - remove `source("help.R")` from all other code, except global.R
  - some code syntax correction
  
  ***

## Changelog version 3.1 (case studies app)
+ Interface
  - major update of the design: all options are now in the right sidebar
  - left sidebar is dedicated to menu
  - modals can now be hidden (via a switch)
  - userMenu in the header which displays the physiological state of the organism
    (sick, healthy, ...). It reacts to case studies... (sometimes does not want to
    open, need to fix it)
+ CaPO4 network
  - improve the navigation in the main network: can filter what to display or not
  - change node size/arrow width
  - the CaPO4 network scales automatically (use visEvent with beforeDrawing and scale = 0.6)
    which prevents it from being generated outside the window...
    when some components are added or removed
+ Graphs
  - replace PO4 by Pi everywhere (notifications, modals, graphs, graphs_notifications,
    checkboxes labels)
  - change Ca to Ca2+ and PO4 to Pi in the network (modified svg images)
  - change color code for Ca and PO4 fluxes (a color gradient instead of dashed,
    dot-dashed curves)
+ Others
  - update the help section with new elements (mainly in the right sidebar):
    still have a problem with the userMenu that is altered by the help, and 
    the z position is not optimized (sometimes hidden, sometimes masking the
    right sidebar,...)
  - the demo section is disabled as long as there is no content inside
  - change the screenshot of the right sidebar in the notifications (will need to
    improve its quality)
  - add a glossary with all abbreviations + link to HSet website
  - syntax improvements of server.R, and other codes


## Changelog version 3 (global app)
- major update of the design: new dashboard display
- major update on the parameter display: now they are accessible
by clicking on nodes (which contain parameters). The controlbar on the right side
plays a crucial role to display parameters
- add a notification in the header to remind the user which parameter is changed
and how (is it increased? decreased?)
- refine some help text written in the zoom area
- new about section (credits)


## Changelog version 3 (case studies app)
- major update of the design: new dashboard display
- major update of the help section
- major update of notifications during case studies
- major update of modal during case studies
- option to change the dashboard theme
- new about section (credits)

## Changelog version 3 (treatments app)
- major update of the design: new dashboard display
- major update on the parameter display: now they are accessible
- refine some help text written in the zoom area
- new about section (credits)

# **To do** list

## General

- set a load balancer (Ready to be done).
- <del>set authentication</del> (DONE).
- <del>use a compiled model instead of only deSolve core<del> (DONE).
- <del>write "about" section for the staff</del> (DONE).
- <del>update the main website</del> (DONE).
- <del>refine video tutorials</del> (DONE).
- <del>make animations on visnetwork (blinking,...)</del> (DONE).


## Global App

- improve the navigation between detailed zoom in the main app
- <del>find another place to put the active slider(s), maybe in the navbar
so that they are always visible</del> (DONE)
- <del>table of modified parameters to remind the user what is changed or not.</del> (DONE)
- the size of the box should scale each screen (and file the whole half page, 
which is not the case now)
- <del>decrease navbar height</del> (DONE)
- <del>improve help navigation</del> (DONE)
- <del>adjust the human background</del> (DONE)

## Case studies App

- check plot scaling when screen size is reduced
- the size of the box should scale each screen (and file the whole half page, 
which is not the case now)
- <del>adjust the human background</del> (DONE)
- <del>slider cursor on 1 by default for each simulation (need to set a reset)</del> (DONE)
- <del>find a way to better user graphs</del> (DONE)
- <del>include detailed zoom?</del> (DONE)
- <del>improve help navigation</del> (DONE)
- change title "steady-state" and "dynamics" simulations (too computational)
- use "explanations"" instead of "notifications"

## Treatments studies App
- update the core to compiled code
