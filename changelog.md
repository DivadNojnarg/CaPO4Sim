## Changelog version 3.1 (case studies app)
+ interface
  - major update of the design: all options are now in the right sidebar
  - left sidebar is dedicated to menu
  - modals can now be hidden (via a switch)
  - userMenu in the header which displays the physiological state of the organism
    (sick, healthy, ...). It reacts to case studies... (sometimes does not want to
    open, need to fix it)
+ CAPO4 network
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
  - the demo section is disabled as long as there is no content inside.
  - change the screenshot of the right sidebar in the notifications (will need to
    improve its quality)
  - add a glossary with all abbreviations + link to HSet website
  - syntax improvements of server.R, and other codes


## Changelog version 3 (global app)
- major update of the design: new dashboard display
- major update on the parameter display: now they are accessible
by clicking on nodes (which contain parameters). The controbar on the right side
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


## Changelog version 2.5 (global app)
- add human background
- adapt node display to the human background
- relabel PTH, D3, FGF23, PO4 and Ca nodes to concentrations
- fix the PTH blurred image problem
- add 4 parathyroid glands for human
- add an about section in the navbar
- rewrite some parts of the compiled C core (variable definitions)


## Changelog version 2.3 (case studies app)
- adapt node display to the human background
- relabel PTH, D3, FGF23, PO4 and Ca nodes to concentrations
- fix the PTH blurred image problem
- add 4 parathyroid glands for human
- delete biological data from plots
- rename sliders for case studies
- add an about section in the navbar
- reorganize the navbar, new sections: basis and case studies
- basis section will contain futur animations (UZH design department)
- add a new method to read files stored in www folder


## Changelog version 2.4 (global app)

- bone zoom image was updated to correct arrow position.
- node size was significantly increased, especially for regulations.
- a detailed view of PO4 reabsorption and its regulation by PTH and FGF23
in the proximal tubule is now available.
- the shinycssloader (spinner when application is busy) has been updated.
- tmax is now secured: if it has a bad value (missing, negative,...), then an
error is displayed and the parameter is reset. Moreover, an orange text indicates
which value to choose for tmax (shinyFeedback)
- the core has been replaced by a C code which runs 60 times faster than
the previous version. 

## Changelog version 2.2 (case studies app)

- the graph part was improved for steady-state simulations: 
orange vertical bars are displayed on each of the
4 subplots. They can be controled via a slider which corresponds to the intensity
of the disease. It was already done for dynamic perturbations.
- notifications are now displayed next to the main graph. Moreover, they can
be dragged anywhere.
- node size was also substancially increased.