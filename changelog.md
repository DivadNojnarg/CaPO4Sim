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