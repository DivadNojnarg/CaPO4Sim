## Test environments
* local OS X install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.4.4
* RHub check: Windows server, Ubuntu 16.04 and Fedora 
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


## Re-submission
I fixed the description file (calcium and phosphate and not Calcium and Phosphate).
I cannot add examples in most of the R files since they correspond to tightly connected shiny modules that do not work alone, so not relevant in this case. run_CaPO4Sim is the only "real" function with a working example that calls all the previous functions together. See here for more details: https://shiny.rstudio.com/articles/communicate-bet-modules.html
I fixed replace the beginning of the description by "Explore calcium (Ca) and phosphate (Pi) homeostasis with two novel 'Shiny' apps, 
    building upon on a previously published mathematical model written in C, 
    to ensure efficient computations".
