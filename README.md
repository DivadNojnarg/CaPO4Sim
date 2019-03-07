# CaPO4Sim, the virtual physiology simulator
[![Travis build status](https://travis-ci.com/DivadNojnarg/CaPO4Sim.svg?token=wk4LByQczpy1WzofyA7q&branch=master)](https://travis-ci.com/DivadNojnarg/CaPO4Sim)
[![CRAN status](https://www.r-pkg.org/badges/version/CaPO4Sim)](https://cran.r-project.org/package=CaPO4Sim)


> Repository of NCCR project containing shiny apps of Calcium Phosphate homeostasis

## Installation

As this package is not currently on CRAN, install it via devtools:

```r
devtools::install_github("DivadNojnarg/CaPO4Sim")
```

## Access Apps

There are two ways to run these apps:

- access the [Apps.Physiol](http://physiol-seafile.uzh.ch) RStudio Connect server and selected the relevant app
- from the package (see below)

```r
library(CaPO4Sim)
# entry level app
run_CaPO4Sim(context = "introduction")
# virtual patient Bootstrap 4
run_CaPO4Sim(context = "virtual-patient")
```

Note that for the patient simulator, you need to install a compiler: either [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for windows or clang for [Mac/Linux](https://cran.r-project.org/bin/macosx/tools/). If you don't want,
you can still access these apps online.

The Bootstrap 3 version will not be updated anymore but can be accessed [here](http://physiol-seafile.uzh.ch:3939/virtual_patient/).

## Demonstration

### Entry Level Application

![](https://community.rstudio.com/uploads/default/original/2X/b/bd5306bab9eb7561da943c59d2693639dc719168.gif)

### Virtual Patient Simulator

![](https://community.rstudio.com/uploads/default/original/2X/e/eb1013fd09ccf10cbe13da3f0168eebfcb0eba75.gif)

## **Contacts** 

*david.granjon_at_uzh.ch*

## **Staff** 
+ *Project manager*: [Dr. Diane de Zélicourt, Prof. Vartan Kurtcuoglu and Dr. David Granjon](http://interfacegroup.ch/people/)
+ *Experts in Physiology*: 
  - [Prof. François Verrey](https://www.physiol.uzh.ch/en/research/institutegroups/EpithelialTransports/TeamMembers/FVerrey.html),
  - [Prof. Olivier Bonny](https://www.unil.ch/dpt/fr/home/menuinst/recherche/groupe-bonny.html),
  - [Prof. Bernard Rossier](https://hset.org/organization/team/),
  - [Prof. Carsten Wagner](https://www.physiol.uzh.ch/en/research/institutegroups/Acidbasetransport/Grwagner/CWagner.html)
+ *Graphic designer*: [Tara von Grebel and Dr. David Granjon](https://www.uzh.ch/id/cl/iframe/org/index.php?id=tg)
+ *Animation*: [Janine Meyer](https://www.uzh.ch/id/cl/iframe/org/index.php?id=jnm)
+ *Model developer*: [Dr. David Granjon](https://divadnojnarg.github.io)
+ *Web/R developer*: Dr. David Granjon
+ *Server management*: Dr. David Granjon

## Acknowledgments
I thank RStudio for providing us with RStudio Connect licences. All illustration backgrounds were designed by Tara Von Grebel and animations were produced by Janine Meyer, both of the Multimedia and eLearning-Services, University of Zurich.

![](https://community.rstudio.com/uploads/default/original/2X/1/16568320e6b8079b21eae4fc61d26b7f03dcd56e.png) 
![](https://community.rstudio.com/uploads/default/original/2X/b/b663036fed740eb1543e1d04c7bcc74b0da821eb.png) 
![](https://community.rstudio.com/uploads/default/original/2X/3/3ad594aa86a0e5a1b5ef038b453eb8fdee4b48f2.png) 
