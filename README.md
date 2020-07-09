# CardioRenalSim, eLearning application for blood pressure regulation


These applications were developed in the [Interface Group](https://interfacegroup.ch).
![](https://lh5.googleusercontent.com/p/AF1QipMbcic6JUV3C8MuraR0BWq7hV-T-I4Y7QuAliz_=w160-h160-k-no)

## Installation

As this package is not currently on CRAN, install it via devtools:

```r
devtools::install_github("ddezel/CardioRenalSim")
```

## Access Apps

There are two ways to run these apps:

- access the [Apps.Physiol](http://physiol-seafile.uzh.ch) RStudio Connect server and selected the relevant app
- from the package (see below)

```r
library(CardioRenalSim)
# entry level app
run_CardioRenalSim(context = "introduction")
# virtual patient Bootstrap 4
run_CardioRenalSim(context = "virtual-patient")
```

Note that for the patient simulator, you need to install a compiler: either [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for windows or clang for [Mac/Linux](https://cran.r-project.org/bin/macosx/tools/). If you don't want,
you can still access these apps online.



## Demonstration

### Entry Level Application
Coming up soon

### Virtual Patient Simulator
Coming up soon

## **Contacts** 

*diane.dezelicourt_at_physiol.uzh.ch*

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

<div class="row">
<div class="col-sm-4">
<img src="man/figures/nccr.svg" height="200px" width="200px"/>
</div>
<div class="col-sm-4">
<img src="man/figures/unil.svg" height="200px" width="200px"/>
</div>
<div class="col-sm-4">
<img src="man/figures/uzh.svg" height="200px" width="200px"/>
</div>
</div>

