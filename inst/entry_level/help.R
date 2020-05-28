# *------------------------------------------------------------------
# | PROGRAM NAME: help.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This code provide datas to generate the help section
# |           required by rintrojs
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)
# |
# |
# *------------------------------------------------------------------


help_text <- c(
  paste(
    "This is the", "<mark><font color=\"#FF0000\"><b>", "navigation bar.",
    "</b></font></mark>", "You can control several options:", "<br>",
    "<ul>",
    "<li>", icon("info-circle fa-2x"), "is the", "<font color=\"#0000FF\"><b>",
    "about", "</b></font>", "section containing informations
        about the development staff.", "</li>",
    "<li>", icon("search fa-2x"), "is the", "<font color=\"#0000FF\"><b>",
    "glossary", "</b></font>", "section where you will find a definition",
    "of all the abbreviations in this app", "</li>",
    "<li>", icon("youtube-play fa-2x"), "contains",
    "<font color=\"#0000FF\"><b>", "videos", "</b></font>",
    "explaining key concepts related to calcium-phosphate homeostasis.", "</li>",
    "<li>", icon("home fa-2x"), "is the", "<font color=\"#FF0000\"><b>", "main window", "</b></font>",
    "containing the application.", "</li>",
    "</ul>", sep = " "),

  paste(
    "The network is the", "<mark><font color=\"#FF0000\"><b>", "central part",
    "</b></font></mark>", "of the app.",
    "<br>",
    "<ul>",
    "<li>",img(src = "rintrojs_help/arrow_help.svg", height = "20px", width = "40px"),
    "<font color=\"#0000FF\"><b>","Ca and PO4 fluxes", "</b></font>",
    "<li>",img(src = "rintrojs_help/regulation_help.svg", height = "40px", width = "40px"),
    "<font color=\"#0000FF\"><b>", "Regulatory hormones and ions.", "</b></font>",
    "<li>",img(src = "rintrojs_help/dashed_arrow_help.svg", height = "20px", width = "40px"),
    "<font color=\"#0000FF\"><b>", "Regulatory effects","<br>","(+ promotor, - inhibitor)", "</b></font>",
    "</ul>",

    "<br>",
    "<b>To explore individual regulatory pathways</b>",
    "<ul>",
    "<li>","Go to Display Options", icon("sliders"),
    "in the", "<mark><font color=\"#FF0000\"><b>", "right sidebar",
    "</b></font></mark>", icon("gears"),"</li>",
    "<li>", "Select hormones and ions to hide/show","</li>",
    "</ul>",

    "<br>",
    "<b>In the case studies</b> (", icon("map-o"), "in the",
    "<mark><font color=\"#FF0000\"><b>", "right sidebar",
    "</b></font></mark>", icon("gears"), "), animations illustrate",
    "the sequence of perturbations associated with the selected pathology:",
    "<br>","<br>","<ul>",
    "<li>",img(src = "rintrojs_help/dashed_arrow_help.svg", height = "20px", width = "40px"),
    "<b>","Regulations become","</b>",
    "<ul>",
      "<li>", "<b>","thicker","</b>","if stronger", "</li>",
      "<li>", "<b>","thinner","</b>","if weaker", "</li>",
      "<li>",img(src = "rintrojs_help/yellow_arrow_help.svg", height = "20px", width = "30px"),
      "<b>","yellow","</b>","when being discussed",
    "</ul>", "</li>","<br>",
    "<li>", img(src = "rintrojs_help/arrow_help.svg", height = "20px", width = "40px"),
    "<b>","Ca/PO4 fluxes become","</b>",
    "<ul>",
      "<li>", img(src = "rintrojs_help/green_arrow_help.svg", height = "20px", width = "30px"),
      "<b>","green","</b>","if increased", "</li>",
      "<li>", img(src = "rintrojs_help/red_arrow_help.svg", height = "20px", width = "30px"),
      "<b>","red","</b>","if decreased", "</li>",
    "</ul>","</li>",
    "</ul>",
    sep = " "
    ),

  paste(
    "<b>When a case study</b> (",icon("map-o"), "in the",
    "<mark><font color=\"#FF0000\"><b>", "right sidebar",
    "</b></font></mark>",") <b>is selected</b>,",
    "this panel displays changes in plasma concentrations and fluxes as a function of disease severity.",
    "<br>","<br>",
    "The x-axis shows the severity of the selected disease.",
    "<br>","<br>",
    "The curves do not represent a dynamic evolution in time:",
    "<b>Concentrations and fluxes are given at steady-state</b>, i.e. after reaching a new, stable equilibrium.",
    "<br>","<br>",
    "<b>Concentrations and fluxes are normalized</b> by their physiological healthy value.",
    "<ul>",
    "<li>","Values > 1 : higher than normal.","</li>",
    "<li>","Values < 1 : lower than normal.","</li>",
    "</ul>","<br>",
    "Results were obtained using the model of",
    "<a href=\"https://pubmed.ncbi.nlm.nih.gov/28747359/\"> Ca and PO4 homeostasis</a> developed by Granjon et al.",
    sep = " "
    ),

  paste(
    "Use this slider to shift the orange mark on the plots and
    read concentrations and fluxes at the corresponding <font color=\"#0000FF\"><b>disease severity</b></font>.",
    "<br>","<br>",
    "<ul>",
    "<li>", "<font color=\"#0000FF\"><b>PHP1</b></font>: PTH synthesis <b>increase</b>",
    "<ul>",
    "<li>","= 0 fold : healthy.", "</li>",
    "<li>","> 0 fold : increasing severity.","</li>",
    "</ul>","</li>","<br>",
    "<li>", "<font color=\"#0000FF\"><b>Hypoparathyroidism or 25(OH) vitamin D3 deficiency</b></font>: PTH synthesis or vitD3 <b>decrease</b>",
    "<ul>",
    "<li>","= 0 fold : healthy.", "</li>",
    "<li>","> 0 fold : increasing deficiency.", "</li>",
    "<li>","= 1 fold : maximum severity. PTH synthesis or VitD3 stocks are fully abolished.","</li>",
    "</ul>","</li>","</ul>",
    "<br>",
    "Mouse over the curves for exact numerical values.",
    sep = " "),

  paste(
    icon("question-circle"),
    "<font color=\"#0000FF\"><b>Help section</b></font>"),

  paste("Choose the <font color=\"#FF0000\"><b>background image</b></font> (Rat by default)."),

  paste("<font color=\"#FF0000\"><b>Enable/disable the display</b></font> of organs and
        hormonal regulations. By default, both are activated."),

  paste("<font color=\"#FF0000\"><b>Select pathways</b></font> to show/hide",
        "(by default, all regulatory pathways are enabled).",
        "<br><br>",
        "Display <font color=\"#FF0000\"><b>also depends</b></font> on whether regulations and organs
        are enabled/disabled."),

  paste("Control the <font color=\"#FF0000\"><b>size</b></font> of the network icons."),

  paste("Control the <font color=\"#FF0000\"><b>width</b></font> of the network arrows.")

  # as the remaining of the help section is part of the controlbar,
  # need to update it manually in dashboardControlbar.R

)
