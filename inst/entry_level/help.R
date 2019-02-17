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
    "</b></font></mark>", "You can control several options:", "<br>", "<ul>",
    "<li>", icon("info-circle fa-2x"), "is the", "<font color=\"#0000FF\"><b>",
    "about", "</b></font>", "section containing informations
        about the development staff.", "</li>", "<li>", icon("search fa-2x"),
    "is the", "<font color=\"#0000FF\"><b>",
    "glossary", "</b></font>", "section where you will find a definition",
    "of all the abbreviations in this app", "</li>",
    "<li>", icon("youtube-play fa-2x"), "contains",
    "<font color=\"#0000FF\"><b>", "video tutorial", "</b></font>",
    "showing how the application works.", "</li>", "<li>", icon("home fa-2x"),
    "is the", "<font color=\"#FF0000\"><b>", "main window", "</b></font>",
    "containing the application.", "</li>","</ul>", sep = " "),

  paste(
    "The network is the", "<mark><font color=\"#FF0000\"><b>", "central part",
    "</b></font></mark>", "of the app.", "<br>",
    "<ul>", "<li>", img(src = "rintrojs_help/arrow_help.svg", height = "50px", width = "50px"),
    "Fluxes between all organs are depicted by black",
    "<font color=\"#0000FF\"><b>", "arrows", "</b></font>",
    "which color may change as follows:",
    "<br>", "<ul>", "<li>", img(src = "rintrojs_help/green_arrow_help.svg", height = "50px", width = "50px"),
    "<br>", "if the flux is increased.", "</li>",
    "<li>", img(src = "rintrojs_help/red_arrow_help.svg", height = "50px", width = "50px"),
    "<br>", "if the flux is decreased.", "</li>",
    "</ul>", "<li>",
    "<font color=\"#0000FF\"><b>", "Hormones", "</b></font>",
    "and regulators are represented by light green nodes. They
    can be enabled in the", icon("sliders"), "section of
    the", "<mark><font color=\"#FF0000\"><b>", "right sidebar",
    "</b></font></mark>", icon("gears"), ".",
    "</li>", "<li>", img(src = "rintrojs_help/dashed_arrow_help.svg", height = "50px", width = "50px"),
    "<font color=\"#0000FF\"><b>", "Regulations", "</b></font>",
    "are depicted by black dashed arrows:", "<ul>",
    "<li>", img(src = "rintrojs_help/yellow_arrow_help.svg", height = "50px", width = "50px"),
    "They are highlighted in yellow, in response to a",
    "<font color=\"#0000FF\"><b>" ,"perturbation", "</b></font>",
    "(see", icon("map-o"), "section, in the",
    "<mark><font color=\"#FF0000\"><b>", "right sidebar",
    "</b></font></mark>", icon("gears"), ").",
    "The size increases if it is a stimulation,
    and inversely.", "</li>", "</ul>", "</li>", "</ul>", sep = " "
    ),

  paste(
    "In this panel are displayed the graphs of each simulation.
    To see results, start by clicking on a case study in the",
    icon("map-o fa-2x"), "section of the",
    "<mark><font color=\"#FF0000\"><b>", "right sidebar.",
    "</b></font></mark>", sep = " "
    ),

  paste(
    "This slider allows the user to control the",
    "<font color=\"#0000FF\"><b>", "severity", "</b></font>", "of the
    simulated disease, namely primary-hyperparathyroidism (PHP1),
    hypoparathyroidism and 25(OH) vitamin D3 deficiency.",
    "Recall that diseases can be selected in the", icon("map-o fa-2x"),
    "section, in the", "<mark><font color=\"#FF0000\"><b>",
    "right sidebar.", "</b></font></mark>",
    "For PHP1, when the slider is on 0, there is no disease.", "<br>",
    "Then, moving the slider to the right increases the severity
    of PHP1 (see below).", "<br>",
    "For hypoparathyroidism or 25(OH) vitamin D3 deficiency,
    the slider starts on 1 (no disease) and 0 represents when
    PTH synthesis is abolished and 25(OH) vitamin D3 stock is 0,
    respectively. Below, an example for hypoparathyroidism.", "<br>", sep = " "),

  paste(
    icon("question-circle fa-2x"), "contains some",
    "<font color=\"#0000FF\"><b>",
    "help", "</b></font>", "text as well as a review of all
    features of this application you can trigger by pressing the
    help button."),

  paste("Choose your <font color=\"#FF0000\"><b> background </b></font> (Rat by default)."),
  paste("<font color=\"#FF0000\"><b>Enable/disable</b></font> organs and/or
        hormonal regulations. By default, hormonal regulations are
        <font color=\"#FF0000\"><b>not activated</b></font>."),
  paste("<font color=\"#FF0000\"><b>Filter</b></font> what to display in the
        network (by default, nothing is enabled). Be careful that you
        <font color=\"#FF0000\"><b>activated/disactivated</b></font>
        hormones and/or organs before."),
  paste("Control the <font color=\"#FF0000\"><b>size</b></font> of nodes."),
  paste("Control the <font color=\"#FF0000\"><b>width</b></font> of edges.")

  # as the remaining of the help section is part of the controlbar,
  # need to update it manually in dashboardControlbar.R

)
