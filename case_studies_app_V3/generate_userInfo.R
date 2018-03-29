# *------------------------------------------------------------------
# | PROGRAM NAME: generate_userInfo.R
# | DATE: 29/03/2018 
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains the userInfo header card
# |   
# *-----------------------------------------------------------------
# | DATA USED:  images from /images_patient_info/ folder
# |
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)          
# |
# |
# *------------------------------------------------------------------


generate_userInfo <- function(input) {
  
  head_user <- dashboardUser(
    name = "Rat State",
    image = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
      generate_userFields(input)$image
    } else {
      "/images_patient_info/happy.png"
    },
    description = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
      generate_userFields(input)$description
    } else {
      "healthy"
    },
    sub_description = if (input$run_php1) {
      "Rat has primary-hyperparathyroidism"
    } else if (input$run_hypopara) {
      "Rat suffers from hypoparathyroidism"
    } else if (input$run_hypoD3) {
      "Rat has vitamin D3 defficiency"
    } else {
      "nothing to declare!"
    }, 
    stat1 = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
      generate_userFields(input)$stat1
    } else {
      HTML(paste(withMathJax(p("$$[Ca^{2+}]_p$$ 1.2 mM")), "<br/>", "(1.1-1.3 mM)"))
    },
    stat2 = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
      generate_userFields(input)$stat2
    } else {
      HTML(paste(withMathJax(p("$$[P_i]_p$$ 3 mM")), "<br/>", "(2.2-3.5 mM)"))
    },
    stat3 = if (input$run_php1 | input$run_hypopara | input$run_hypoD3) {
      generate_userFields(input)$stat3
    } else {
      HTML(paste(withMathJax(p("$$[PTH]_p$$ 66 ng/l")), "<br/>", "(20-70 ng/l)"))
    },
    stat4 = NULL
  )
  return(list(head_user = head_user))
}