# *------------------------------------------------------------------
# | PROGRAM NAME: network_modals.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This code provide all modals needed to generate the
# |           detailed zooms for  each case study
# *-----------------------------------------------------------------
# | UPDATES: 29/05/2018 (last update)
# |
# |
# *------------------------------------------------------------------

# Modal for baseline intestinal mechanisms
modal_zoom_intestine <- modalDialog(
  title = tagList(
    "Detailed Ca intestinal absorption",
    HTML(
      paste0(
        '<button type="button" class="btn btn-default pull-right" data-dismiss="modal">
         <i class="fa fa-close"></i>
         Dismiss
        </button>'
      )
    )
  ),
  HTML(
    paste0(
      '<a href=\"base_case_zoom/intestine/base_case_notif_intestine.svg\" target=\"_blank\">
       <img id = \"zoom_image\" width=\"220\" height=\"220\" border=\"0\"
       align=\"center\"  src=\"base_case_zoom/intestine/base_case_notif_intestine.svg\"/></a>
      '
    )
  ),
  size = "s",
  footer = NULL
)
