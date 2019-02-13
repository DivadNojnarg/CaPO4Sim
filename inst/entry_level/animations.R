# *------------------------------------------------------------------
# | PROGRAM NAME: animations.R
# | DATE: 29/03/2018
# | CREATED BY:  David Granjon
# *----------------------------------------------------------------
# | PURPOSE:  This UI code contains vectors required by extract_animation()
# |           and arrow_lighting() from model_utils.R
# *-----------------------------------------------------------------
# | UPDATES: 29/03/2018 (last update)
# |
# |
# *------------------------------------------------------------------


# table for php1 animations
animation_php1 <- list(
  PTH = c(19:21),
  D3 = c(22:27),
  FGF23 = c(28, 29),
  Ca = c(13:15),
  PO4 = c(16:18),
  Fluxes = c(9, 5, 12, 3, 1, 4, 6, 8, 10, 2, 11)
)

# table for hypopara animation (same as php1)
animation_hypopara <- animation_php1

# table fir hypoD3
animation_hypoD3 <- list(
  PTH = c(22:27),
  D3 = c(19:21),
  FGF23 = c(28, 29),
  Ca = c(13:15),
  PO4 = c(16:18),
  Fluxes = c(9, 5, 12, 3, 1, 4, 6, 8, 10, 2, 11)
)
