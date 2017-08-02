#-------------------------------------------------------------------------
#  This code contains the function that calculates the percentage of 
#  change of each flux, which is then needed to change the color of arrows
#  depending on the resulting variation. Green for increase and red
#  for decrease.
#
#  David Granjon, the Interface Group, Zurich
#  July 10th, 2017
#-------------------------------------------------------------------------


calc_change <- function (out){
  
  Abs_int_change <- 0.5*( (out[nrow(out),"Abs_int_Ca"] - out[1,"Abs_int_Ca"])/out[1,"Abs_int_Ca"]*100 +
                            (out[nrow(out),"Abs_int_PO4"] - out[1,"Abs_int_PO4"])/out[1,"Abs_int_PO4"]*100 )
  U_Ca_change <- (out[nrow(out),"U_Ca"] - out[1,"U_Ca"])/out[1,"U_Ca"]*100
  U_PO4_change <- (out[nrow(out),"U_PO4"] - out[1,"U_PO4"])/out[1,"U_PO4"]*100
  Res_change <- 0.5*((out[nrow(out),"Res_Ca"] - out[1,"Res_Ca"])/out[1,"Res_Ca"]*100 + 
                          (out[nrow(out),"Res_PO4"] - out[1,"Res_PO4"])/out[1,"Res_PO4"]*100)
  Ac_Ca_change <- (out[nrow(out),"Ac_Ca"] - out[1,"Ac_Ca"])/out[1,"Ac_Ca"]*100
  Ac_PO4_change <- (out[nrow(out),"Ac_PO4"] - out[1,"Ac_PO4"])/out[1,"Ac_PO4"]*100
  Reabs_Ca_change <- (out[nrow(out),"Reabs_Ca"] - out[1,"Reabs_Ca"])/out[1,"Reabs_Ca"]*100
  Reabs_PO4_change <- (out[nrow(out),"Reabs_PO4"] - out[1,"Reabs_PO4"])/out[1,"Reabs_PO4"]*100
  Ca_pf_change <- (out[nrow(out),"Ca_pf"] - out[1,"Ca_pf"])/out[1,"Ca_pf"]*100
  PO4_pf_change <- (out[nrow(out),"PO4_pf"] - out[1,"PO4_pf"])/out[1,"PO4_pf"]*100
  Ca_fp_change <- (out[nrow(out),"Ca_fp"] - out[1,"Ca_fp"])/out[1,"Ca_fp"]*100
  PO4_fp_change <- (out[nrow(out),"PO4_fp"] - out[1,"PO4_fp"])/out[1,"PO4_fp"]*100
  PO4_pc_change <- (out[nrow(out),"PO4_pc"] - out[1,"PO4_pc"])/out[1,"PO4_pc"]*100
  PO4_cp_change <- (out[nrow(out),"PO4_cp"] - out[1,"PO4_cp"])/out[1,"PO4_cp"]*100
  
  df <- data.frame(Abs_int_change = Abs_int_change, U_Ca_change = U_Ca_change, U_PO4_change = U_PO4_change, Res_change = Res_change, 
                   Ac_Ca_change = Ac_Ca_change, Ac_PO4_change = Ac_PO4_change, Reabs_Ca_change = Reabs_Ca_change, 
                   Reabs_PO4_change = Reabs_PO4_change, Ca_pf_change = Ca_pf_change, PO4_pf_change = PO4_pf_change, Ca_fp_change = Ca_fp_change, 
                   PO4_fp_change = PO4_fp_change, PO4_pc_change = PO4_pc_change, PO4_cp_change = PO4_cp_change, stringsAsFactors = F)
  
}