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
  
  # change for Ca and PO4 fluxes
  Abs_int_change <- 0.5*( (out[nrow(out),"Abs_int_Ca"] - 9.829864e-04)/9.829864e-04*100 +  # numbers represent the base-case value
                            (out[nrow(out),"Abs_int_PO4"] - 8.233724e-04)/8.233724e-04*100 )
  U_Ca_change <- (out[nrow(out),"U_Ca"] - 3.907788e-05)/3.907788e-05*100
  U_PO4_change <- (out[nrow(out),"U_PO4"] - 3.969683e-04)/3.969683e-04*100
  Res_change <- 0.5*((out[nrow(out),"Res_Ca"] - 3.921871e-04)/3.921871e-04*100 + 
                          (out[nrow(out),"Res_PO4"] - 1.176561e-04)/1.176561e-04*100)
  Ac_Ca_change <- (out[nrow(out),"Ac_Ca"] - 1.009965e-03)/1.009965e-03*100
  Ac_PO4_change <- (out[nrow(out),"Ac_PO4"] - 2.178550e-04)/2.178550e-04*100
  Reabs_Ca_change <- (out[nrow(out),"Reabs_Ca"] - 2.592522e-03)/2.592522e-03*100
  Reabs_PO4_change <- (out[nrow(out),"Reabs_PO4"] - 4.606232e-03)/4.606232e-03*100
  Ca_pf_change <- (out[nrow(out),"Ca_pf"] - 5.306840e-03)/5.306840e-03*100
  PO4_pf_change <- (out[nrow(out),"PO4_pf"] - 1.995840e-01)/1.995840e-01*100
  Ca_fp_change <- (out[nrow(out),"Ca_fp"] - 4.296942e-03)/4.296942e-03*100
  PO4_fp_change <- (out[nrow(out),"PO4_fp"] - 1.993571e-01)/1.993571e-01*100
  PO4_pc_change <- (out[nrow(out),"PO4_pc"] - 2.772000e-03)/2.772000e-03*100
  PO4_cp_change <- (out[nrow(out),"PO4_cp"] - 2.771900e-03)/2.771900e-03*100
  
  # change for PTH fluxes
  
  PTHg_synth_change <- (out[nrow(out),"PTHg_synth"] - 54.02698)/54.02698*100 # numbers represent the base-case value
  PTHg_deg_change <- (out[nrow(out),"PTHg_deg"] - 45.086650)/45.086650*100
  PTHg_exo_change <- (out[nrow(out),"PTHg_exo"] - 8.936505)/8.936505*100
  PTHp_deg_change <- (out[nrow(out),"PTHp_deg"] - 8.931000)/8.931000*100
  
  df <- data.frame(Abs_int_change = Abs_int_change, 
                   U_Ca_change = U_Ca_change, 
                   U_PO4_change = U_PO4_change, 
                   Res_change = Res_change, 
                   Ac_Ca_change = Ac_Ca_change, 
                   Ac_PO4_change = Ac_PO4_change, 
                   Reabs_Ca_change = Reabs_Ca_change, 
                   Reabs_PO4_change = Reabs_PO4_change, 
                   Ca_pf_change = Ca_pf_change, 
                   PO4_pf_change = PO4_pf_change, 
                   Ca_fp_change = Ca_fp_change, 
                   PO4_fp_change = PO4_fp_change, 
                   PO4_pc_change = PO4_pc_change, 
                   PO4_cp_change = PO4_cp_change,
                   PTHg_synth_change = PTHg_synth_change,
                   PTHg_deg_change = PTHg_deg_change,
                   PTHg_exo_change = PTHg_exo_change,
                   PTHp_deg_change = PTHp_deg_change,
                   stringsAsFactors = F)
  
}

# Uncomment if need to set new base case values

# c(out()[1,"Abs_int_Ca"],
#   out()[1,"Abs_int_PO4"],
#   out()[1,"U_Ca"],
#   out()[1,"U_PO4"],
#   out()[1,"Res_Ca"],
#   out()[1,"Res_PO4"],
#   out()[1,"Ac_Ca"],
#   out()[1,"Ac_PO4"],
#   out()[1,"Reabs_Ca"],
#   out()[1,"Reabs_PO4"],
#   out()[1,"Ca_pf"],
#   out()[1,"PO4_pf"],
#   out()[1,"Ca_fp"],
#   out()[1,"PO4_fp"],
#   out()[1,"PO4_pc"],
#   out()[1,"PO4_cp"],
#   out()[1,"PTHg_synth"],
#   out()[1,"PTHg_deg"],
#   out()[1,"PTHg_exo"],
#   out()[1,"PTHp_deg"])