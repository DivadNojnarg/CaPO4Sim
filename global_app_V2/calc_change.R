#-------------------------------------------------------------------------
#  This code contains the function that calculates the percentage of 
#  change of each flux, which is then needed to change the color of arrows
#  depending on the resulting variation. (see global.R for color change)
#
#  David Granjon, the Interface Group, Zurich
#  July 10th, 2017
#-------------------------------------------------------------------------


calc_change <- function(out, t_target) {
  
  # change for Ca and PO4 fluxes
  # numbers represent the base-case value
  # t target is the time at which to compute calc_change
  Abs_int_change <- 0.5*((out[t_target,"Abs_int_Ca"] - 9.829864e-04)/9.829864e-04*100 +  
                           (out[t_target,"Abs_int_PO4"] - 8.233724e-04)/8.233724e-04*100 )
  U_Ca_change <- (out[t_target,"U_Ca"] - 3.907788e-05)/3.907788e-05*100
  U_PO4_change <- (out[t_target,"U_PO4"] - 3.969683e-04)/3.969683e-04*100
  Res_change <- 0.5*((out[t_target,"Res_Ca"] - 3.921871e-04)/3.921871e-04*100 + 
                       (out[t_target,"Res_PO4"] - 1.176561e-04)/1.176561e-04*100)
  Ac_Ca_change <- (out[t_target,"Ac_Ca"] - 1.009965e-03)/1.009965e-03*100
  Ac_PO4_change <- (out[t_target,"Ac_PO4"] - 2.178550e-04)/2.178550e-04*100
  Reabs_Ca_change <- (out[t_target,"Reabs_Ca"] - 2.592522e-03)/2.592522e-03*100
  Reabs_PO4_change <- (out[t_target,"Reabs_PO4"] - 4.606232e-03)/4.606232e-03*100
  Net_Ca_pf_change <- ((out[t_target,"Ca_pf"] - out[t_target,"Ca_fp"]) -
                         (5.306840e-03 - 4.296942e-03))/(5.306840e-03 - 4.296942e-03)*100
  Net_PO4_pf_change <- (round((out[t_target,"PO4_pf"] - out[t_target,"PO4_fp"]) -
                                (1.995840e-01 - 1.993571e-01),4))/(1.995840e-01 - 1.993571e-01)*100
  # need to round since the order or magnitude of the difference is 1e-7
  Net_PO4_pc_change <- (round((out[t_target,"PO4_pc"] - out[t_target,"PO4_cp"]) -
                                (2.772000e-03 - 2.771900e-03),6))/(2.772000e-03 - 2.771900e-03)*100
  
  # change for PTH fluxes
  PTHg_synth_change <- (out[t_target,"PTHg_synth"] - 54.02698)/54.02698*100
  PTHg_synth_D3_change <- (out[t_target,"PTHg_synth_D3"] - 0.68025)/0.68025*100
  PTHg_synth_PO4_change <- (out[t_target,"PTHg_synth_PO4"] - 0.18945)/0.18945*100
  PTHg_exo_CaSR_change <- (out[t_target,"PTHg_exo_CaSR"] - 0.00693)/0.00693*100
  PTHg_deg_change <- (out[t_target,"PTHg_deg"] - 45.086650)/45.086650*100
  PTHg_exo_change <- (out[t_target,"PTHg_exo"] - 8.936505)/8.936505*100
  PTHp_deg_change <- (out[t_target,"PTHp_deg"] - 8.931000)/8.931000*100
  
  # Changes for PTH contribution in the proximal tubule
  Reabs_PT_change <- (out[t_target, "Reabs_PT_PTH"] - 0.0098)/0.0098*100
  
  # changes for PTH and CaSR contribution in TAL
  Reabs_TAL_CaSR_change <- (out[t_target, "Reabs_TAL_CaSR"] - 0.0104)/0.0104*100
  Reabs_TAL_PTH_change <- (out[t_target, "Reabs_TAL_PTH"] - 0.00465)/0.00465*100
  
  # changes for PTH and D3 contributions in DCT
  Reabs_DCT_PTH_change <- (out[t_target, "Reabs_DCT_PTH"] - 0.00417)/0.00417*100
  Reabs_DCT_D3_change <- (out[t_target, "Reabs_DCT_D3"] - 0.00108)/0.00108*100
  
  # change for intest Ca reabs due to D3
  Abs_int_D3_change <- (out[t_target, "Abs_int_D3"] - 0.000433)/0.000433*100
  
  # change for Ca resorption due to PTH and D3
  Res_PTH_change <- (out[t_target, "Res_PTH"] - 0.0000669)/0.0000669*100
  Res_D3_change <- (out[t_target, "Res_D3"] - 0.000225)/0.000225*100
  
  df <- data.frame(Abs_int_change = Abs_int_change, 
                   U_Ca_change = U_Ca_change, 
                   U_PO4_change = U_PO4_change, 
                   Res_change = Res_change, 
                   Ac_Ca_change = Ac_Ca_change, # 5
                   Ac_PO4_change = Ac_PO4_change, 
                   Reabs_Ca_change = Reabs_Ca_change, 
                   Reabs_PO4_change = Reabs_PO4_change, 
                   Net_Ca_pf_change = Net_Ca_pf_change, 
                   Net_PO4_pf_change = Net_PO4_pf_change, # 10
                   Net_PO4_pc_change = Net_PO4_pc_change, 
                   PTHg_synth_change = PTHg_synth_change,
                   PTHg_synth_D3_change = PTHg_synth_D3_change,
                   PTHg_synth_PO4_change = PTHg_synth_PO4_change,
                   PTHg_exo_CaSR_change = PTHg_exo_CaSR_change, # 15
                   PTHg_deg_change = PTHg_deg_change,
                   PTHg_exo_change = PTHg_exo_change,
                   PTHp_deg_change = PTHp_deg_change,
                   Reabs_PT_change = Reabs_PT_change,
                   Reabs_TAL_CaSR_change = Reabs_TAL_CaSR_change, # 20
                   Reabs_TAL_PTH_change = Reabs_TAL_PTH_change,
                   Reabs_DCT_PTH_change = Reabs_DCT_PTH_change,
                   Reabs_DCT_D3_change = Reabs_DCT_D3_change,
                   Abs_int_D3_change = Abs_int_D3_change,
                   Res_PTH_change = Res_PTH_change, # 25
                   Res_D3_change = Res_D3_change,
                   stringsAsFactors = FALSE)
  
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