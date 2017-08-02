source("cap_fixed_norm_parameters.R")
source("calcium_phosphate_norm_core.R")

tau <- 1/parameters["k_deg_PTHp"]
D3_p_bar <- tau*parameters["D3_inact"]*parameters["delta_conv_max"]
PTH_g_bar <- tau*parameters["k_prod_PTHg"]/(D3_p_bar*parameters["gamma_prod_D3"])
PTH_p_bar <- PTH_g_bar*parameters["Vc"]/parameters["Vp"]
FGF_p_bar <- tau*parameters["k_prod_FGF"]
Ca_p_bar <-  tau*parameters["I_Ca"]/parameters["Vp"]
CaProt_p_bar <-  tau*parameters["k_f_CaProt"]*Ca_p_bar*parameters["N_Prot"]*parameters["Prot_tot_p"]
PO4_p_bar <- tau*parameters["I_PO4"]/parameters["Vp"]
NaPO4_p_bar <- tau*(parameters["a"]*parameters["k_f_NaHPO4"] + parameters["b"]*parameters["k_f_NaH2PO4"])*parameters["Na"]*PO4_p_bar
CaHPO4_p_bar <- tau*parameters["k_f_CaHPO4"]*parameters["a"]*parameters["f2"]^2*Ca_p_bar*PO4_p_bar
CaH2PO4_p_bar <- tau*parameters["k_f_CaH2PO4"]*parameters["a"]*parameters["f2"]*parameters["f1"]*Ca_p_bar*PO4_p_bar
CPP_p_bar <- tau*parameters["k_fet"]*CaHPO4_p_bar
Ca_b_bar <- tau*parameters["Lambda_res_min"]
PO4_b_bar <- 0.3*Ca_b_bar
PO4_c_bar <- tau*parameters["k_pc"]*PO4_p_bar*parameters["Vp"]
PO4_f_bar <- tau*parameters["k_pf_PO4"]*PO4_p_bar*parameters["Vp"]
Ca_f_bar <- tau*parameters["k_pf_Ca"]*Ca_p_bar*parameters["Vp"]
CaHPO4_f_bar <- tau*parameters["k_f_CaHPO4"]*parameters["f2"]^2*Ca_f_bar*PO4_f_bar
CaH2PO4_f_bar <- tau*parameters["k_f_CaH2PO4"]*parameters["f2"]*parameters["f1"]*Ca_f_bar*PO4_f_bar


times <- seq(0,1000, by = 1)
state <- c("PTH_g_norm" = 12.88238, "PTH_p_norm" = 0.0687, "D3_p_norm" = 564.2683, "FGF_p_norm" = 16.78, "Ca_p_norm" = 1.2061, 
           "Ca_f_norm" = 1.8363, "Ca_b_norm" = 100, "PO4_p_norm" = 1.4783, "PO4_f_norm" = 0.7922, "PO4_b_norm" = 90, "PO4_c_norm" = 2.7719, 
           "CaHPO4_p_norm" = 0.1059, "CaH2PO4_p_norm" = 0.0038, "CPP_p_norm" = 0.0109, "CaHPO4_f_norm" = 0.0864, "CaH2PO4_f_norm" = 0.0031, 
           "CaProt_p_norm" = 1.4518, "NaPO4_p_norm" = 0.9135, "Ca_tot_norm" = 2.4914, "PO4_tot_norm" = 2.8354) 

out <- as.data.frame(ode(y = state, times = times, 
                         func = calcium_phosphate_norm_core, parms = parameters))

par(mfrow=c(2,3))

plot(out[,1],out[,"Ca_p_norm"]/Ca_p_bar, type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"Ca_p_norm"]/Ca_p_bar*1.2)), col ="blue")
plot(out[,1],out[,"PO4_p_norm"]/PO4_p_bar, type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"PO4_p_norm"]/PO4_p_bar*1.2)), col = "red")
plot(out[,1],out[,"PTH_p_norm"]/PTH_p_bar, type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"PTH_p_norm"]/PTH_p_bar*1.2)))
plot(out[,1],out[,"FGF_p_norm"]/FGF_p_bar, type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"FGF_p_norm"]/FGF_p_bar*1.2)))
plot(out[,1],out[,"D3_p_norm"]/D3_p_bar, type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"D3_p_norm"]/D3_p_bar*1.2)))