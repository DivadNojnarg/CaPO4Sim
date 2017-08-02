library(deSolve)
source("cap_fixed_norm_parameters.R") # source all fixed parameters that will not be changed by the user


calcium_phosphate_norm_core <- function (t, state, parameters){
  with(as.list(c(state, parameters)),{
    
    ##################
    #                #
    #  Normalization #
    #                #
    ##################
    
    tau <- 1/k_deg_PTHp
    D3_p_bar <- tau*D3_inact*delta_conv_max
    PTH_g_bar <- tau*k_prod_PTHg/(D3_p_bar*gamma_prod_D3)
    PTH_p_bar <- PTH_g_bar*Vc/Vp
    FGF_p_bar <- tau*k_prod_FGF
    Ca_p_bar <-  tau*I_Ca/Vp
    CaProt_p_bar <-  tau*k_f_CaProt*Ca_p_bar*N_Prot*Prot_tot_p
    PO4_p_bar <- tau*I_PO4/Vp
    NaPO4_p_bar <- tau*(a*k_f_NaHPO4+b*k_f_NaH2PO4)*Na*PO4_p_bar
    CaHPO4_p_bar <- tau*k_f_CaHPO4*a*f2^2*Ca_p_bar*PO4_p_bar
    CaH2PO4_p_bar <- tau*k_f_CaH2PO4*a*f2*f1*Ca_p_bar*PO4_p_bar
    CPP_p_bar <- tau*k_fet*CaHPO4_p_bar
    Ca_b_bar <- tau*Lambda_res_min
    PO4_b_bar <- 0.3*tau*Lambda_res_min
    PO4_c_bar <- tau*k_pc*PO4_p_bar*Vp
    PO4_f_bar <- tau*k_pf_PO4*PO4_p_bar*Vp
    Ca_f_bar <- tau*k_pf_Ca*Ca_p_bar*Vp
    CaHPO4_f_bar <- tau*k_f_CaHPO4*f2^2*Ca_f_bar*PO4_f_bar
    CaH2PO4_f_bar <- tau*k_f_CaH2PO4*f2*f1*Ca_f_bar*PO4_f_bar
    
    Xi_Ca <- tau/(Ca_p_bar*Vp)
    Xi_PO4 <- tau/(PO4_p_bar*Vp)
    
    # normalized parameters for PTH
    
    tilde_K_PTH_D3 <- 1/(gamma_prod_D3*D3_p_bar)
    tilde_K_prod_PTH_P <- K_prod_PTH_P/PO4_p_bar
    alpha_deg_PTHg <- tau*k_deg_PTHg
    Beta_exo_PTHg <- tau*beta_exo_PTHg
    Gamma_exo_PTHg <- tau*gamma_exo_PTHg
    tilde_K_Ca <- K_Ca/Ca_p_bar
    tilde_R <- R/Ca_p_bar
    
    # normalized parameters for D3
    
    k_conv <- k_conv_min/delta_conv_max
    tilde_gamma_ca_conv <- gamma_ca_conv*Ca_p_bar
    tilde_gamma_D3_conv <- gamma_D3_conv*D3_p_bar
    tilde_gamma_PO4_conv <- gamma_PO4_conv*PO4_p_bar
    tilde_gamma_FGF_conv <- gamma_FGF_conv*FGF_p_bar
    tilde_K_conv <- K_conv/PTH_p_bar
    nu <- tau*k_deg_D3
    tilde_gamma_deg_PTH <- gamma_deg_PTH*PTH_p_bar
    tilde_gamma_deg_FGF <- gamma_deg_FGF*FGF_p_bar
    
    # normalized parameters for FGF
    
    alpha_deg_FGFp <- tau*k_deg_FGF
    tilde_K_prod_D3 <- K_prod_D3/D3_p_bar
    tilde_K_prod_PO4 <- K_prod_PO4/PO4_p_bar
    
    # Intestinal absorption
    
    tilde_K_abs_D3 <- K_abs_D3/D3_p_bar
    
    # Bone resorption 
    
    tilde_K_res_PTH <- K_res_PTH/PTH_p_bar
    tilde_K_res_D3 <- K_res_D3/D3_p_bar
    
    # Kidney Calcium
    
    PTH_ref_norm <- PTH_p_bar/PTH_ref
    Ca_ref_norm <- Ca_p_bar/Ca_ref
    tilde_K_TAL_PTH <- K_TAL_PTH/PTH_p_bar
    tilde_K_DCT_PTH <- K_DCT_PTH/PTH_p_bar
    tilde_K_DCT_D3 <- K_DCT_D3/D3_p_bar
    
    # Kidney Phosphate
    
    tilde_K_PT_PTH <- K_PT_PTH/PTH_p_bar
    tilde_K_PT_FGF <- K_PT_FGF/FGF_p_bar
    tilde_K_PT_PO4 <- K_PT_PO4/PO4_p_bar
    
    ##################
    #                #
    #   Equations    #
    #                #
    ##################
    
    # PTHg #
    
    PTHg_synthesis_norm = PO4_p_norm^n_prod_Pho/((tilde_K_PTH_D3 + D3_p_norm)*(tilde_K_prod_PTH_P^n_prod_Pho + PO4_p_norm^n_prod_Pho))
    PTHg_degradation_norm = alpha_deg_PTHg*PTH_g_norm
    n_Ca_norm = n1_exo/(1+exp(-rho_exo*(tilde_R-Ca_p_norm)))+n2_exo
    F_Ca_norm = Beta_exo_PTHg - Gamma_exo_PTHg*Ca_p_norm^n_Ca_norm/(Ca_p_norm^n_Ca_norm + tilde_K_Ca^n_Ca_norm)
    PTHg_exocytosis_norm = F_Ca_norm*PTH_g_norm
  
    # PTHp #
    
    PTHp_influx_norm = PTHg_exocytosis_norm*PTH_g_norm
    PTHp_degradation_norm = PTH_p_norm
    
    # D3 #
    
    D3_basal_synthesis_norm = k_conv
    D3_conv_PTH_norm = 1/(1 + (tilde_K_conv/PTH_p_norm)^n_conv)
    D3_conv_Ca_norm = 1/(1 + tilde_gamma_ca_conv*Ca_p_norm)
    D3_conv_D3_norm = 1/(1 + tilde_gamma_D3_conv*D3_p_norm)
    D3_conv_P_norm = 1/(1 + tilde_gamma_PO4_conv*PO4_p_norm)
    D3_conv_FGF_norm = 1/(1 + tilde_gamma_FGF_conv*FGF_p_norm)
    D3_synthesis_norm = D3_basal_synthesis_norm + D3_conv_PTH_norm*D3_conv_Ca_norm*D3_conv_D3_norm*D3_conv_P_norm*D3_conv_FGF_norm
    
    D3_degradation_norm = (1 + tilde_gamma_deg_FGF*FGF_p_norm)/(1 + tilde_gamma_deg_PTH*PTH_p_norm)*nu*D3_p_norm
    
    # FGF23 #
    
    FGF_D3_activ_norm = delta_max_prod_D3*D3_p_norm^n_prod_FGF/(D3_p_norm^n_prod_FGF + tilde_K_prod_D3^n_prod_FGF)
    FGF_P_activ_norm = PO4_p_norm/(PO4_p_norm+ tilde_K_prod_PO4)
    
    FGF_synthesis_norm = 1 + FGF_D3_activ_norm*FGF_P_activ_norm
    
    FGF_degradation_norm = alpha_deg_FGFp*FGF_p_norm
    
    # Ca Abs_intest #
    
    Abs_intest_basal_norm = 0.25
    Abs_intest_D3_norm =(0.45*D3_p_norm^n_abs)/(D3_p_norm^n_abs + tilde_K_abs_D3^n_abs)
    
    Abs_intest_norm = Abs_intest_basal_norm + Abs_intest_D3_norm
    
    # P Abs_intest #
    
    Abs_intest_basal_P_norm = 0.4
    Abs_intest_D3_P_norm =(0.3*D3_p_norm^n_abs)/(D3_p_norm^n_abs + tilde_K_abs_D3^n_abs)
    
    Abs_intest_P_norm = Abs_intest_basal_P_norm + Abs_intest_D3_P_norm
    
    # Ca Fast bone #
    
    Caf_flux_norm = Ca_p_norm - tau*k_fp_Ca*Ca_f_norm
    Accretion_norm = tau*Lambda_ac_Ca*Ca_f_norm
    
    # P Fast Bone #
    
    Pf_flux_norm = PO4_p_norm - tau*k_fp_PO4*PO4_f_norm
    Accretion_P_norm = tau*Lambda_ac_P*PO4_f_norm
    
    # Ca Slow bone #
    
    Resorption_basal = 1
    Resorption_PTH_norm = delta_res_max/Lambda_res_min*0.2*PTH_p_norm^n_res/(PTH_p_norm^n_res + tilde_K_res_PTH^n_res)
    Resorption_D3_norm = delta_res_max/Lambda_res_min*0.8*D3_p_norm^n_res/(D3_p_norm^n_res + tilde_K_res_D3^n_res)
    
    Resorption_norm = (Resorption_basal + Resorption_PTH_norm + Resorption_D3_norm)
    
    # P Slow Bone #
    
    Resorption_P_norm = 0.6*Ca_P_stoech*Resorption_norm
    
    # Ca Kidney #
    
    Reabs_PT_basal = lambda_reabs_PT_0
    Reabs_PT_PTH_norm = delta_PT_max/(1 + (PTH_ref_norm*PTH_p_norm)^n_PT)
    Reabs_PT = Reabs_PT_basal + Reabs_PT_PTH_norm
    
    Reabs_TAL_basal = lambda_TAL_0
    Reabs_TAL_CaSR_norm = delta_CaSR_max/(1 + (Ca_ref_norm*Ca_p_norm)^n_TAL)
    Reabs_TAL_PTH_norm = delta_PTH_max*PTH_p_norm/(PTH_p_norm + tilde_K_TAL_PTH)
    
    Reabs_DCT_basal = lambda_DCT_0
    Reabs_DCT_PTH_norm = (delta_DCT_max*0.8*PTH_p_norm)/(PTH_p_norm + tilde_K_DCT_PTH)
    Reabs_DCT_D3_norm = (delta_DCT_max*0.2*D3_p_norm)/(D3_p_norm + tilde_K_DCT_D3)
    
    Excretion_norm = Xi_Ca*(1-(Reabs_PT + Reabs_TAL_basal + Reabs_TAL_CaSR_norm + Reabs_TAL_PTH_norm + Reabs_DCT_basal + 
                           Reabs_DCT_PTH_norm + Reabs_DCT_D3_norm))*GFR*(Ca_p_bar*Ca_p_norm + CaHPO4_p_bar*CaHPO4_p_norm + CaH2PO4_p_bar*CaHPO4_p_norm)
    
    # P Kidney #
    
    Reabs_PT_basal_P = lambda_PT_0
    Reabs_PT_PTH_P_norm = (delta_PTH_max_P*tilde_K_PT_PTH^n_reabs_P)/(PTH_p_norm^n_reabs_P + tilde_K_PT_PTH^n_reabs_P)
    Reabs_PT_FGF_P_norm = (delta_FGF_max*tilde_K_PT_FGF^n_reabs_P)/(FGF_p_norm^n_reabs_P + tilde_K_PT_FGF^n_reabs_P)
    Reabs_PT_P_norm = (delta_P_max*tilde_K_PT_PO4^n_reabs_P)/(PO4_p_norm^n_reabs_P + tilde_K_PT_PO4^n_reabs_P)
    Reabs_DCT_basal_P = lambda_DCT_P
    
    Excretion_P_norm = Xi_PO4*(1-(Reabs_PT_basal_P + Reabs_PT_PTH_P_norm + Reabs_PT_FGF_P_norm + Reabs_PT_P_norm +
                             Reabs_DCT_basal_P))*GFR*(PO4_p_bar*PO4_p_norm + CaHPO4_p_bar*CaHPO4_p_norm + 
                                                        CaH2PO4_p_bar*CaH2PO4_p_norm + NaPO4_p_bar*NaPO4_p_norm)
    
    # Intracellular P #
    
    Plasma_intra_Flux_norm = PO4_p_norm
    Intra_plasma_Flux_norm = tau*k_cp*PO4_c_norm;
    
    # Ca/P from HPO42- and H2PO4+  in plasma #
    
    k_form_CaHPO4_norm = Ca_p_norm*PO4_p_norm
    k_diss_CaHPO4_norm = tau*k_d_CaHPO4*CaHPO4_p_norm
    k_form_CaH2PO4_norm = Ca_p_norm*PO4_p_norm
    k_diss_CaH2PO4_norm = tau*k_d_CaH2PO4*CaH2PO4_p_norm*f1
    
    # Ca/P from HPO42- and H2PO4+  in the fast bone pool #
    
    k_form_CaHPO4f_norm = Ca_p_norm*PO4_p_norm
    k_diss_CaHPO4f_norm = tau*k_d_CaHPO4*CaHPO4_f_norm
    k_form_CaH2PO4f_norm = Ca_p_norm*PO4_p_norm
    k_diss_CaH2PO4f_norm = tau*k_d_CaH2PO4*CaH2PO4_f_norm*f1
    
    # Fetuin-A complexation with CaHPO4 and CaH2PO4 in plasma #
    
    k_fet_CaHPO4_norm = CaHPO4_p_norm
    k_fet_CaH2PO4_norm = CaH2PO4_p_norm*f1/CaHPO4_p_norm
    
    # CPP degradation 
    
    CPP_degradation = tau*k_c_CPP*CPP_p_norm
    
    # CaProt formation
    
    k_form_CaProt = Ca_p_norm*(1 - tau*k_f_CaProt*Ca_p_bar*CaProt_p_norm)
    k_diss_CaProt = tau*k_d_CaProt*CaProt_p_norm
    
    # Na and phosphate reaction in plasma
    
    k_form_NaPho = PO4_p_norm
    k_diss_NaPho = tau*(c*k_d_NaHPO4 + d*k_d_NaH2PO4)*NaPO4_p_norm
    
    ##################
    #                #
    #    Rate of     #
    #    Change      #
    ##################
    
    dPTH_g_norm = PTHg_synthesis_norm - PTHg_degradation_norm - PTHg_exocytosis_norm # PTHg           
    dPTH_p_norm = k_inject_PTH + PTHp_influx_norm - PTHp_degradation_norm # PTHp
    dD3_p_norm = D3_synthesis_norm - D3_degradation_norm # D3
    dFGF_p_norm = k_inject_FGF + FGF_synthesis_norm - FGF_degradation_norm # FGF23
    dCa_p_norm = k_inject_Ca + Abs_intest_norm  + Resorption_norm/I_Ca - Caf_flux_norm/I_Ca - Excretion_norm/I_Ca - k_form_CaProt + k_diss_CaProt - 
                 k_form_CaHPO4_norm + k_diss_CaHPO4_norm - k_form_CaH2PO4_norm + k_diss_CaH2PO4_norm   # Plasma Ca
                    #- EGTA_form + EGTA_diss/Ca_p_norm;
    dCa_f_norm = Caf_flux_norm - Accretion_norm - k_form_CaHPO4f_norm - k_form_CaH2PO4f_norm # Rapid Bone Ca
    dCa_b_norm = Accretion_norm - Resorption_norm; # Slow Bone Ca
    dPO4_p_norm = k_inject_P + Abs_intest_P_norm + Resorption_P_norm/I_PO4 - Pf_flux_norm/I_PO4 - Excretion_P_norm/I_PO4 -
                     Plasma_intra_Flux_norm/I_PO4 + Intra_plasma_Flux_norm/I_PO4 - k_form_CaHPO4_norm  + k_diss_CaHPO4_norm -
                     k_form_CaH2PO4_norm + k_diss_CaH2PO4_norm - k_form_NaPho + k_diss_NaPho # plasma P
    dPO4_f_norm =  Pf_flux_norm - Accretion_P_norm - k_form_CaHPO4f_norm - k_form_CaH2PO4f_norm # Rapid Bone P
    dPO4_b_norm = Accretion_P_norm - Resorption_P_norm # Slow bone P
    dPO4_c_norm = Plasma_intra_Flux_norm - Intra_plasma_Flux_norm # Intracellular P
    dCaHPO4_p_norm = k_form_CaHPO4_norm - k_diss_CaHPO4_norm - k_fet_CaHPO4_norm # CaHPO4p
    dCaH2PO4_p_norm = k_form_CaH2PO4_norm - k_diss_CaHPO4_norm - k_fet_CaH2PO4_norm # Ca(H2PO4)2p
    dCPP_p_norm = k_fet_CaHPO4_norm + k_fet_CaH2PO4_norm - CPP_degradation  # CPP particles
    dCaHPO4_f_norm = k_form_CaHPO4f_norm  -  k_diss_CaHPO4f_norm # CaHPO4 fast pool
    dCaH2PO4_f_norm = k_form_CaH2PO4f_norm - k_diss_CaH2PO4f_norm  # CaH2PO4 fast pool
    dCaProt_p_norm = k_form_CaProt - k_diss_CaProt # CaProt plasma
    dNaPO4_p_norm = k_form_NaPho - k_diss_NaPho # NaPO4 plasma
    dCa_tot_norm = dCa_p_norm + 1*dCaHPO4_p_norm + 1*dCaH2PO4_p_norm + dCaProt_p_norm + 1*dCPP_p_norm # calcium total
    dPO4_tot_norm = dPO4_p_norm + 1*dCaHPO4_p_norm + 1*dCaH2PO4_p_norm + dNaPO4_p_norm + 1*dCPP_p_norm # phosphate total
    
    list(c(dPTH_g_norm, dPTH_p_norm, dD3_p_norm, dFGF_p_norm, dCa_p_norm, dCa_f_norm, dCa_b_norm, dPO4_p_norm, dPO4_f_norm, dPO4_b_norm, dPO4_c_norm, 
           dCaHPO4_p_norm, dCaH2PO4_p_norm, dCPP_p_norm, dCaHPO4_f_norm, dCaH2PO4_f_norm, dCaProt_p_norm, dNaPO4_p_norm, dCa_tot_norm, dPO4_tot_norm))
    
  })
}