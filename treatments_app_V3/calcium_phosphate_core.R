#-------------------------------------------------------------------------
#
#  This is the model core containing all equations and fluxes,
#  it is translated from a previous Matlab code
#
#  David Granjon, the Interface Group, Zurich
#  June 12th, 2017
#-------------------------------------------------------------------------

calcium_phosphate_core <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    
    ##################
    #                #
    #  Lag Setting   #
    #                #
    ##################
    
    #tau <- 240
    #PTH_p_lag <- ifelse((t - tau) <= 0, 0.0683/Vp, lagvalue(t - tau))
    
    ##################
    #                #
    #   Simulations  #
    ##################
    if (!is.null(event)) {
      event_table <- event$table
      event <- event_table$event
      t_start <- event_table$start_time
      t_stop <- event_table$stop_time
      if (!is.null(event)) {
        if (event == "Ca_inject") {
          if (t > t_start && t < t_stop) {
            k_inject_Ca <- Ca_inject
          }
        }
      }
    }
    
    # if (!is.null(t_start) & !is.null(t_stop)) {
    #   if (!is.null(Ca_inject)) {
    #     if (t > t_start && t < t_stop) {
    #       k_inject_Ca <- Ca_inject
    #     }
    #   } else if (!is.null(Ca_food)) {
    #     I_Ca <- Ca_food
    #   } else if (!is.null(D3_inject)) {
    #     if (t > t_start && t < t_stop) {
    #       k_inject_D3 <- D3_inject * Vp
    #     }
    #   } else if (!is.null(P_inject)) {
    #     if (t > t_start && t < t_stop) {
    #       k_inject_P <- P_inject
    #     }
    #   } else if (!is.null(P_food)) {
    #     I_P <- P_food
    #   }
    # }
    
    ##################
    #                #
    #   Equations    #
    #                #
    ##################
    
    # Conversion of PTH in concentration
    
    PTH_p <- PTH_p/Vp # be carefull when Vp changes
    
    # PTHg #
    
    PTHg_basal_synthesis_norm <- k_prod_PTHg * Vc / PTH_g_norm
    PTHg_synthesis_D3_norm <- 1 / (1 + gamma_prod_D3 * D3_norm * D3_p)
    PTHg_synthesis_PO4_norm <- PO4_p^n_prod_Pho /
      ((K_prod_PTH_P / Pho_p_norm)^n_prod_Pho + PO4_p^n_prod_Pho)
    
    PTHg_synthesis_norm <- PTX_coeff * PTHg_basal_synthesis_norm * 
                           PTHg_synthesis_D3_norm * PTHg_synthesis_PO4_norm
    
    PTHg_degradation_norm <- k_deg_PTHg * PTH_g
    n_Ca_norm <- n1_exo / (1 + exp(-rho_exo * Ca_p_norm * (R / Ca_p_norm - Ca_p))) + n2_exo
    F_Ca_norm <- beta_exo_PTHg - gamma_exo_PTHg * Ca_p^n_Ca_norm /
      (Ca_p^n_Ca_norm + (K_Ca / Ca_p_norm)^n_Ca_norm)
    PTHg_exocytosis_norm <- F_Ca_norm * PTH_g
    
    # PTHp #
    
    PTHp_influx_norm <- PTHg_exocytosis_norm * PTH_g_norm / PTH_p_norm
    PTHp_degradation_norm <- k_deg_PTHp * PTH_p
    
    # D3 #
    
    D3_basal_synthesis_norm <- k_conv_min * D3_inact / D3_norm
    # choose PTH_p or PTH_p_lag[1]
    D3_conv_PTH_norm <- (delta_conv_max * (D3_inact / D3_norm) * PTH_p^n_conv) /
      (PTH_p^n_conv + (K_conv / PTH_p_norm)^n_conv) 
    D3_conv_Ca_norm <- 1 / (1 + gamma_ca_conv * Ca_p_norm * Ca_p)
    D3_conv_D3_norm <- 1 / (1 + gamma_D3_conv * D3_norm * D3_p)
    D3_conv_P_norm <- 1 / (1 + gamma_P_conv * Pho_p_norm * PO4_p)
    D3_conv_FGF_norm <- 1/(1 + gamma_FGF_conv * FGF_p_norm * FGF_p)
    D3_synthesis_norm <- D3_basal_synthesis_norm + D3_conv_PTH_norm * D3_conv_Ca_norm *
      D3_conv_D3_norm*D3_conv_P_norm*D3_conv_FGF_norm
    
    D3_degradation_norm <- (k_deg_D3 * (1 + gamma_deg_FGF * FGF_p_norm * FGF_p) * D3_p) /
      (1 + gamma_deg_PTH * PTH_p_norm * PTH_p)  
    
    # FGF23 #
    
    FGF_basal_synthesis_norm <- k_prod_FGF / FGF_p_norm
    FGF_D3_activ_norm <- delta_max_prod_D3 * D3_p^n_prod_FGF /
      (D3_p^n_prod_FGF + (K_prod_D3 / D3_norm)^n_prod_FGF)
    FGF_P_activ_norm <- PO4_p / (PO4_p + K_prod_P / Pho_p_norm)
    
    FGF_synthesis_norm <- FGF_basal_synthesis_norm * 
      (1 + FGF_D3_activ_norm * FGF_P_activ_norm)
    
    FGF_degradation_norm <- k_deg_FGF * FGF_p
    
    # Ca Abs_intest #
    
    Abs_intest_basal_norm <- 0.25 * I_Ca
    Abs_intest_D3_norm <- (0.45 * I_Ca * D3_p^n_abs) / 
      (D3_p^n_abs + (K_abs_D3 / D3_norm)^n_abs)
    
    Abs_intest_norm <- (Abs_intest_basal_norm + Abs_intest_D3_norm) / Ca_p_norm
    
    # P Abs_intest #
    
    Abs_intest_basal_P_norm <- 0.4 * I_P
    Abs_intest_D3_P_norm <- (0.3 * I_P * D3_p^n_abs) / 
      (D3_p^n_abs + (K_abs_D3 / D3_norm)^n_abs)
    
    Abs_intest_P_norm <- (Abs_intest_basal_P_norm + Abs_intest_D3_P_norm) / 
      Pho_p_norm
    
    # Ca Fast bone #
    
    Rapid_storage_Ca <- k_p_Ca * Ca_p * Vp  
    Rapid_release_Ca <- k_f_Ca * Ca_f
    Accretion_norm <- Lambda_ac_Ca * Ca_f
    
    # P Fast Bone #
    
    Rapid_storage_P <- k_p_P * PO4_p * Vp 
    Rapid_release_P <- k_f_P * PO4_f
    Accretion_P_norm <- Lambda_ac_P * PO4_f
    
    # Ca Slow bone #
    
    Resorption_basal <- Lambda_res_min
    Resorption_PTH_norm <- (delta_res_max * 0.2 * PTH_p^n_res) /
      (PTH_p^n_res + (K_res_PTH / PTH_p_norm)^n_res)
    Resorption_D3_norm <- (delta_res_max * 0.8 * D3_p^n_res) /
      (D3_p^n_res + (K_res_D3 / D3_norm)^n_res)
    
    Resorption_norm <- Resorption_basal + Resorption_PTH_norm + Resorption_D3_norm
    
    # P Slow Bone #
    
    Resorption_P_norm <- 0.3 * Resorption_norm 
    
    # Ca Kidney #
    
    Reabs_PT_basal <- lambda_reabs_PT_0
    Reabs_PT_PTH_norm <- delta_PT_max / (1 + (PTH_p*PTH_p_norm / PTH_ref)^n_PT)
    Reabs_PT <- Reabs_PT_basal + Reabs_PT_PTH_norm
    
    Reabs_TAL_basal <- lambda_TAL_0
    Reabs_TAL_CaSR_norm <- delta_CaSR_max / (1 + (Ca_p * Ca_p_norm / Ca_ref)^n_TAL)
    Reabs_TAL_PTH_norm <- delta_PTH_max * PTH_p / (PTH_p + K_TAL_PTH / PTH_p_norm)
    
    Reabs_DCT_basal <- lambda_DCT_0
    Reabs_DCT_PTH_norm <- (delta_DCT_max * 0.8 * PTH_p) / 
      (PTH_p + K_DCT_PTH / PTH_p_norm)
    Reabs_DCT_D3_norm <- (delta_DCT_max * 0.2 * D3_p) / 
      (D3_p + K_DCT_D3 / D3_norm)
    
    Excretion_norm <- (1 - (Reabs_PT + Reabs_TAL_basal + Reabs_TAL_CaSR_norm + 
                              Reabs_TAL_PTH_norm + Reabs_DCT_basal + 
                              Reabs_DCT_PTH_norm + Reabs_DCT_D3_norm)) *
      GFR*(Ca_p + CaHPO4_p + CaH2PO4_p)
    
    Reabs_norm <- (Reabs_PT + Reabs_TAL_basal + Reabs_TAL_CaSR_norm + 
                     Reabs_TAL_PTH_norm + Reabs_DCT_basal + 
                     Reabs_DCT_PTH_norm + Reabs_DCT_D3_norm) *
      GFR * (Ca_p + CaHPO4_p + CaH2PO4_p)
    
    # P Kidney #
    
    Reabs_PT_basal_P <- lambda_PT_0
    Reabs_PT_PTH_P_norm <- (delta_PTH_max_P * (K_PT_PTH / PTH_p_norm)^n_reabs_P) /
      (PTH_p^n_reabs_P + (K_PT_PTH / PTH_p_norm)^n_reabs_P)
    Reabs_PT_FGF_P_norm <- (delta_FGF_max * (K_PT_FGF / FGF_p_norm)^n_reabs_P) /
      (FGF_p^n_reabs_P + (K_PT_FGF / FGF_p_norm)^n_reabs_P)
    Reabs_PT_P_norm <- (delta_P_max * (K_PT_P / Pho_p_norm)^n_reabs_P) /
      (PO4_p^n_reabs_P + (K_PT_P / Pho_p_norm)^n_reabs_P)
    Reabs_DCT_basal_P <- lambda_DCT_P
    
    Excretion_P_norm <- (1 - (Reabs_PT_basal_P + Reabs_PT_PTH_P_norm + 
                                Reabs_PT_FGF_P_norm + Reabs_PT_P_norm +
                                Reabs_DCT_basal_P)) *
      GFR * (PO4_p + CaHPO4_p + CaH2PO4_p + NaPO4_p)
    
    Reabs_P_norm <- (Reabs_PT_basal_P + Reabs_PT_PTH_P_norm + 
                       Reabs_PT_FGF_P_norm + Reabs_PT_P_norm +
                       Reabs_DCT_basal_P) *
      GFR * (PO4_p + CaHPO4_p + CaH2PO4_p + NaPO4_p)
    
    # Intracellular P #
    
    Plasma_intra_Flux_norm <- k_pc * PO4_p * Vp
    Intra_plasma_Flux_norm <- k_cp * PO4_c 
    
    # Ca/P from HPO42- and H2PO4+  in plasma #
    
    k_form_CaHPO4_norm <- k_f_CaHPO4 * Ca_p * a * PO4_p * f2^2
    k_diss_CaHPO4_norm <- k_d_CaHPO4 * CaHPO4_p
    k_form_CaH2PO4_norm <- k_f_CaH2PO4 * Ca_p * b * PO4_p * f2 * f1
    k_diss_CaH2PO4_norm <- k_d_CaH2PO4 * CaH2PO4_p * f1
    
    # Ca/P from HPO42- and H2PO4+  in the fast bone pool #
    
    k_form_CaHPO4f_norm <- k_f_CaHPO4 * Ca_f * a * PO4_f * f2^2
    k_diss_CaHPO4f_norm <- k_d_CaHPO4 * CaHPO4_f
    k_form_CaH2PO4f_norm <- k_f_CaH2PO4 * Ca_f * b * PO4_f * f2 * f1
    k_diss_CaH2PO4f_norm <- k_d_CaH2PO4 * CaH2PO4_f * f1
    
    # Fetuin-A complexation with CaHPO4 and CaH2PO4 in plasma #
    
    k_fet_CaHPO4_norm <- k_fet * CaHPO4_p
    k_fet_CaH2PO4_norm <- k_fet * CaH2PO4_p*f1
    
    # CPP degradation 
    
    CPP_degradation <- k_c_CPP * CPP_p
    
    # CaProt formation
    
    k_form_CaProt <- k_f_CaProt * Ca_p * (N_Prot * Prot_tot_p - CaProt_p)
    k_diss_CaProt <- k_d_CaProt * CaProt_p
    
    # Na and phosphate reaction in plasma
    
    k_form_NaPO4 <- (a * k_f_NaHPO4 + b * k_f_NaH2PO4) * Na * PO4_p
    k_diss_NaPO4 <- (c * k_d_NaHPO4 + d * k_d_NaH2PO4) * NaPO4_p
    
    # EGTA reaction
    
    EGTA_form <- k_on_egta * Ca_p * EGTA_p
    EGTA_diss <- k_off_egta * CaEGTA_p
    
    
    ##################
    #                #
    #    Rate of     #
    #    Change      #
    ##################
    
    # PTHg
    dPTH_g <- PTHg_synthesis_norm - PTHg_degradation_norm - PTHg_exocytosis_norm
    
    # PTHp
    dPTH_p <- k_inject_PTH + PTHp_influx_norm - PTHp_degradation_norm 
    
    # D3
    dD3_p <- k_inject_D3 + D3_synthesis_norm - D3_degradation_norm 
    
    # FGF23
    dFGF_p <- k_inject_FGF + FGF_synthesis_norm - FGF_degradation_norm 
    
    # Plasma Ca
    dCa_p <- 1 / Vp * (k_inject_Ca + Abs_intest_norm  + Resorption_norm/Ca_p_norm - 
                         Rapid_storage_Ca + Rapid_release_Ca - Excretion_norm) -
      k_form_CaProt + k_diss_CaProt - k_form_CaHPO4_norm*HPO4_norm + 
      k_diss_CaHPO4_norm * CaHPO4_norm / Ca_p_norm -
      k_form_CaH2PO4_norm / Ca_p_norm + 
      k_diss_CaH2PO4_norm * CaH2PO4_norm / Ca_p_norm -
      EGTA_form + EGTA_diss / Ca_p_norm 
    
    # Rapid Bone Ca
    dCa_f <- Rapid_storage_Ca - Rapid_release_Ca - Accretion_norm - 
      k_form_CaHPO4f_norm * Ca_p_norm * HPO4_norm / CaHPO4_norm + 
      k_diss_CaHPO4f_norm - 
      k_form_CaH2PO4f_norm * Ca_p_norm*H2PO4_norm / CaH2PO4_norm + 
      k_diss_CaH2PO4f_norm  
    
    # Slow Bone Ca
    dCa_b <- 1 / Ca_b_norm * (Accretion_norm - Resorption_norm) 
    
    # plasma PO4
    dPO4_p <- 1 / Vp * (k_inject_P + Abs_intest_P_norm + Resorption_P_norm / Pho_p_norm - 
                          Rapid_storage_P + Rapid_release_P - Excretion_P_norm - 
                          Plasma_intra_Flux_norm + Intra_plasma_Flux_norm / Pho_p_norm) - 
      k_form_CaHPO4_norm * Ca_p_norm + 
      k_diss_CaHPO4_norm * CaHPO4_norm / Pho_p_norm - 
      k_form_CaH2PO4_norm * Ca_p_norm + 
      k_diss_CaH2PO4_norm * CaH2PO4_norm / Pho_p_norm - 
      k_form_NaPO4 + k_diss_NaPO4 
    
    # Rapid Bone PO4
    dPO4_f <- Rapid_storage_P - Rapid_release_P - Accretion_P_norm - 
      k_form_CaHPO4f_norm * Ca_p_norm*HPO4_norm / CaHPO4_norm + 
      k_diss_CaHPO4f_norm - k_form_CaH2PO4f_norm * Ca_p_norm * 
      H2PO4_norm / CaH2PO4_norm + k_diss_CaH2PO4f_norm 
    
    # Slow bone PO4
    dPO4_b <- 1 / Pho_b_norm * (Accretion_P_norm - Resorption_P_norm) 
    
    # Intracellular PO4
    dPO4_c <- Plasma_intra_Flux_norm / Pho_c_norm - Intra_plasma_Flux_norm 
    
    # CaHPO4p
    dCaHPO4_p <- k_form_CaHPO4_norm * Ca_p_norm*HPO4_norm / CaHPO4_norm - 
      k_diss_CaHPO4_norm - k_fet_CaHPO4_norm 
    
    # Ca(H2PO4)2p
    dCaH2PO4_p <- k_form_CaH2PO4_norm * Ca_p_norm * H2PO4_norm / CaH2PO4_norm - 
      k_diss_CaH2PO4_norm - k_fet_CaH2PO4_norm 
    
    # CPP particles
    dCPP_p <- k_fet_CaHPO4_norm + k_fet_CaH2PO4_norm - CPP_degradation  
    
    # CaHPO4 fast pool
    dCaHPO4_f <- k_form_CaHPO4f_norm * Ca_p_norm * HPO4_norm / CaHPO4_norm - 
      k_diss_CaHPO4f_norm 
    
    # CaH2PO4 fast pool
    dCaH2PO4_f <- k_form_CaH2PO4f_norm * Ca_p_norm * H2PO4_norm / CaH2PO4_norm - 
      k_diss_CaH2PO4f_norm 
    
    # CaProt plasma
    dCaProt_p <- k_form_CaProt - k_diss_CaProt 
    
    # NaPho plasma
    dNaPO4_p <- k_form_NaPO4 - k_diss_NaPO4 
    
    # calcium total
    dCa_tot <- dCa_p + 1 * dCaHPO4_p + 1 * dCaH2PO4_p + dCaProt_p + 1 * dCPP_p 
    
    # phosphate total
    dPO4_tot <- dPO4_p + 1 * dCaHPO4_p + 1 * dCaH2PO4_p + dNaPO4_p + 1 * dCPP_p 
    
    # equation for continuous EGTA injection
    dEGTA_p <- 1 / Vp * k_inject_egta / EGTA_norm - EGTA_form + EGTA_diss / EGTA_norm 
    
    # kinetic of CaEGTA_p complex
    dCaEGTA_p <- EGTA_form / Ca_EGTA_norm - EGTA_diss 
    
    
    ##################
    #                #
    #    Results     #
    ##################
    
    # return the list of variables as well as fluxes in 
    # another vector
    list(
      list(
        dPTH_g, dPTH_p, dD3_p, dFGF_p, dCa_p, dCa_f, dCa_b, dPO4_p, dPO4_f, 
        dPO4_b, dPO4_c, dCaHPO4_p, dCaH2PO4_p, dCPP_p, dCaHPO4_f, dCaH2PO4_f,
        dCaProt_p, dNaPO4_p, dCa_tot, dPO4_tot, dEGTA_p, dCaEGTA_p
      ), 
      list(U_Ca = Excretion_norm, # out 24
           U_PO4 = Excretion_P_norm, 
           Abs_int_Ca = Abs_intest_norm,
           Abs_int_PO4 = Abs_intest_P_norm, 
           Res_Ca = Resorption_norm, 
           Res_PO4 = Resorption_P_norm, 
           Ac_Ca = Accretion_norm, 
           Ac_PO4 = Accretion_P_norm, 
           Reabs_Ca = Reabs_norm, 
           Reabs_PO4 = Reabs_P_norm,
           Ca_pf = Rapid_storage_Ca, # out 34
           Ca_fp = Rapid_release_Ca, 
           PO4_pf = Rapid_storage_P,
           PO4_fp = Rapid_release_P, 
           PO4_pc = Plasma_intra_Flux_norm,
           PO4_cp = Intra_plasma_Flux_norm,
           PTHg_synth = PTHg_synthesis_norm, # out 40
           PTHg_synth_D3 = PTHg_synthesis_D3_norm,
           PTHg_synth_PO4 = PTHg_synthesis_PO4_norm,
           PTHg_exo_CaSR = F_Ca_norm,
           PTHg_deg = PTHg_degradation_norm,
           PTHg_exo = PTHg_exocytosis_norm, # 45
           PTHp_deg = PTHp_degradation_norm,
           Reabs_PT_PTH = Reabs_PT_PTH_norm,
           Reabs_TAL_CaSR = Reabs_TAL_CaSR_norm,
           Reabs_TAL_PTH = Reabs_TAL_PTH_norm,
           Reabs_DCT_PTH = Reabs_DCT_PTH_norm, # 50
           Reabs_DCT_D3 = Reabs_DCT_D3_norm,
           Abs_int_D3 = Abs_intest_D3_norm,
           Res_PTH = Resorption_PTH_norm,
           Res_D3 = Resorption_D3_norm,
           Reabs_PT_PO4_PTH = Reabs_PT_PTH_P_norm, # 55
           Reabs_PT_PO4_FGF = Reabs_PT_FGF_P_norm
      )
    ) 
  })
}