library(deSolve)
library(parallel)
library(plotly)
source("parameters_without_Shiny.R")

calcium_phosphate_core <- function (t, state, parameters){
  with(as.list(c(state, parameters)),{
    
    ##################
    #                #
    #   Equations    #
    #                #
    ##################
    
    # if (t>=0 && t<60){
    #   k_inject_Ca = 5e-004
    # }
    # else if (t>=60 && t<120){
    #   k_inject_egta = 1e-003
    #   k_on_egta = 9e004
    #   k_off_egta = 18
    # }
    
    # Conversion of PTH in concentration
    
    PTH_p <- PTH_p/Vp # be carefull when Vp changes
    
    # PTHg #
    
    PTHg_synthesis_norm <- (k_prod_PTHg*Vc/PTH_g_norm)*PO4_p^n_prod_Pho/((1 + gamma_prod_D3*D3_norm*D3_p)*
                                                                          ((K_prod_PTH_P/Pho_p_norm)^n_prod_Pho + PO4_p^n_prod_Pho))
    PTHg_degradation_norm <- k_deg_PTHg*PTH_g
    n_Ca_norm <- (n1_exo/(1+exp(-rho_exo*Ca_p_norm*(R/Ca_p_norm-Ca_p)))+n2_exo)
    F_Ca_norm <- beta_exo_PTHg - gamma_exo_PTHg*Ca_p^n_Ca_norm/(Ca_p^n_Ca_norm + (K_Ca/Ca_p_norm)^n_Ca_norm)
    PTHg_exocytosis_norm <- F_Ca_norm*PTH_g
    
    # PTHp #
    
    PTHp_influx_norm <- PTHg_exocytosis_norm*PTH_g_norm/PTH_p_norm
    PTHp_degradation_norm <- k_deg_PTHp*PTH_p
    
    # D3 #
    
    D3_basal_synthesis_norm <- k_conv_min*D3_inact/D3_norm
    D3_conv_PTH_norm <- (delta_conv_max*(D3_inact/D3_norm)*PTH_p^n_conv)/(PTH_p^n_conv + (K_conv/PTH_p_norm)^n_conv)
    D3_conv_Ca_norm <- 1/(1+gamma_ca_conv*Ca_p_norm*Ca_p)
    D3_conv_D3_norm <- 1/(1+gamma_D3_conv*D3_norm*D3_p)
    D3_conv_P_norm <- 1/(1+gamma_P_conv*Pho_p_norm*PO4_p)
    D3_conv_FGF_norm <- 1/(1+gamma_FGF_conv*FGF_p_norm*FGF_p)
    D3_synthesis_norm <- D3_basal_synthesis_norm + D3_conv_PTH_norm*D3_conv_Ca_norm*D3_conv_D3_norm*D3_conv_P_norm*D3_conv_FGF_norm
    
    D3_degradation_norm <- (k_deg_D3*(1+gamma_deg_FGF*FGF_p_norm*FGF_p)*D3_p)/(1+gamma_deg_PTH*PTH_p_norm*PTH_p)
    
    # FGF23 #
    
    FGF_basal_synthesis_norm <- k_prod_FGF/FGF_p_norm
    FGF_D3_activ_norm <- delta_max_prod_D3*D3_p^n_prod_FGF/(D3_p^n_prod_FGF+ (K_prod_D3/D3_norm)^n_prod_FGF)
    FGF_P_activ_norm <- PO4_p/(PO4_p+ K_prod_P/Pho_p_norm)
    
    FGF_synthesis_norm <- FGF_basal_synthesis_norm*(1+FGF_D3_activ_norm*FGF_P_activ_norm)
    
    FGF_degradation_norm <- k_deg_FGF*FGF_p
    
    # Ca Abs_intest #
    
    Abs_intest_basal_norm <- 0.25*I_Ca
    Abs_intest_D3_norm <-(0.45*I_Ca*D3_p^n_abs)/(D3_p^n_abs+ (K_abs_D3/D3_norm)^n_abs)
    
    Abs_intest_norm <- (Abs_intest_basal_norm + Abs_intest_D3_norm)/(Ca_p_norm)
    
    # P Abs_intest #
    
    Abs_intest_basal_P_norm <- 0.4*I_P
    Abs_intest_D3_P_norm <-(0.3*I_P*D3_p^n_abs)/(D3_p^n_abs+ (K_abs_D3/D3_norm)^n_abs)
    
    Abs_intest_P_norm <- (Abs_intest_basal_P_norm + Abs_intest_D3_P_norm)/(Pho_p_norm)
    
    # Ca Fast bone #
    
    Caf_flux_norm <- k_p_Ca*Ca_p*Vp - k_f_Ca*Ca_f
    Accretion_norm <- Lambda_ac_Ca*Ca_f
    
    # P Fast Bone #
    
    Pf_flux_norm <- k_p_P*PO4_p*Vp - k_f_P*PO4_f
    Accretion_P_norm <- Lambda_ac_P*PO4_f
    
    # Ca Slow bone #
    
    Resorption_basal <- Lambda_res_min
    Resorption_PTH_norm <- (delta_res_max*0.2*PTH_p^n_res)/(PTH_p^n_res + (K_res_PTH/PTH_p_norm)^n_res)
    Resorption_D3_norm <- (delta_res_max*0.8*D3_p^n_res)/(D3_p^n_res + (K_res_D3/D3_norm)^n_res)
    
    Resorption_norm <- (Resorption_basal + Resorption_PTH_norm + Resorption_D3_norm)
    
    # P Slow Bone #
    
    Resorption_P_norm <- 0.6*Ca_P_stoech*Resorption_norm 
    
    # Ca Kidney #
    
    Reabs_PT_basal <- lambda_reabs_PT_0
    Reabs_PT_PTH_norm <- delta_PT_max/(1+(PTH_p*PTH_p_norm/PTH_ref)^n_PT)
    Reabs_PT <- Reabs_PT_basal + Reabs_PT_PTH_norm
    
    Reabs_TAL_basal <- lambda_TAL_0
    Reabs_TAL_CaSR_norm <- delta_CaSR_max/(1+(Ca_p*Ca_p_norm/Ca_ref)^n_TAL)
    Reabs_TAL_PTH_norm <- delta_PTH_max*PTH_p/(PTH_p+ K_TAL_PTH/PTH_p_norm)
    
    Reabs_DCT_basal <- lambda_DCT_0
    Reabs_DCT_PTH_norm <- (delta_DCT_max*0.8*PTH_p)/(PTH_p + K_DCT_PTH/PTH_p_norm)
    Reabs_DCT_D3_norm <- (delta_DCT_max*0.2*D3_p)/(D3_p + K_DCT_D3/D3_norm)
    
    Excretion_norm <- (1-(Reabs_PT + Reabs_TAL_basal + Reabs_TAL_CaSR_norm + Reabs_TAL_PTH_norm + Reabs_DCT_basal + 
                           Reabs_DCT_PTH_norm + Reabs_DCT_D3_norm))*GFR*(Ca_p+CaHPO4_p+CaH2PO4_p)
    
    # P Kidney #
    
    Reabs_PT_basal_P <- lambda_PT_0
    Reabs_PT_PTH_P_norm <- (delta_PTH_max_P*(K_PT_PTH/PTH_p_norm)^n_reabs_P)/(PTH_p^n_reabs_P + (K_PT_PTH/PTH_p_norm)^n_reabs_P)
    Reabs_PT_FGF_P_norm <- (delta_FGF_max*(K_PT_FGF/FGF_p_norm)^n_reabs_P)/(FGF_p^n_reabs_P + (K_PT_FGF/FGF_p_norm)^n_reabs_P)
    Reabs_PT_P_norm <- (delta_P_max*(K_PT_P/Pho_p_norm)^n_reabs_P)/(PO4_p^n_reabs_P + (K_PT_P/Pho_p_norm)^n_reabs_P)
    Reabs_DCT_basal_P <- lambda_DCT_P
    
    Excretion_P_norm <- (1-(Reabs_PT_basal_P + Reabs_PT_PTH_P_norm + Reabs_PT_FGF_P_norm + Reabs_PT_P_norm +
                             Reabs_DCT_basal_P))*GFR*(PO4_p+ CaHPO4_p+ CaH2PO4_p+ NaPO4_p)
    
    # Intracellular P #
    
    Plasma_intra_Flux_norm <- k_pc*PO4_p*Vp
    Intra_plasma_Flux_norm <- k_cp*PO4_c 
    
    # Ca/P from HPO42- and H2PO4+  in plasma #
    
    k_form_CaHPO4_norm <- k_f_CaHPO4*Ca_p*a*PO4_p*f2^2
    k_diss_CaHPO4_norm <- k_d_CaHPO4*CaHPO4_p
    k_form_CaH2PO4_norm <- k_f_CaH2PO4*Ca_p*b*PO4_p*f2*f1
    k_diss_CaH2PO4_norm <- k_d_CaH2PO4*CaH2PO4_p*f1
    
    # Ca/P from HPO42- and H2PO4+  in the fast bone pool #
    
    k_form_CaHPO4f_norm <- k_f_CaHPO4*Ca_f*a*PO4_f*f2^2
    k_diss_CaHPO4f_norm <- k_d_CaHPO4*CaHPO4_f
    k_form_CaH2PO4f_norm <- k_f_CaH2PO4*Ca_f*b*PO4_f*f2*f1
    k_diss_CaH2PO4f_norm <- k_d_CaH2PO4*CaH2PO4_f*f1
    
    # Fetuin-A complexation with CaHPO4 and CaH2PO4 in plasma #
    
    k_fet_CaHPO4_norm <- k_fet*CaHPO4_p
    k_fet_CaH2PO4_norm <- k_fet*CaH2PO4_p*f1
    
    # CPP degradation 
    
    CPP_degradation <- k_c_CPP*CPP_p
    
    # CaProt formation
    
    k_form_CaProt <- k_f_CaProt*Ca_p*(N_Prot*Prot_tot_p - CaProt_p)
    k_diss_CaProt <- k_d_CaProt*CaProt_p
    
    # Na and phosphate reaction in plasma
    
    k_form_NaPO4 <- (a*k_f_NaHPO4+b*k_f_NaH2PO4)*Na*PO4_p
    k_diss_NaPO4 <- (c*k_d_NaHPO4 + d*k_d_NaH2PO4)*NaPO4_p
    
    # EGTA reaction
    
    EGTA_form = k_on_egta*Ca_p*EGTA_p;
    EGTA_diss = k_off_egta*CaEGTA_p;
    
    ##################
    #                #
    #   Simulations  #
    ##################
    
    # if (t>0){
    #   k_inject_P = 0.01
    # }
    
    ##################
    #                #
    #    Rate of     #
    #    Change      #
    ##################
      
    dPTH_g <- PTHg_synthesis_norm - PTHg_degradation_norm - PTHg_exocytosis_norm # PTHg           
    dPTH_p <- k_inject_PTH + PTHp_influx_norm - PTHp_degradation_norm # PTHp
    dD3_p <- k_inject_D3 + D3_synthesis_norm - D3_degradation_norm # D3
    dFGF_p <- k_inject_FGF + FGF_synthesis_norm - FGF_degradation_norm # FGF23
    dCa_p <- (1/Vp)*(k_inject_Ca + Abs_intest_norm  + Resorption_norm/Ca_p_norm - Caf_flux_norm - Excretion_norm) -
                     k_form_CaProt + k_diss_CaProt - k_form_CaHPO4_norm*HPO4_norm + k_diss_CaHPO4_norm*CaHPO4_norm/Ca_p_norm -
                     k_form_CaH2PO4_norm/Ca_p_norm + k_diss_CaH2PO4_norm*CaH2PO4_norm/Ca_p_norm  -
                     EGTA_form + EGTA_diss/Ca_p_norm # Plasma Ca
    dCa_f <- Caf_flux_norm - Accretion_norm - k_form_CaHPO4f_norm*Ca_p_norm*HPO4_norm/CaHPO4_norm + k_diss_CaHPO4f_norm - 
            k_form_CaH2PO4f_norm*Ca_p_norm*H2PO4_norm/CaH2PO4_norm + k_diss_CaH2PO4f_norm  # Rapid Bone Ca
    dCa_b <- (1/Ca_b_norm)*(Accretion_norm - Resorption_norm) # Slow Bone Ca
    dPO4_p <- (1/Vp)*(k_inject_P + Abs_intest_P_norm + Resorption_P_norm/Pho_p_norm - Pf_flux_norm - Excretion_P_norm - 
                      Plasma_intra_Flux_norm + Intra_plasma_Flux_norm/Pho_p_norm) - 
                      k_form_CaHPO4_norm*Ca_p_norm + k_diss_CaHPO4_norm*CaHPO4_norm/Pho_p_norm - 
                      k_form_CaH2PO4_norm*Ca_p_norm + k_diss_CaH2PO4_norm*CaH2PO4_norm/Pho_p_norm - 
                      k_form_NaPO4 + k_diss_NaPO4 # plasma P
    dPO4_f <- Pf_flux_norm - Accretion_P_norm - k_form_CaHPO4f_norm*Ca_p_norm*HPO4_norm/CaHPO4_norm + k_diss_CaHPO4f_norm - 
              k_form_CaH2PO4f_norm*Ca_p_norm*H2PO4_norm/CaH2PO4_norm + k_diss_CaH2PO4f_norm # Rapid Bone P
    dPO4_b <- (1/Pho_b_norm)*(Accretion_P_norm - Resorption_P_norm) # Slow bone P
    dPO4_c <- Plasma_intra_Flux_norm/Pho_c_norm - Intra_plasma_Flux_norm # Intracellular P
    dCaHPO4_p <- k_form_CaHPO4_norm*Ca_p_norm*HPO4_norm/CaHPO4_norm - k_diss_CaHPO4_norm - k_fet_CaHPO4_norm # CaHPO4p
    dCaH2PO4_p <- k_form_CaH2PO4_norm*Ca_p_norm*H2PO4_norm/CaH2PO4_norm - k_diss_CaH2PO4_norm - k_fet_CaH2PO4_norm # Ca(H2PO4)2p
    dCPP_p <- k_fet_CaHPO4_norm + k_fet_CaH2PO4_norm - CPP_degradation  # CPP particles
    dCaHPO4_f <- k_form_CaHPO4f_norm*Ca_p_norm*HPO4_norm/CaHPO4_norm - k_diss_CaHPO4f_norm # CaHPO4 fast pool
    dCaH2PO4_f <- k_form_CaH2PO4f_norm*Ca_p_norm*H2PO4_norm/CaH2PO4_norm - k_diss_CaH2PO4f_norm # CaH2PO4 fast pool
    dCaProt_p <- k_form_CaProt - k_diss_CaProt # CaProt plasma
    dNaPO4_p <- k_form_NaPO4 - k_diss_NaPO4# NaPho plasma
    dCa_tot <- dCa_p + 1*dCaHPO4_p + 1*dCaH2PO4_p + dCaProt_p + 1*dCPP_p # calcium total
    dPO4_tot <- dPO4_p + 1*dCaHPO4_p + 1*dCaH2PO4_p + dNaPO4_p + 1*dCPP_p # phosphate total
    dEGTA_p <- 1/Vp*k_inject_egta/EGTA_norm - EGTA_form + EGTA_diss/EGTA_norm # equation for continuous EGTA injection
    dCaEGTA_p <- EGTA_form/Ca_EGTA_norm - EGTA_diss # kinetic of CaEGTA_p complex
    
    list(c(dPTH_g, dPTH_p, dD3_p, dFGF_p, dCa_p, dCa_f, dCa_b, dPO4_p, dPO4_f, dPO4_b, dPO4_c, dCaHPO4_p, dCaH2PO4_p, dCPP_p, dCaHPO4_f, dCaH2PO4_f,
           dCaProt_p, dNaPO4_p, dCa_tot, dPO4_tot, dEGTA_p, dCaEGTA_p))
    
  })
}

times <- seq(0,1000, by = 1)
state <- c(PTH_g = 1288.238, PTH_p = 0.06874084, D3_p = 564.2508, FGF_p = 16.78112, Ca_p = 1.2061, 
           Ca_f = 1.8363, Ca_b = 250, PO4_p = 1.4783, PO4_f = 0.7922, PO4_b = 90, PO4_c = 2.7719, 
           CaHPO4_p = 0.1059, CaH2PO4_p = 0.0038, CPP_p = 0.0109, CaHPO4_f = 0.0864, CaH2PO4_f = 0.0031, CaProt_p = 1.4518,
           NaPO4_p = 0.9135, Ca_tot = 2.4914, PO4_tot = 2.8354, EGTA_p = 0, CaEGTA_p = 0) 

out <- as.data.frame(ode(y = state, times = times, 
                  func = calcium_phosphate_core, parms = parameters)) # atol = 1e-10, rtol = 1e-10


par(mfrow=c(2,3))

plot(out[,1],out[,"Ca_p"], type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"Ca_p"]*1.2)), col ="blue")
plot(out[,1],out[,"PO4_p"], type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"PO4_p"]*1.2)), col = "red")
plot(out[,1],out[,"PTH_p"]/parameters["Vp"], type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"PTH_p"]/parameters["Vp"]*1.2)))
plot(out[,1],out[,"FGF_p"], type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"FGF_p"]*1.2)))
plot(out[,1],out[,"D3_p"], type = "l", xlim = c(0,max(out[,1])), ylim = c(0,max(out[,"D3_p"]*1.2)))

out[nrow(out),-1] # final values

diagnostics(ode(y = state, times = times, 
                func = calcium_phosphate_core, parms = parameters))

##################
#                #
#   Sparkline    #
#                #
################## 

sparkline(out[,"Ca_p"], width = 200, height = 100)

node <- data.frame(id = 1, title = c('
                                     <script type="text/javascript">
                                     console.info("la")
                                     $(function(test,test_bis) {
                                     var myvalues = [1,2,3]; 
                                     $(".dynamicsparkline").sparkline(myvalues);
                                     });
                                     </script>
                                     <span class="dynamicsparkline">Loading..</span>'))

visNetwork(node, edges = data.frame())%>%spk_add_deps()
# cat(out[,"Ca_p"], sep = ", ")
# paste(round(out[seq(1,nrow(out), by = 100),"Ca_p"],2), collapse = ", ")

##################
#                #
#  Stream plot   #
#                #
##################

df <- out[seq(1,nrow(out), by = 50),]
df$ID <- seq.int(nrow(df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>%
  accumulate_by(~ID)

p <- ggplot(df,aes(ID, Ca_p, frame = frame)) +  # stream plot Cap test
  geom_line()

p <- ggplotly(p)

##################
#                #
#    Events      #
#                #
##################

# injectevents <- data.frame(var = "Ca_p",
#                            time = 0:60,
#                            value = 100*1.3e-003,
#                            method = "add")

injectevents <- data.frame(times = c(0, 60, 65, 70, 80, 90, 100, 110, 120), Ca_val = 1/1.35*c(1.35, 1.45, 1.30, 1.20, 1.15, 1.10, 1.10, 1.00, 1.05),
                           PTH_val = 1/65*c(65, 10, 190, 260, 300, 260, 240, 290, 310), err_Ca = 1/1.35*2*c(0.02,0.04,0.04,0.06,0.04,0.06,0.06,0.07, 0.04),
                           err_PTH = 1/65*c(20, 0, 70, 100, 70, 70, 50, 70, 110))
times <- seq(0,120, by = 1)
out <- as.data.frame(ode(y = state, times = times, func = calcium_phosphate_core, parms = parameters))
plot(out[,1],out[,"Ca_p"]/out[1,"Ca_p"], type = "l", xlim = c(0,120), ylim = c(0,max(out[,"Ca_p"]/out[1,"Ca_p"]*1.2)), col ="blue")
points(x= injectevents$times, y = injectevents$Ca_val)

obj_function <- sum(out[injectevents$times + 1, "Ca_p"]/out[1, "Ca_p"] - injectevents$Ca_val)^2 + 
                sum(out[injectevents$times + 1, "PTH_p"]/out[1, "PTH_p"] - injectevents$PTH_val)^2

##################
#                #
#      PHP1      #
#                #
##################

php1_vec <- parameters["k_prod_PTHg"]*seq(1,100,by = 10) # create the sequence of PTH production rate
names(php1_vec) <- paste("k_prod_PTHg =", php1_vec)

no_cores <- detectCores() -1 # initiate parallel computing
cl <- makeCluster(no_cores)

clusterExport(cl=cl, varlist=c("parameters", "state", "times", "php1_vec","ode","calcium_phosphate_core", "Lambda_ac_P", "a", "r", "b"),
              envir=environment()) # export all previous variables to each R 
                                   #process belonging to the cluster

list_php1 <- parLapply(cl, php1_vec, function(x) {
  parameters["k_prod_PTHg"] <- x
  out_php1 <- ode(y = state, times = times, func = calcium_phosphate_core, parms = parameters, method = "lsodes")
  #PTH_php1_eq <- out_php1[nrow(out_php1),"PTH_p"] # we only take the last value at equilibrium
  #Ca_php1_eq <- out_php1[nrow(out_php1),"Ca_p"]
  PO4_php1_eq <- out_php1[nrow(out_php1),]
  #res <- c(PTH_php1_eq, Ca_php1_eq)
  
  #out_php1 <- out_php1[nrow(out_php1),]
  
})

list_php1 <- data.frame(t(as.data.frame(list_php1))) # convert into a "good" dataframe for plotting

clusterExport(cl=cl, varlist=c("list_php1"), envir=environment())


stopCluster(cl)

plot_PO4_php1 <- plot_ly(list_php1, x = php1_vec, y = list_php1[,"PO4_p"], type = "scatter", mode = "lines")