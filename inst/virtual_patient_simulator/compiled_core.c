/*
#  This is the model core containing all equations and fluxes,
#  it is translated from a previous Matlab code. Here it is 
#  in a compiled version (C)
#
#  David Granjon, the Interface Group, Zurich
#  November 13th, 2017
*/

/* includes */
#include <R.h>

/* define parameters */
static double parameters[133];

#define k_prod_PTHg parameters[0]      
#define D3_inact parameters[1]         
#define Vp parameters[2]                
#define Vc parameters[3]                
#define k_deg_PTHg parameters[4]        
#define k_deg_PTHp parameters[5]       
#define K_Ca parameters[6]             
#define beta_exo_PTHg parameters[7]     
#define gamma_exo_PTHg parameters[8]   
#define gamma_prod_D3 parameters[9]     
#define n1_exo parameters[10]           
#define n2_exo parameters[11]    
#define rho_exo parameters[12]           
#define R parameters[13]                 
#define K_prod_PTH_P parameters[14]     
#define n_prod_Pho parameters[15]        
#define k_conv_min parameters[16]       
#define delta_conv_max parameters[17]  
#define gamma_ca_conv parameters[18]     
#define gamma_D3_conv parameters[19]     
#define gamma_P_conv parameters[20]    
#define gamma_FGF_conv parameters[21]   
#define K_conv parameters[22]            
#define n_conv parameters[23]   
#define k_deg_D3 parameters[24]          
#define gamma_deg_PTH parameters[25]     
#define gamma_deg_FGF parameters[26]     
#define k_prod_FGF parameters[27]       
#define delta_max_prod_D3 parameters[28] 
#define K_prod_D3 parameters[29]
#define n_prod_FGF parameters[30]       
#define K_prod_P parameters[31]         
#define k_deg_FGF parameters[32]        
#define K_abs_D3 parameters[33]          
#define I_Ca parameters[34]              
#define n_abs parameters[35]
#define I_P parameters[36]             
#define Lambda_ac_Ca parameters[37]      
#define Lambda_ac_P parameters[38]       
#define Lambda_res_min parameters[39]    
#define delta_res_max parameters[40]     
#define K_res_D3 parameters[41] 
#define K_res_PTH parameters[42]         
#define n_res parameters[43]             
#define k_p_Ca parameters[44]            
#define k_f_Ca parameters[45]            
#define k_p_P parameters[46]            
#define k_f_P parameters[47] 
#define k_pc parameters[48]             
#define k_cp parameters[49]              
#define k_f_CaHPO4 parameters[50]        
#define k_d_CaHPO4 parameters[51]        
#define k_f_CaH2PO4 parameters[52]      
#define k_d_CaH2PO4 parameters[53]
#define f1 parameters[54]               
#define f2 parameters[55]               
#define f3 parameters[56]               
#define k_fet parameters[57]            
#define k_c_CPP parameters[58]          
#define k_f_CaProt parameters[59] 
#define k_d_CaProt parameters[60]       
#define N_Prot parameters[61]           
#define Prot_tot_p parameters[62]       
#define k_f_NaHPO4 parameters[63]       
#define k_f_NaH2PO4 parameters[64]      
#define Na parameters[65]   
#define k_d_NaHPO4 parameters[66]        
#define k_d_NaH2PO4 parameters[67]      
#define c parameters[68]                
#define d parameters[69]                
#define GFR parameters[70]              
#define lambda_reabs_PT_0 parameters[71]
#define delta_PT_max parameters[72]     
#define PTH_ref parameters[73]          
#define n_PT parameters[74]              
#define lambda_TAL_0 parameters[75]      
#define delta_CaSR_max parameters[76]    
#define Ca_ref parameters[77]  
#define n_TAL parameters[78]             
#define K_TAL_PTH parameters[79]         
#define delta_PTH_max parameters[80]    
#define lambda_DCT_0 parameters[81]      
#define delta_DCT_max parameters[82]     
#define K_DCT_PTH parameters[83] 
#define K_DCT_D3 parameters[84]         
#define n_reabs_P parameters[85]        
#define lambda_PT_0 parameters[86]      
#define delta_PTH_max_P parameters[87]   
#define K_PT_PTH parameters[88]         
#define delta_P_max parameters[89]
#define K_PT_P parameters[90]           
#define delta_FGF_max parameters[91]     
#define K_PT_FGF parameters[92]         
#define lambda_DCT_P parameters[93]     
#define pH parameters[94]               
#define pKa parameters[95]  
#define D3_norm parameters[96]           
#define PTH_g_norm parameters[97]       
#define PTH_p_norm parameters[98]       
#define FGF_p_norm parameters[99]       
#define Ca_p_norm parameters[100]        
#define Ca_f_norm parameters[101] 
#define Ca_b_norm parameters[102]        
#define Pho_p_norm parameters[103]       
#define Pho_c_norm parameters[104]       
#define Pho_f_norm parameters[105]       
#define Pho_b_norm parameters[106]       
#define HPO4_norm parameters[107] 
#define H2PO4_norm parameters[108]        
#define CaHPO4_norm parameters[109]      
#define CaH2PO4_norm parameters[110]     
#define CPP_norm parameters[111]         
#define EGTA_norm parameters[112]        
#define Ca_EGTA_norm parameters[113] 
#define k_on_egta parameters[114]        
#define k_off_egta parameters[115]       
#define k_inject_egta parameters[116]    
#define K_sp_DCPD parameters[117]        
 
/* define 3 calculated parameters */
#define r parameters[118]
#define a parameters[119]
#define b parameters[120]

/* define simulated parameters */
#define PTX_coeff parameters[121]
#define t_start parameters[122]
#define t_stop parameters[123]
#define Ca_inject parameters[124]
#define Ca_food parameters[125]
#define D3_inject parameters[126]
#define P_inject parameters[127]
#define P_food parameters[128]
#define D3_intake_reduction parameters[129]
#define Bispho parameters[130]
#define Furo parameters[131]
#define Cinacal parameters[132]

/* initializer  */
void initmod(void (* odeparms)(int *, double *))
{
  int N = 133;
  odeparms(&N, parameters);
}
/* Derivatives and 1 output variable */
void derivs (int *neq, double *t, double *y, double *ydot,
             double *yout, int *ip)
{
  if (ip[0] <1) error("nout should be at least 1");
  
  /* define equations */
  double PTHg_basal_synthesis_norm, PTHg_synthesis_D3_norm,  
         PTHg_synthesis_PO4_norm, PTHg_synthesis_norm,  PTHg_degradation_norm,
         n_Ca_norm,  F_Ca_norm,  PTHg_exocytosis_norm, PTHp_influx_norm,  
         PTHp_degradation_norm, D3_basal_synthesis_norm,  D3_conv_PTH_norm,
         D3_conv_Ca_norm,  D3_conv_D3_norm, D3_conv_P_norm,  D3_conv_FGF_norm, 
         D3_synthesis_norm,  D3_degradation_norm, FGF_basal_synthesis_norm,  
         FGF_D3_activ_norm, FGF_P_activ_norm,  FGF_synthesis_norm,
         FGF_degradation_norm,  Abs_intest_basal_norm,
         Abs_intest_D3_norm,  Abs_intest_norm, Abs_intest_basal_P_norm,  
         Abs_intest_D3_P_norm, Abs_intest_P_norm,  Rapid_storage_Ca, 
         Rapid_release_Ca,  Accretion_norm, Rapid_storage_P,  Rapid_release_P,
         Accretion_P_norm,  Resorption_basal, Resorption_PTH_norm,  
         Resorption_D3_norm, Resorption_norm,  Resorption_P_norm,
         Reabs_PT_basal,  Reabs_PT_PTH_norm, Reabs_PT,  Reabs_TAL_basal, 
         Reabs_TAL_CaSR_norm,  Reabs_TAL_PTH_norm, Reabs_DCT_basal,  
         Reabs_DCT_PTH_norm, Reabs_DCT_D3_norm,  Excretion_norm,
         Reabs_norm,  Reabs_PT_basal_P, Reabs_PT_PTH_P_norm,  Reabs_PT_FGF_P_norm,
         Reabs_PT_P_norm,  Reabs_DCT_basal_P, Excretion_P_norm,  Reabs_P_norm,
         Plasma_intra_Flux_norm,  Intra_plasma_Flux_norm, k_form_CaHPO4_norm,  
         k_diss_CaHPO4_norm, k_form_CaH2PO4_norm,  k_diss_CaH2PO4_norm,
         k_form_CaHPO4f_norm,  k_diss_CaHPO4f_norm, k_form_CaH2PO4f_norm,  
         k_diss_CaH2PO4f_norm, k_fet_CaHPO4_norm,  k_fet_CaH2PO4_norm,
         CPP_degradation,  k_form_CaProt, k_diss_CaProt,  k_form_NaPO4,  
         k_diss_NaPO4, EGTA_form,  EGTA_diss;
  
  
  /* define parameters for simulations*/
  double k_inject_P = 0, k_inject_Ca = 0, k_inject_FGF = 0, 
         k_inject_PTH = 0, k_inject_D3 = 0;
  
  /* simulation engine */
  if (t_stop != 0) {
    if (*t >= t_start && *t <= t_stop) {
      if (Ca_inject != 0) {
        k_inject_Ca = Ca_inject;
      } else if (Ca_food != 0) {
        I_Ca = Ca_food;
      } else if (D3_inject != 0) {
        k_inject_D3 = D3_inject * Vp;
      } else if (P_inject != 0) {
        k_inject_P = P_inject;
      } else if (P_food != 0) {
        I_P = P_food;
      } 
    }
  }
   
  
  /* PTHg */
  PTHg_basal_synthesis_norm = k_prod_PTHg * Vc / PTH_g_norm;
  PTHg_synthesis_D3_norm = 1 / (1 + gamma_prod_D3 * D3_norm * y[2]);
  PTHg_synthesis_PO4_norm = pow(y[7], n_prod_Pho) / 
                          (pow(K_prod_PTH_P / Pho_p_norm, n_prod_Pho) + pow(y[7], n_prod_Pho));
    
  PTHg_synthesis_norm = PTX_coeff * PTHg_basal_synthesis_norm * PTHg_synthesis_D3_norm *
                        PTHg_synthesis_PO4_norm;
    
  PTHg_degradation_norm = k_deg_PTHg * y[0];
  n_Ca_norm = n1_exo / (1 + exp(-rho_exo * Ca_p_norm * (R / Ca_p_norm - y[4]))) + n2_exo;
  // First action of cinacalcet
  if (Cinacal == 0) {
    F_Ca_norm = beta_exo_PTHg - gamma_exo_PTHg * pow(y[4], n_Ca_norm) /
      (pow(y[4], n_Ca_norm) + pow(K_Ca / Ca_p_norm, n_Ca_norm));;
  } else {
    F_Ca_norm = beta_exo_PTHg - gamma_exo_PTHg;
  }
  // F_Ca_norm = (beta_exo_PTHg - gamma_exo_PTHg) * 0.035;
  PTHg_exocytosis_norm = F_Ca_norm * y[0];
  
  /* PTHp */
  PTHp_influx_norm = PTHg_exocytosis_norm * PTH_g_norm / PTH_p_norm;
  PTHp_degradation_norm = k_deg_PTHp * y[1] / Vp;
  
  /* D3p */
  D3_basal_synthesis_norm = (1 - D3_intake_reduction / 100) * k_conv_min * D3_inact / D3_norm;
  D3_conv_PTH_norm = (delta_conv_max * (D3_inact / D3_norm) * pow(y[1] / Vp, n_conv)) /
                     (pow(y[1] / Vp, n_conv) + pow(K_conv / PTH_p_norm, n_conv));
  D3_conv_Ca_norm = 1 / (1 + gamma_ca_conv * Ca_p_norm * y[4]);
  D3_conv_D3_norm = 1 / (1 + gamma_D3_conv * D3_norm * y[2]);
  D3_conv_P_norm = 1 / (1 + gamma_P_conv * Pho_p_norm * y[7]);
  D3_conv_FGF_norm = 1 / (1 + gamma_FGF_conv * FGF_p_norm * y[3]);
  D3_synthesis_norm = D3_basal_synthesis_norm + D3_conv_PTH_norm * D3_conv_Ca_norm *
                      D3_conv_D3_norm*D3_conv_P_norm*D3_conv_FGF_norm;
    
  D3_degradation_norm = (k_deg_D3 * (1 + gamma_deg_FGF * FGF_p_norm * y[3]) * y[2]) /
                        (1 + gamma_deg_PTH * PTH_p_norm * y[1] / Vp);
  
  /* FGF23p */
  FGF_basal_synthesis_norm = k_prod_FGF / FGF_p_norm;
  FGF_D3_activ_norm = delta_max_prod_D3 * pow(y[2], n_prod_FGF) /
                      (pow(y[2], n_prod_FGF) + pow(K_prod_D3 / D3_norm, n_prod_FGF));
  FGF_P_activ_norm = y[7] / (y[7] + K_prod_P / Pho_p_norm);
  FGF_synthesis_norm = FGF_basal_synthesis_norm * (1 + FGF_D3_activ_norm * FGF_P_activ_norm);
  FGF_degradation_norm = k_deg_FGF * y[3];
  
  /* Ca Abs_intest */
  Abs_intest_basal_norm = 0.25 * I_Ca;
  Abs_intest_D3_norm = (0.45 * I_Ca * pow(y[2], n_abs)) / 
                        (pow(y[2], n_abs) + pow(K_abs_D3 / D3_norm, n_abs));
  Abs_intest_norm = (Abs_intest_basal_norm + Abs_intest_D3_norm) / Ca_p_norm;
  
  /* PO4 abs_intest */
  Abs_intest_basal_P_norm = 0.4 * I_P;
  Abs_intest_D3_P_norm = (0.3 * I_P * pow(y[2], n_abs)) / 
                          (pow(y[2], n_abs) + pow(K_abs_D3 / D3_norm, n_abs));
  Abs_intest_P_norm = (Abs_intest_basal_P_norm + Abs_intest_D3_P_norm) / Pho_p_norm;
  
  /* Ca Fast bone */
  Rapid_storage_Ca = k_p_Ca * y[4] * Vp;  
  Rapid_release_Ca = k_f_Ca * y[5];
  Accretion_norm = Lambda_ac_Ca * y[5];
    
  /* P Fast Bone */
  Rapid_storage_P = k_p_P * y[7] * Vp;
  Rapid_release_P = k_f_P * y[8];
  Accretion_P_norm = Lambda_ac_P * y[8];
  
  /* Ca Slow bone */
  Resorption_basal = Lambda_res_min;
  Resorption_PTH_norm = (delta_res_max * 0.2 * pow(y[1] / Vp, n_res)) /
                         (pow(y[1] / Vp, n_res) + pow(K_res_PTH / PTH_p_norm, n_res));
  Resorption_D3_norm = (delta_res_max * 0.8 * pow(y[2], n_res)) /
                        (pow(y[2], n_res) + pow(K_res_D3 / D3_norm, n_res));
    
  Resorption_norm = Bispho * (Resorption_basal + Resorption_PTH_norm + Resorption_D3_norm);
    
  /* P Slow Bone */
  Resorption_P_norm = 0.3 * Resorption_norm;
  
  /* Ca Kidney */
  Reabs_PT_basal = lambda_reabs_PT_0;
  Reabs_PT_PTH_norm = delta_PT_max / (1 + pow(y[1] / Vp*PTH_p_norm / PTH_ref, n_PT));
  Reabs_PT = Reabs_PT_basal + Reabs_PT_PTH_norm;
    
  Reabs_TAL_basal = lambda_TAL_0;
  // Second action of cinacalcet if any
  if (Cinacal == 0) {
    Reabs_TAL_CaSR_norm = delta_CaSR_max / (1 + pow(y[4] * Ca_p_norm / Ca_ref, n_TAL));
  } else {
    Reabs_TAL_CaSR_norm = delta_CaSR_max;
  }
  
  Reabs_TAL_PTH_norm = delta_PTH_max * y[1] / Vp / (y[1] / Vp + K_TAL_PTH / PTH_p_norm);
    
  Reabs_DCT_basal = lambda_DCT_0;
  Reabs_DCT_PTH_norm = (delta_DCT_max * 0.8 * y[1] / Vp) / 
                       (y[1] / Vp + K_DCT_PTH / PTH_p_norm);
  Reabs_DCT_D3_norm = (delta_DCT_max * 0.2 * y[2]) / 
                      (y[2] + K_DCT_D3 / D3_norm);
    
  Excretion_norm = Furo * (1 - (Reabs_PT + Reabs_TAL_basal + Reabs_TAL_CaSR_norm + 
                   Reabs_TAL_PTH_norm + Reabs_DCT_basal + 
                   Reabs_DCT_PTH_norm + Reabs_DCT_D3_norm)) *
                   GFR*(y[4] + y[11] + y[12]);
    
  Reabs_norm = (Reabs_PT + Reabs_TAL_basal + Reabs_TAL_CaSR_norm + 
               Reabs_TAL_PTH_norm + Reabs_DCT_basal + 
               Reabs_DCT_PTH_norm + Reabs_DCT_D3_norm) *
               GFR * (y[4] + y[11] + y[12]);
  
  /* P Kidney */
  Reabs_PT_basal_P = lambda_PT_0;
  Reabs_PT_PTH_P_norm = (delta_PTH_max_P * pow(K_PT_PTH / PTH_p_norm, n_reabs_P)) /
                        (pow(y[1] / Vp, n_reabs_P) + pow(K_PT_PTH / PTH_p_norm, n_reabs_P));
  Reabs_PT_FGF_P_norm = (delta_FGF_max * pow(K_PT_FGF / FGF_p_norm, n_reabs_P)) /
                        (pow(y[3], n_reabs_P) + pow(K_PT_FGF / FGF_p_norm, n_reabs_P));
  Reabs_PT_P_norm = (delta_P_max * pow(K_PT_P / Pho_p_norm, n_reabs_P)) /
                    (pow(y[7], n_reabs_P) + pow(K_PT_P / Pho_p_norm, n_reabs_P));
  Reabs_DCT_basal_P = lambda_DCT_P;
    
  Excretion_P_norm = (1 - (Reabs_PT_basal_P + Reabs_PT_PTH_P_norm + 
                     Reabs_PT_FGF_P_norm + Reabs_PT_P_norm +
                     Reabs_DCT_basal_P)) *
                     GFR * (y[7] + y[11] + y[12] + y[17]);
    
  Reabs_P_norm = (Reabs_PT_basal_P + Reabs_PT_PTH_P_norm + 
                 Reabs_PT_FGF_P_norm + Reabs_PT_P_norm +
                 Reabs_DCT_basal_P) *
                 GFR * (y[7] + y[11] + y[12] + y[17]);
  
  /* Intracellular P */
  Plasma_intra_Flux_norm = k_pc * y[7] * Vp;
  Intra_plasma_Flux_norm = k_cp * y[10];
    
  /* Ca/P from HPO42- and H2PO4+  in plasma */
  k_form_CaHPO4_norm = k_f_CaHPO4 * y[4] * a * y[7] * pow(f2, 2);
  k_diss_CaHPO4_norm = k_d_CaHPO4 * y[11];
  k_form_CaH2PO4_norm = k_f_CaH2PO4 * y[4] * b * y[7] * f2 * f1;
  k_diss_CaH2PO4_norm = k_d_CaH2PO4 * y[12] * f1;
      
  /* Ca/P from HPO42- and H2PO4+  in the fast bone pool */
  k_form_CaHPO4f_norm = k_f_CaHPO4 * y[5] * a * y[8] * pow(f2, 2);
  k_diss_CaHPO4f_norm = k_d_CaHPO4 * y[14];
  k_form_CaH2PO4f_norm = k_f_CaH2PO4 * y[5] * b * y[8] * f2 * f1;
  k_diss_CaH2PO4f_norm = k_d_CaH2PO4 * y[15] * f1;
        
  /* Fetuin-A complexation with CaHPO4 and CaH2PO4 in plasma */
  k_fet_CaHPO4_norm = k_fet * y[11];
  k_fet_CaH2PO4_norm = k_fet * y[12]*f1;
  
  /* CPP degradation */
  CPP_degradation = k_c_CPP * y[13];
    
  /* CaProt formation */
  k_form_CaProt = k_f_CaProt * y[4] * (N_Prot * Prot_tot_p - y[16]);
  k_diss_CaProt = k_d_CaProt * y[16];
      
  /* Na and phosphate reaction in plasma */
  k_form_NaPO4 = (a * k_f_NaHPO4 + b * k_f_NaH2PO4) * Na * y[7];
  k_diss_NaPO4 = (c * k_d_NaHPO4 + d * k_d_NaH2PO4) * y[17];
        
  /* EGTA reaction */
  EGTA_form = k_on_egta * y[4] * y[20];
  EGTA_diss = k_off_egta * y[21];
    

  /* ODEs */
  ydot[0] = PTHg_synthesis_norm - PTHg_degradation_norm - PTHg_exocytosis_norm;
  ydot[1] = k_inject_PTH + PTHp_influx_norm - PTHp_degradation_norm;
  ydot[2] = k_inject_D3 + D3_synthesis_norm - D3_degradation_norm;
  ydot[3] = k_inject_FGF + FGF_synthesis_norm - FGF_degradation_norm;
  ydot[4] = 1 / Vp * (k_inject_Ca + Abs_intest_norm  + Resorption_norm/Ca_p_norm - 
                     Rapid_storage_Ca + Rapid_release_Ca - Excretion_norm) -
            k_form_CaProt + k_diss_CaProt - k_form_CaHPO4_norm*HPO4_norm + 
            k_diss_CaHPO4_norm * CaHPO4_norm / Ca_p_norm -
            k_form_CaH2PO4_norm / Ca_p_norm + 
            k_diss_CaH2PO4_norm * CaH2PO4_norm / Ca_p_norm -
            EGTA_form + EGTA_diss / Ca_p_norm;
  ydot[5] = Rapid_storage_Ca - Rapid_release_Ca - Accretion_norm - 
            k_form_CaHPO4f_norm * Ca_p_norm * HPO4_norm / CaHPO4_norm + 
            k_diss_CaHPO4f_norm - 
            k_form_CaH2PO4f_norm * Ca_p_norm*H2PO4_norm / CaH2PO4_norm + 
            k_diss_CaH2PO4f_norm;
  ydot[6] = 1 / Ca_b_norm * (Accretion_norm - Resorption_norm);
  ydot[7] = 1 / Vp * (k_inject_P + Abs_intest_P_norm + Resorption_P_norm / Pho_p_norm - 
                      Rapid_storage_P + Rapid_release_P - Excretion_P_norm - 
                      Plasma_intra_Flux_norm + Intra_plasma_Flux_norm / Pho_p_norm) - 
            k_form_CaHPO4_norm * Ca_p_norm + 
            k_diss_CaHPO4_norm * CaHPO4_norm / Pho_p_norm - 
            k_form_CaH2PO4_norm * Ca_p_norm + 
            k_diss_CaH2PO4_norm * CaH2PO4_norm / Pho_p_norm - 
            k_form_NaPO4 + k_diss_NaPO4;
  ydot[8] = Rapid_storage_P - Rapid_release_P - Accretion_P_norm - 
            k_form_CaHPO4f_norm * Ca_p_norm*HPO4_norm / CaHPO4_norm + 
            k_diss_CaHPO4f_norm - k_form_CaH2PO4f_norm * Ca_p_norm * 
            H2PO4_norm / CaH2PO4_norm + k_diss_CaH2PO4f_norm;
  ydot[9] = 1 / Pho_b_norm * (Accretion_P_norm - Resorption_P_norm); 
  ydot[10] = Plasma_intra_Flux_norm / Pho_c_norm - Intra_plasma_Flux_norm;
  ydot[11] = k_form_CaHPO4_norm * Ca_p_norm*HPO4_norm / CaHPO4_norm - 
             k_diss_CaHPO4_norm - k_fet_CaHPO4_norm;
  ydot[12] = k_form_CaH2PO4_norm * Ca_p_norm * H2PO4_norm / CaH2PO4_norm - 
             k_diss_CaH2PO4_norm - k_fet_CaH2PO4_norm;
  ydot[13] = k_fet_CaHPO4_norm + k_fet_CaH2PO4_norm - CPP_degradation;
  ydot[14] = k_form_CaHPO4f_norm * Ca_p_norm * HPO4_norm / CaHPO4_norm - 
             k_diss_CaHPO4f_norm;
  ydot[15] = k_form_CaH2PO4f_norm * Ca_p_norm * H2PO4_norm / CaH2PO4_norm - 
             k_diss_CaH2PO4f_norm; 
  ydot[16] = k_form_CaProt - k_diss_CaProt;
  ydot[17] = k_form_NaPO4 - k_diss_NaPO4;
  ydot[18] = ydot[4] + 1 * ydot[11] + 1 * ydot[12] + ydot[16] + 1 * ydot[13];
  ydot[19] = ydot[7] + 1 * ydot[11] + 1 * ydot[12] + ydot[17] + 1 * ydot[13];
  ydot[20] = 1 / Vp * k_inject_egta / EGTA_norm - EGTA_form + EGTA_diss / EGTA_norm;
  ydot[21] = EGTA_form / Ca_EGTA_norm - EGTA_diss;
  
  
  /* This variables will be used by the application
     Do not change their name nor their order.
  */
  yout[0] = Excretion_norm;
  yout[1] = Excretion_P_norm;
  yout[2] = Abs_intest_norm;
  yout[3] = Abs_intest_P_norm;
  yout[4] = Resorption_norm;
  yout[5] = Resorption_P_norm;
  yout[6] = Accretion_norm;
  yout[7] = Accretion_P_norm;
  yout[8] = Reabs_norm;
  yout[9] = Reabs_P_norm;
  yout[10] = Rapid_storage_Ca;
  yout[11] = Rapid_release_Ca;
  yout[12] = Rapid_storage_P;
  yout[13] = Rapid_release_P;
  yout[14] = Plasma_intra_Flux_norm;
  yout[15] = Intra_plasma_Flux_norm;
  yout[16] = PTHg_synthesis_norm;
  yout[17] = PTHg_synthesis_D3_norm;
  yout[18] = PTHg_synthesis_PO4_norm;
  yout[19] = F_Ca_norm;
  yout[20] = PTHg_degradation_norm;
  yout[21] = PTHg_exocytosis_norm;
  yout[22] = PTHp_degradation_norm;
  yout[23] = Reabs_PT_PTH_norm;
  yout[24] = Reabs_TAL_CaSR_norm;
  yout[25] = Reabs_TAL_PTH_norm;
  yout[26] = Reabs_DCT_PTH_norm;
  yout[27] = Reabs_DCT_D3_norm;
  yout[28] = Abs_intest_D3_norm;
  yout[29] = Resorption_PTH_norm;
  yout[30] = Resorption_D3_norm;
  yout[31] = Reabs_PT_PTH_P_norm;
  yout[32] = Reabs_PT_FGF_P_norm;
  
}

/* END file mymod.c */