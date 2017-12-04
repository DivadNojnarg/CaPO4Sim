#-------------------------------------------------------------------------
#  This code contains the controlsidebar of shinydashboard. It is the
#  sidebar available on the left. Parameters are put in this sidebar.
#  Sliders are handled via a conditionalPanel, but this can be disable
#
#  David Granjon, the Interface Group, Zurich
#  December 4th, 2017
#-------------------------------------------------------------------------

dashboardControlbar <- function(ctrlHTML = NULL) {
  
  if ( is.null(ctrlHTML) ) {

    HTML(paste0(
      '<!-- Control Sidebar -->
      <aside class="control-sidebar control-sidebar-dark">
      <!-- Create the tabs -->
      <ul class="nav nav-tabs nav-justified control-sidebar-tabs">
      <li class="active"><a href="#control-sidebar-home-tab" data-toggle="tab"><i class="fa fa-home"></i></a></li>
      <li><a href="#control-sidebar-settings-tab" data-toggle="tab"><i class="fa fa-gears"></i></a></li>
      </ul>
      <!-- Tab panes -->
      <div class="tab-content">
      <!-- Home tab content -->
      <div class="tab-pane active" id="control-sidebar-home-tab">
      <h3 class="control-sidebar-heading">Recent Activity</h3>
      <ul class="control-sidebar-menu">
      <li>
      <a href="javascript::;">
      <i class="menu-icon fa fa-code bg-purple"></i>
      <div class="menu-info">
      <h4 class="control-sidebar-subheading">ygdashboard development</h4>
      <p>TODO: complete control-sidebar module.</p>
      </div>
      </a>
      </li>
      </ul>

      <div data-display-if="input.current_edge_bis_id != &#39;null&#39; &amp;&amp; &#10;    input.current_edge_bis_id == 1 &amp;&amp; &#10;    input.current_node_bis_id == 11 ||&#10;    input.help" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="k_prod_PTHg" style="width:100%;">
            PTH synthesis rate constant (μmol/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetPTHsynthesis" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="k_prod_PTHg" data-min="0" data-max="100" data-from="1" data-step="1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.current_edge_bis_id != &#39;null&#39; &amp;&amp; &#10;    input.current_edge_bis_id == 3 &amp;&amp;&#10;    input.current_node_bis_id == 11" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="beta_exo_PTHg" style="width:100%;">
            Maximal PTH secretion from parathyroid 
                  glands (1/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetPTHexocytosis" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="beta_exo_PTHg" data-min="0" data-max="2" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.current_edge_bis_id != &#39;null&#39; &amp;&amp; &#10;    input.current_edge_bis_id == 4 &amp;&amp;&#10;    input.current_node_bis_id == 11" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="gamma_exo_PTHg" style="width:100%;">
            Maximal inhibition of PTH 
            secretion from parathyroid glands 
            by calcium (1/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetPTHexocytosisinhib" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="gamma_exo_PTHg" data-min="0" data-max="1" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="10" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.current_node_id == 13 || &#10;    input.current_node_id == 14 ||&#10;    input.current_node_id == 15" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="D3_inact" style="width:100%;">
            Plasma concentration of 25(OH)D (nM)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetD3inact" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="D3_inact" data-min="0" data-max="100" data-from="1" data-step="1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="k_deg_D3" style="width:100%;">
            Rate constant for vitamin D3 
            degradation (1/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetD3deg" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="k_deg_D3" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.current_node_id == 16" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="k_prod_FGF" style="width:100%;">
            Minimal rate of FGF23 synthesis (fM/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetFGFsynth" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="k_prod_FGF" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.current_node_id == 1" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="I_Ca" style="width:100%;">
            Calcium intake (μmol/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetCaintake" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="I_Ca" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="I_P" style="width:100%;">
            Phosphate intake (μmol/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetPintake" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="I_P" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.current_node_id == 3" data-ns-prefix="">
        <div class="form-group shiny-input-container">
            <label class="control-label" for="k_p_Ca" style="width:100%;">
              Ca transfer 
              from plasma to fast bone pool (1/min)
              <div class="pull-right">
                <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetkpCa" type="button">
                  <i class="fa fa-undo"></i>
      
                </button>
              </div>
            </label>
            <input class="js-range-slider" id="k_p_Ca" data-min="0" data-max="2" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5" data-data-type="number"/>
          </div>
          <div class="form-group shiny-input-container">
            <label class="control-label" for="k_f_Ca" style="width:100%;">
              Ca transfer 
              from fast bone pool to plasma (1/min)
              <div class="pull-right">
                <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetkfCa" type="button">
                  <i class="fa fa-undo"></i>
      
                </button>
              </div>
            </label>
            <input class="js-range-slider" id="k_f_Ca" data-min="0" data-max="2" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5" data-data-type="number"/>
          </div>
          <div class="form-group shiny-input-container">
            <label class="control-label" for="k_p_P" style="width:100%;">
              PO4 transfer from 
              plasma to fast bone pool (1/min)
                <div class="pull-right">
                  <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetkpP" type="button">
                    <i class="fa fa-undo"></i>
      
                </button>
              </div>
            </label>
            <input class="js-range-slider" id="k_p_P" data-min="0" data-max="2" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5" data-data-type="number"/>
          </div>
          <div class="form-group shiny-input-container">
            <label class="control-label" for="k_f_P" style="width:100%;">
              PO4 transfer from 
              fast bone pool to plasma (1/min)
              <div class="pull-right">
                <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetkfP" type="button">
                  <i class="fa fa-undo"></i>
      
                </button>
              </div>
            </label>
            <input class="js-range-slider" id="k_f_P" data-min="0" data-max="2" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5" data-data-type="number"/>
          </div>
      </div>


      <div data-display-if="input.current_node_id == 4" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="Lambda_ac_Ca" style="width:100%;">
            rate constant of Ca flux into bone (1/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetacCa" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="Lambda_ac_Ca" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="Lambda_res_min" style="width:100%;">
            Minimal resorption rate (μmol/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetresmin" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="Lambda_res_min" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="delta_res_max" style="width:100%;">
            Maximal resorption rate (μmol/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetresmax" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="delta_res_max" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.current_node_id == 8" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="k_pc" style="width:100%;">
            PO4 transfer from 
            plasma pool to intracellular (1/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetkpc" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="k_pc" data-min="0" data-max="2" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="k_cp" style="width:100%;">
            PO4 transfer from 
            intracellular pool to plasma (1/min)
            <div class="pull-right">
              <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="resetkcp" type="button">
                <i class="fa fa-undo"></i>
      
              </button>
            </div>
          </label>
          <input class="js-range-slider" id="k_cp" data-min="0" data-max="2" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.CaPO4group" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="k_fet" style="width:100%;">
            $$ k_f^{fetA} $$
            <div class="pull-right">
              <i class="fa fa-info" title="Fetuin-A binding to CaHPO4 and &#10;                                 CaH2PO4+ (1/min)" data-toggle="tooltip" data-placement="top"></i>
            </div>
          </label>
          <input class="js-range-slider" id="k_fet" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="k_c_CPP" style="width:100%;">
            $$ k_c^{CPP} $$
            <div class="pull-right">
              <i class="fa fa-info" title="CPP degradation rate constant (1/min)" data-toggle="tooltip" data-placement="top"></i>
            </div>
          </label>
          <input class="js-range-slider" id="k_c_CPP" data-min="0" data-max="100" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="0.1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="Na" style="width:100%;">
            $$ [Na^+]_p $$
            <div class="pull-right">
              <i class="fa fa-info" title="Sodium plasma concentration (mM)" data-toggle="tooltip" data-placement="top"></i>
            </div>
          </label>
          <input class="js-range-slider" id="Na" data-min="0" data-max="10" data-from="1" data-step="0.1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="Prot_tot_p" style="width:100%;">
            $$ [Prot^{tot}]_p $$
            <div class="pull-right">
              <i class="fa fa-info" title="Concentration of proteins in plasma (mM)" data-toggle="tooltip" data-placement="top"></i>
            </div>
          </label>
          <input class="js-range-slider" id="Prot_tot_p" data-min="0.25" data-max="2" data-from="1" data-step="0.1" data-grid="true" data-grid-num="8.75" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5.71428571428571" data-data-type="number"/>
        </div>
      </div>


      <div data-display-if="input.others" data-ns-prefix="">
        <div class="form-group shiny-input-container">
          <label class="control-label" for="Vp" style="width:100%;">
            $$ V_p $$
            <div class="pull-right">
              <i class="fa fa-info" title="Plasma volume (mL)" data-toggle="tooltip" data-placement="top"></i>
            </div>
          </label>
          <input class="js-range-slider" id="Vp" data-min="0.7" data-max="1.3" data-from="1" data-step="0.1" data-grid="true" data-grid-num="6" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="16.6666666666667" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container">
          <label class="control-label" for="GFR" style="width:100%;">
            $$ GFR $$
            <div class="pull-right">
              <i class="fa fa-info" title="glomerular filtration rate (mL/min)" data-toggle="tooltip" data-placement="top"></i>
            </div>
          </label>
          <input class="js-range-slider" id="GFR" data-min="0" data-max="1.5" data-from="1" data-step="0.1" data-grid="true" data-grid-num="7.5" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="6.66666666666667" data-data-type="number"/>
        </div>
      </div>

      <!-- /.control-sidebar-menu -->
      <h3 class="control-sidebar-heading">Tasks Progress</h3>
      <ul class="control-sidebar-menu">
      <li>
      <a href="javascript::;">
      <h4 class="control-sidebar-subheading">
      Custom Template Design
      <span class="label label-danger pull-right">70%</span>
      </h4>
      <div class="progress progress-xxs">
      <div class="progress-bar progress-bar-danger" style="width: 70%"></div>
      </div>
      </a>
      </li>
      </ul>
      <!-- /.control-sidebar-menu -->
      </div>
      <!-- /.tab-pane -->
      <!-- Stats tab content -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-settings-tab">
      <form method="post">
      <h3 class="control-sidebar-heading">General Settings</h3>
      <div class="form-group">
      <label class="control-sidebar-subheading">
      Report panel usage
      <input type="checkbox" class="pull-right" checked>
      </label>
      <p>
      Some information about this general settings option
      </p>
      </div>
      <!-- /.form-group -->
      </form>
      </div>
      <!-- /.tab-pane -->
      </div>
      </aside>
      <!-- /.control-sidebar -->
      <!-- Add the sidebar"s background. This div must be placed
      immediately after the control sidebar -->
      <div class="control-sidebar-bg"></div>
      '))
  
  } else {

    ctrlHTML

  }
}