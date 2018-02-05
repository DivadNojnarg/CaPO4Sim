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
      <h3 class="control-sidebar-heading">Parameter Input</h3>

      <button class="action-button bttn-fill bttn-md bttn-primary bttn-no-outline" id="play" type="button">
        <i class="fa fa-play"></i>
        Run
      </button>


      <div class="dropdown">
        <button class="btn btn-danger action-button dropdown-toggle " type="button" id="drop821252635" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
          <i class="fa fa-gear"></i>
          Options
          <span class="caret"></span>
        </button>
        <ul aria-labelledby="drop821252635" class="dropdown-menu  dropdown-shinyWidgets" id="dropdown-menu-drop821252635" style="width: 150px;">
          <li style="margin-left: 10px; margin-right: 10px;">
            <div class="form-group shiny-input-container" style="width: 50%;">
              <label for="tmax">Value of tmax:</label>
              <input id="tmax" type="number" class="form-control" value="500" min="0"/>
            </div>
          </li>
        </ul>
      </div>


      <div class="dropdown">
        <button class="btn btn-primary action-button dropdown-toggle " type="button" id="drop932063567" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
          <i class="fa fa-stethoscope"></i>
          Disease
          <span class="caret"></span>
        </button>
        <ul aria-labelledby="drop932063567" class="dropdown-menu  dropdown-shinyWidgets" id="dropdown-menu-drop932063567" style="width: 350px;">
          <li style="margin-left: 10px; margin-right: 10px;">
            <div class="form-group shiny-input-container" style="width: 100%">
              <label class="control-label" for="disease_selected">Select a disease :</label>
              <select id="disease_selected" multiple="multiple" class="multijs">
                <option value="primary-hyperparathyroidism">primary-hyperparathyroidism</option>
                <option value="hypoparathyroidism">hypoparathyroidism</option>
                <option value="vitamin D3 deficiency">vitamin D3 deficiency</option>
                <option value="pseudohypoparathyroidism">pseudohypoparathyroidism</option>
              </select>
              <script>$(\'#disease_selected\').multi({});</script>
            </div>
          </li>
        </ul>
      </div>


    <div class="dropdown">
  <button class="btn btn-primary action-button dropdown-toggle " type="button" id="drop692195743" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
      <i class="fa fa-medkit"></i>
      Treatment
      <span class="caret"></span>
      </button>
      <ul aria-labelledby="drop692195743" class="dropdown-menu  dropdown-shinyWidgets" id="dropdown-menu-drop692195743" style="width: 310px;">
      <li style="margin-left: 10px; margin-right: 10px;">
      <div id="dropdown_treatment">
      <div class="form-group shiny-input-container" style="width: 100%">
      <label class="control-label" for="treatment_selected">Select a treatment :</label>
      <select id="treatment_selected" multiple="multiple" class="multijs">
      <option value="parathyroid surgery">parathyroid surgery</option>
      <option value="vitamin D3 iv injection">vitamin D3 iv injection</option>
      <option value="Ca supplementation">Ca supplementation</option>
      <option value="Ca iv injection">Ca iv injection</option>
      <option value="PO4 supplementation">PO4 supplementation</option>
      <option value="PO4 iv injection">PO4 iv injection</option>
      <option value="cinacalcet">cinacalcet</option>
      </select>
      <script>$(\'#treatment_selected\').multi({});</script>
      </div>
        <div data-display-if="/Ca iv injection/.test(input.treatment_selected)" data-ns-prefix="">
        <div class="form-group shiny-input-container">
        <label class="control-label" for="Ca_inject" style="width:100%;">
        $$ k_{inject}^{Ca} $$
        <div class="pull-right">
        <i class="fa fa-info" title="Rate of injection of calcium in plasma (μmol/min)" data-toggle="tooltip" data-placement="top"></i>
        </div>
        </label>
        <input class="js-range-slider" id="Ca_inject" data-min="0" data-max="0.002" data-from="0.001" data-step="0.0001" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="5" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_start_Cainject">Time when begins the Ca iv injection:</label>
        <input id="t_start_Cainject" type="number" class="form-control" value="0" min="0"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_stop_Cainject">Time,when stops the Ca iv injection:</label>
        <input id="t_stop_Cainject" type="number" class="form-control" value="100" min="0"/>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newCaiv" type="button">
        <i class="fa fa-plus"></i>
        </button>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldCaiv" type="button">
        <i class="fa fa-minus"></i>
        </button>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="delete_Caiv_id">Event to remove?</label>
        <input id="delete_Caiv_id" type="number" class="form-control" value="1" min="1"/>
        </div>
        </div>
        </div>
        <div data-display-if="/Ca supplementation/.test(input.treatment_selected)" data-ns-prefix="">
        <div class="form-group shiny-input-container">
        <label class="control-label" for="Ca_food" style="width:100%;">
        Ca intake
        <div class="pull-right">
        <i class="fa fa-info" title="Calcium intake (μmol/min)" data-toggle="tooltip" data-placement="top"></i>
        </div>
        </label>
        <input class="js-range-slider" id="Ca_food" data-min="0" data-max="0.008" data-from="0.0022" data-step="0.0001" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1.25" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_start_Caintake">Time when begins the Ca supplementation:</label>
        <input id="t_start_Caintake" type="number" class="form-control" value="0" min="0"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_stop_Caintake">Time when stops the Ca supplementation:</label>
        <input id="t_stop_Caintake" type="number" class="form-control" value="100" min="0"/>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newCaintake" type="button">
        <i class="fa fa-plus"></i>
        </button>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldCaintake" type="button">
        <i class="fa fa-minus"></i>
        </button>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="delete_Caintake_id">Event to remove?</label>
        <input id="delete_Caintake_id" type="number" class="form-control" value="1" min="1"/>
        </div>
        </div>
        </div>
        <div data-display-if="/vitamin D3 iv injection/.test(input.treatment_selected)" data-ns-prefix="">
        <div class="form-group shiny-input-container">
        <label class="control-label" for="D3_inject" style="width:100%;">
        D3 injection
        <div class="pull-right">
        <i class="fa fa-info" title="D3 injection (pmol/min)" data-toggle="tooltip" data-placement="top"></i>
        </div>
        </label>
        <input class="js-range-slider" id="D3_inject" data-min="0" data-max="0.1" data-from="0.001" data-step="0.001" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_start_D3inject">Time when begins the D3 iv injection:</label>
        <input id="t_start_D3inject" type="number" class="form-control" value="0" min="0"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_stop_D3inject">Time when stops the D3 iv injection:</label>
        <input id="t_stop_D3inject" type="number" class="form-control" value="100" min="0"/>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newD3iv" type="button">
        <i class="fa fa-plus"></i>
        </button>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldD3iv" type="button">
        <i class="fa fa-minus"></i>
        </button>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="delete_D3iv_id">Event to remove?</label>
        <input id="delete_D3iv_id" type="number" class="form-control" value="1" min="1"/>
        </div>
        </div>
        </div>
        <div data-display-if="/PO4 iv injection/.test(input.treatment_selected)" data-ns-prefix="">
        <div class="form-group shiny-input-container">
        <label class="control-label" for="P_inject" style="width:100%;">
        PO4 injection
        <div class="pull-right">
        <i class="fa fa-info" title="PO4 injection (μmol/min)" data-toggle="tooltip" data-placement="top"></i>
        </div>
        </label>
        <input class="js-range-slider" id="P_inject" data-min="0" data-max="0.01" data-from="0.001" data-step="0.0001" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_start_Pinject">Time when begins the PO4 iv injection:</label>
        <input id="t_start_Pinject" type="number" class="form-control" value="0" min="0"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_stop_Pinject">Time when stops the PO4 iv injection:</label>
        <input id="t_stop_Pinject" type="number" class="form-control" value="100" min="0"/>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newPiv" type="button">
        <i class="fa fa-plus"></i>
        </button>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldPiv" type="button">
        <i class="fa fa-minus"></i>
        </button>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="delete_Piv_id">Event to remove?</label>
        <input id="delete_Piv_id" type="number" class="form-control" value="1" min="1"/>
        </div>
        </div>
        </div>
        <div data-display-if="/PO4 supplementation/.test(input.treatment_selected)" data-ns-prefix="">
        <div class="form-group shiny-input-container">
        <label class="control-label" for="P_food" style="width:100%;">
        PO4 intake
        <div class="pull-right">
        <i class="fa fa-info" title="Phosphate intake (μmol/min)" data-toggle="tooltip" data-placement="top"></i>
        </div>
        </label>
        <input class="js-range-slider" id="P_food" data-min="0" data-max="0.01" data-from="0.00155" data-step="0.0001" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="1" data-data-type="number"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_start_Pintake">Time when begins the Ca supplementation:</label>
        <input id="t_start_Pintake" type="number" class="form-control" value="0" min="0"/>
        </div>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="t_stop_Pintake">Time when stops the Ca supplementation:</label>
        <input id="t_stop_Pintake" type="number" class="form-control" value="100" min="0"/>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newPintake" type="button">
        <i class="fa fa-plus"></i>
        </button>
        </div>
        <div class="col-sm-6" align="center">
        <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldPintake" type="button">
        <i class="fa fa-minus"></i>
        </button>
        <div class="form-group shiny-input-container" style="width: 50%;">
        <label for="delete_Pintake_id">Event to remove?</label>
        <input id="delete_Pintake_id" type="number" class="form-control" value="1" min="1"/>
        </div>
        </div>
        </div>
        </div>
        </li>
        </ul>
        </div>

      
      <!-- /.control-sidebar-menu -->
      </div>
      <!-- /.tab-pane -->
      <!-- Stats tab content -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-settings-tab">
      <h3 class="control-sidebar-heading">Other Options</h3>
      
      <div class="form-group shiny-input-container">
      <label class="control-label" for="skin">Select a skin:</label>
      <div>
      <select id="skin"><option value="blue">blue</option>
      <option value="black" selected>black</option>
      <option value="purple">purple</option>
      <option value="green">green</option>
      <option value="red">red</option>
      <option value="yellow">yellow</option>
      </select>
      <script type="application/json" data-for="skin" data-nonempty="">{}</script>
      </div>
      </div>




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