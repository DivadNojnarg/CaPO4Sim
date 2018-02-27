dashboardControlbar <- function(ctrlHTML = NULL) {
  
  if ( is.null(ctrlHTML) ) {
    
    HTML(paste0(
      '<!-- Control Sidebar -->
      <div id="sidebar_bis">
      <aside class="control-sidebar control-sidebar-dark">
      <!-- Create the tabs -->
      <div data-step="9" data-intro="&lt;li&gt; &lt;i class=&quot;fa fa-sliders fa-2x&quot;&gt;&lt;/i&gt; 
      is where you handle the &lt;font color=&quot;#FF0000&quot;&gt;&lt;b&gt; parameters &lt;/b&gt;&lt;/font&gt; of this application &#10; 
      such as changing the background, display regulation&#10; mechanisms or not... &lt;/li&gt; &lt;li&gt; &lt;i class=&quot;fa fa-map-o fa-2x&quot;&gt;&lt;/i&gt; 
      is dedicated to the &lt;font color=&quot;#0000FF&quot;&gt;&lt;b&gt; educational content &lt;/b&gt;&lt;/font&gt; of the app. Go here if you&#10;
      want to select some case studies. &lt;/li&gt; &lt;li&gt; &lt;i class=&quot;fa fa-paint-brush fa-2x&quot;&gt;&lt;/i&gt; Here you can change &#10;  
      the global &lt;font color=&quot;#FF0000&quot;&gt;&lt;b&gt; theme &lt;/b&gt;&lt;/font&gt; of the dashboard.">
      <ul class="nav nav-tabs nav-justified control-sidebar-tabs">
      <li class="active"><a href="#control-sidebar-parms-tab" data-toggle="tab"><i class="fa fa-sliders"></i></a></li>
      <li><a href="#control-sidebar-casestudies-tab" data-toggle="tab"><i class="fa fa-map-o"></i></a></li>
      <li><a href="#control-sidebar-solver-tab" data-toggle="tab"><i class="fa fa-gears"></i></a></li>
      <li><a href="#control-sidebar-custom-tab" data-toggle="tab"><i class="fa fa-paint-brush"></i></a></li>
      
      </ul>
      </div>
      
      <!-- Tab panes -->
      <div class="tab-content">
      <!-- Home tab content -->
      <div class="tab-pane active" id="control-sidebar-parms-tab">
      <h3 class="control-sidebar-heading">CaPO4 Network Options</h3>
      
      <div data-step="10" data-intro="Choose your &lt;font color=&quot;#FF0000&quot;&gt;&lt;b&gt; background &lt;/b&gt;&lt;/font&gt; (Rat by default).">
      <div id="background_choice" class="form-group shiny-input-checkboxgroup shiny-input-container shiny-input-container-inline">
      <label class="control-label" for="background_choice">Background</label>
      <div class="shiny-options-group">
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="background_choice" value="rat" checked = "checked"/>
      <div class="state p-primary">
      <label>
      <span>rat</span>
      </label>
      </div>
      </div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="background_choice" value="human"/>
      <div class="state p-primary">
      <label>
      <span>human</span>
      </label>
      </div>
      </div>
      </div>
      </div>
      </div>
      
      <hr/>
      
      
      <div data-step="11" data-intro="&lt;font color=&quot;#FF0000&quot;&gt;&lt;b&gt; Filter &lt;/b&gt;&lt;/font&gt; 
      what to display in the network (by default, nothing is enabled). 
      Be careful that you activated/disactivated hormones and/or organs before. ">
      <div id="network_Ca_choice" class="form-group shiny-input-checkboxgroup shiny-input-container shiny-input-container-inline">
      <label class="control-label" for="network_Ca_choice">Choose your network</label>
      <div class="shiny-options-group">
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="network_Ca_choice" value="Ca"/>
      <div class="state p-primary">
      <label>
      <span>Ca</span>
      </label>
      </div>
      </div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="network_Ca_choice" value="PO4"/>
      <div class="state p-primary">
      <label>
      <span>Pi</span>
      </label>
      </div>
      </div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="network_Ca_choice" value="PTH"/>
      <div class="state p-primary">
      <label>
      <span>PTH</span>
      </label>
      </div>
      </div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="network_Ca_choice" value="D3"/>
      <div class="state p-primary">
      <label>
      <span>D3</span>
      </label>
      </div>
      </div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="network_Ca_choice" value="FGF23"/>
      <div class="state p-primary">
      <label>
      <span>FGF23</span>
      </label>
      </div>
      </div>
      </div>
      </div>
      </div>
      
      <hr/>
      
      <div data-step="12" data-intro="&lt;font color=&quot;#FF0000&quot;&gt;&lt;b&gt; Enable/disable &lt;/b&gt;&lt;/font&gt; 
      organs and/or hormonal regulations. By default, &#10; hormonal regulations are &lt;font color=&quot;#FF0000&quot;&gt;&lt;b&gt; 
      not activated. &lt;/b&gt;&lt;/font&gt;">
      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-switch p-bigger p-slim">
      <input id="network_hormonal_choice" type="checkbox"/>
      <div class="state p-success">
      <label>
      <span>Regulations?</span>
      </label>
      </div>
      </div>
      </div>
      
      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-switch p-bigger p-slim">
      <input id="network_organ_choice" type="checkbox" checked="checked"/>
      <div class="state p-success">
      <label>
      <span>Organs?</span>
      </label>
      </div>
      </div>
      </div>
      </div
      
      <hr/>
      
      <h4>Nodes size</h4>
      <div data-step="13" data-intro="&lt;font color=&quot;#FF0000&quot;&gt;&lt;b&gt; 
      Control &lt;/b&gt;&lt;/font&gt;  the size of nodes.">
      <div class="row">
      <div class="col-sm-6">
      <div id="size_nodes_organs" class="shiny-html-output"></div>
      </div>
      <div class="col-sm-6">
      <div id="size_nodes_hormones" class="shiny-html-output"></div>
      </div>
      </div>
      </div>
      
      <h4>Arrow width</h4>
      <div data-step="14" data-intro="&lt;font color=&quot;#FF0000&quot;&gt;&lt;b&gt; 
      Control &lt;/b&gt;&lt;/font&gt;  the size of arrows.">
      <div class="row">
      <div class="col-sm-6">
      <div id="width_arrows_organs" class="shiny-html-output"></div>
      </div>
      <div class="col-sm-6">
      <div id="width_arrows_hormones" class="shiny-html-output"></div>
      </div>
      </div>
      </div>
      
      
      <!-- /.control-sidebar-menu -->
      </div>
      <!-- /.tab-pane -->
      
      
      <!-- Case Studies -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-casestudies-tab">
      <h3 class="control-sidebar-heading">Case Studies</h3> 
      
      
      <i class="fa fa-heartbeat"></i><h6>Steady-State Simulations</h6>
      
      <div id="disease_selected" style="width: 100%;" class="form-group shiny-input-checkboxgroup shiny-input-container">
      <label class="control-label" for="disease_selected">Select a disease:</label>
      <div class="shiny-options-group">
      <div style="height:7px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="disease_selected" value="primary-hyperparathyroidism"/>
      <div class="state p-info">
      <label>
      <span>Hyperparathyroidism I</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="disease_selected" value="hypoparathyroidism"/>
      <div class="state p-info">
      <label>
      <span>Hypoparathyroidism</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="disease_selected" value="vitamin D3 deficiency"/>
      <div class="state p-info">
      <label>
      <span>vitamin D3 deficiency</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      </div>
      </div>
      
      <hr/>
      
      
      <i class="fa fa-user-md"></i><h6>Treatments</h6>
      
      
      <div id="dropdown_treatment">
      <div id="treatment_selected" style="width: 100%;" class="form-group shiny-input-checkboxgroup shiny-input-container">
      <label class="control-label" for="treatment_selected">Select a treatment:</label>
      <div class="shiny-options-group">
      <div style="height:7px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="treatment_selected" value="parathyroid surgery"/>
      <div class="state p-info">
      <label>
      <span>parathyroid surgery</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="treatment_selected" value="vitamin D3 iv injection"/>
      <div class="state p-info">
      <label>
      <span>vitamin D3 iv injection</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="treatment_selected" value="Ca supplementation"/>
      <div class="state p-info">
      <label>
      <span>Ca supplementation</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="treatment_selected" value="Ca iv injection"/>
      <div class="state p-info">
      <label>
      <span>Ca iv injection</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="treatment_selected" value="PO4 supplementation"/>
      <div class="state p-info">
      <label>
      <span>PO4 supplementation</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="treatment_selected" value="PO4 iv injection"/>
      <div class="state p-info">
      <label>
      <span>PO4 iv injection</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      <div class="pretty p-default p-thick p-pulse">
      <input type="checkbox" name="treatment_selected" value="cinacalcet"/>
      <div class="state p-info">
      <label>
      <span>cinacalcet</span>
      </label>
      </div>
      </div>
      <div style="height:3px;"></div>
      </div>
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
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_start_Cainject">Time when begins the Ca iv injection:</label>
      <input id="t_start_Cainject" type="number" class="form-control" value="0" min="0"/>
      </div>
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_stop_Cainject">Time,when stops the Ca iv injection:</label>
      <input id="t_stop_Cainject" type="number" class="form-control" value="100" min="0"/>
      </div>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newCaiv" type="button">
      <i class="fa fa-plus"></i>
      </button>
      </div>
      <br/>
      <br/>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldCaiv" type="button">
      <i class="fa fa-minus"></i>
      </button>
      </div>
      <div class="col-sm-12" align="center">
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="delete_Caiv_id">Event to remove:</label>
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
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_start_Caintake">Time when begins the Ca supplementation:</label>
      <input id="t_start_Caintake" type="number" class="form-control" value="0" min="0"/>
      </div>
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_stop_Caintake">Time when stops the Ca supplementation:</label>
      <input id="t_stop_Caintake" type="number" class="form-control" value="100" min="0"/>
      </div>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newCaintake" type="button">
      <i class="fa fa-plus"></i>
      </button>
      </div>
      <br/>
      <br/>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldCaintake" type="button">
      <i class="fa fa-minus"></i>
      </button>
      </div>
      <div class="col-sm-12" align="center">
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="delete_oldCaintake_id">Event to remove:</label>
      <input id="delete_oldCaintake_id" type="number" class="form-control" value="1" min="1"/>
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
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_start_D3inject">Time when begins the D3 iv injection:</label>
      <input id="t_start_D3inject" type="number" class="form-control" value="0" min="0"/>
      </div>
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_stop_D3inject">Time when stops the D3 iv injection:</label>
      <input id="t_stop_D3inject" type="number" class="form-control" value="100" min="0"/>
      </div>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newD3iv" type="button">
      <i class="fa fa-plus"></i>
      </button>
      </div>
      <br/>
      <br/>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldD3iv" type="button">
      <i class="fa fa-minus"></i>
      </button>
      </div>
      <div class="col-sm-12" align="center">
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="delete_oldD3iv_id">Event to remove:</label>
      <input id="delete_oldD3iv_id" type="number" class="form-control" value="1" min="1"/>
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
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_start_Pinject">Time when begins the PO4 iv injection:</label>
      <input id="t_start_Pinject" type="number" class="form-control" value="0" min="0"/>
      </div>
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_stop_Pinject">Time when stops the PO4 iv injection:</label>
      <input id="t_stop_Pinject" type="number" class="form-control" value="100" min="0"/>
      </div>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newPiv" type="button">
      <i class="fa fa-plus"></i>
      </button>
      </div>
      <br/>
      <br/>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldPiv" type="button">
      <i class="fa fa-minus"></i>
      </button>
      </div>
      <div class="col-sm-12" align="center">
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="delete_oldPiv_id">Event to remove:</label>
      <input id="delete_oldPiv_id" type="number" class="form-control" value="1" min="1"/>
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
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_start_Pintake">Time when begins the Ca supplementation:</label>
      <input id="t_start_Pintake" type="number" class="form-control" value="0" min="0"/>
      </div>
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="t_stop_Pintake">Time when stops the Ca supplementation:</label>
      <input id="t_stop_Pintake" type="number" class="form-control" value="100" min="0"/>
      </div>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-primary bttn-no-outline" id="add_newPintake" type="button">
      <i class="fa fa-plus"></i>
      </button>
      </div>
      <br/>
      <br/>
      <div class="col-sm-12" align="center">
      <button class="action-button bttn-material-circle bttn-md bttn-danger bttn-no-outline" id="delete_oldPintake" type="button">
      <i class="fa fa-minus"></i>
      </button>
      </div>
      <div class="col-sm-12" align="center">
      <div class="form-group shiny-input-container" style="width: 100%;">
      <label for="delete_oldPintake_id">Event to remove:</label>
      <input id="delete_oldPintake_id" type="number" class="form-control" value="1" min="1"/>
      </div>
      </div>
      </div>
      </div>
      
      <hr/>
      
      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-switch p-bigger p-slim">
      <input id="notif2_switch" type="checkbox" checked="checked"/>
      <div class="state p-success">
      <label>
      <span>Notifications?</span>
      </label>
      </div>
      </div>
      </div>
      
      
      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-switch p-bigger p-slim">
      <input id="modal_switch" type="checkbox" checked="checked"/>
      <div class="state p-success">
      <label>
      <span>Description?</span>
      </label>
      </div>
      </div>
      </div>
      
      
      
      </div>
      <!-- /.Case Studies -->
      
      
      
      <!-- Solver Options -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-solver-tab">
      <h3 class="control-sidebar-heading">Solver Options</h3>
      
      <div class="form-group shiny-input-container" style="width: 100%;">
        <label for="tmax">Maximum simulated time:</label>
        <input id="tmax" type="number" class="form-control" value="500" min="0"/>
      </div>

      <hr/>
      
<div class="form-group shiny-input-container" style="width: 90%;">
  <label class="control-label" for="t_now" style="width:100%;">
    Time after simulation:
    <div class="pull-right">
      <button class="action-button bttn-unite bttn-xs bttn-danger bttn-no-outline" id="reset_t_now" type="button">
        <i class="fa fa-undo"></i>
        
      </button>
    </div>
  </label>
  <input class="js-range-slider" id="t_now" data-min="1" data-max="500" data-from="500" data-step="1" data-grid="true" data-grid-num="9.98" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="0.200400801603206" data-data-type="number"/>
</div>
      
      </div>
      <!-- /.Solver Options -->


      
      <!-- Interface customization -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-custom-tab">
      <h3 class="control-sidebar-heading">Other Options</h3>
      
      <!-- Select input -->
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
      <!-- /.interface customization -->





      </div>
      </aside>
      <!-- /.control-sidebar -->
      <!-- Add the sidebar"s background. This div must be placed
      immediately after the control sidebar -->
      <div class="control-sidebar-bg"></div>
      </div>
      '))
    
  } else {
    
    ctrlHTML
    
  }
}