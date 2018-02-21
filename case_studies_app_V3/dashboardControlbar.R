dashboardControlbar <- function(ctrlHTML = NULL) {
  
  if ( is.null(ctrlHTML) ) {
    
    HTML(paste0(
      '<!-- Control Sidebar -->
      <div id="sidebar_bis">
      <aside class="control-sidebar control-sidebar-dark">
      <!-- Create the tabs -->
      <ul class="nav nav-tabs nav-justified control-sidebar-tabs">
      <li class="active"><a href="#control-sidebar-home-tab" data-toggle="tab"><i class="fa fa-sliders"></i></a></li>
      <li><a href="#control-sidebar-parms-tab" data-toggle="tab"><i class="fa fa-map-o"></i></a></li>
      <li><a href="#control-sidebar-settings-tab" data-toggle="tab"><i class="fa fa-paint-brush"></i></a></li>
      
      </ul>
      
      <!-- Tab panes -->
      <div class="tab-content">
      <!-- Home tab content -->
      <div class="tab-pane active" id="control-sidebar-home-tab">
      <h3 class="control-sidebar-heading">CaPO4 Network Options</h3>
      
      <div data-step="10" data-intro="test">
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
          <span>PO4</span>
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

      <hr/>

      
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

      <hr/>
      
      <h4>Nodes size</h4>
      <div id="knob_size_nodes" class="shiny-html-output"></div>

      <h4>Arrow width</h4>
      <div id="knob_width_arrows" class="shiny-html-output"></div>

      
      <!-- /.control-sidebar-menu -->
      </div>
      <!-- /.tab-pane -->


      <!-- Case Studies -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-parms-tab">
      <h3 class="control-sidebar-heading">Case Studies</h3> 
      
      
      <h6>Steady-State Simulations</h6>

      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-thick p-pulse">
      <input id="run_php1" type="checkbox"/>
      <div class="state p-primary">
      <label>
      <span>Primary hyperparathyroidism</span>
      </label>
      </div>
      </div>
      </div>


      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-thick p-pulse">
      <input id="run_hypopara" type="checkbox"/>
      <div class="state p-primary">
      <label>
      <span>Hypoparathyroidism</span>
      </label>
      </div>
      </div>
      </div>

      
      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-thick p-pulse">
      <input id="run_hypoD3" type="checkbox"/>
      <div class="state p-primary">
      <label>
      <span>25(OH)D deficiency</span>
      </label>
      </div>
      </div>
      </div>

      <hr/>


      <h6>Dynamic Simulations</h6>
      
      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-thick p-pulse">
      <input id="run_Ca_inject" type="checkbox"/>
      <div class="state p-primary">
      <label>
      <span>Ca/EGTA IV injection</span>
      </label>
      </div>
      </div>
      </div>

      
      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-thick p-pulse">
      <input id="run_PO4_inject" type="checkbox"/>
      <div class="state p-primary">
      <label>
      <span>PO4 IV injection</span>
      </label>
      </div>
      </div>
      </div>


      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-thick p-pulse">
      <input id="run_PO4_gav" type="checkbox"/>
      <div class="state p-primary">
      <label>
      <span>PO4 gavage</span>
      </label>
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
      

      
      <!-- Interface customization -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-settings-tab">
      <h3 class="control-sidebar-heading">Other Options</h3>
      
      <!-- Select input -->
      <div data-step="7" data-intro="Here you can change the global &lt;b&gt;theme&lt;/b&gt; &#10;of the dashboard" data-position="left">
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