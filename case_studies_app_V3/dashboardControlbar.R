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
      <li class="active"><a href="#control-sidebar-home-tab" data-toggle="tab"><i class="fa fa-sliders"></i></a></li>
      <li><a href="#control-sidebar-parms-tab" data-toggle="tab"><i class="fa fa-map-o"></i></a></li>
      <li><a href="#control-sidebar-settings-tab" data-toggle="tab"><i class="fa fa-paint-brush"></i></a></li>
      
      </ul>
      </div>
      
      <!-- Tab panes -->
      <div class="tab-content">
      <!-- Home tab content -->
      <div class="tab-pane active" id="control-sidebar-home-tab">
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
      <span>Pi IV injection</span>
      </label>
      </div>
      </div>
      </div>


      <div class="form-group shiny-input-container">
      <div class="pretty p-default p-thick p-pulse">
      <input id="run_PO4_gav" type="checkbox"/>
      <div class="state p-primary">
      <label>
      <span>Pi gavage</span>
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