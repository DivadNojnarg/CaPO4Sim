$(document).ready(function() {
  window.onresize = function() {
    var screenInfo = {width: $(window).width(), height: $(window).height()};
    Shiny.onInputChange("screenSize", screenInfo);
  };
});
