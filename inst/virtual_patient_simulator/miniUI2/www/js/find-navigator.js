$(document).on('shiny:connected', function(event) {
  var windowHeight = $( window ).height();
  var windowWidth = $( window ).width();
  Shiny.onInputChange('height', windowHeight);
  Shiny.onInputChange('width', windowWidth);
});
