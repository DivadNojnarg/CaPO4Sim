$(document).on('shiny:connected', function(event) {
  var isMobile = /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
  Shiny.onInputChange('isMobile', isMobile);
});