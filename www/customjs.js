var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.onInputChange('alive_count', n++)
  }, 10000);
});

$(document).on('shiny:disconnected', function(event) {
  clearInterval(socket_timeout_interval)
});

$(document).keyup(function(event) {
  if ($("#number").is(":focus") && (event.key == "Enter")) {
    $("#convert").click();
  }
});

$(document).keyup(function(event) {
  if ($("#page").is(":focus") && (event.key == "Enter")) {
    $("#go").click();
  };
});

$(document).keyup(function(event) {
  if ($("#page2").is(":focus") && (event.key == "Enter")) {
    $("#go2").click();
  };
});

$("input[type=text]").focus(function() {
   $(this).select();
});