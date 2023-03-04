var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.setInputValue('keepalive', n++);
  }, 1000*55);
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