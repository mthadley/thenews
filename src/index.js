require('./index.html');
require('./styles/main.css');

var Elm = require('./Main');

var app = Elm.Main.fullscreen();

app.ports.setTitle.subscribe(function(title) {
  document.title = title;
});
