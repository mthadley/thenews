var app = Elm.Main.fullscreen();

app.ports.setTitle.subscribe(function(title) {
  document.title = title;
});
