Shiny.addCustomMessageHandler('copy-code', function(message) {
  if (!navigator.clipboard) return;
  navigator.clipboard.writeText(message || '');
});
