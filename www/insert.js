Shiny.addCustomMessageHandler("insertAtCursor", function(x) {
  var el = document.getElementById(x.id);
  if (!el) return;
  el.focus();
  var start = typeof el.selectionStart === "number" ? el.selectionStart : el.value.length;
  var end = typeof el.selectionEnd === "number" ? el.selectionEnd : start;
  var before = el.value.slice(0, start);
  var after = el.value.slice(end);
  el.value = before + x.text + after;
  var pos = start + x.text.length;
  if (el.setSelectionRange) {
    el.setSelectionRange(pos, pos);
  }
  el.dispatchEvent(new Event("input", { bubbles: true }));
  $(el).trigger("change");
});
