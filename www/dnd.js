(function () {
  function send(id, payload) {
    payload.nonce = Date.now();
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, payload, { priority: "event" });
    }
  }

  window.studioDrop = function (action, name) {
    send("ws_drop", { action: action || "", name: name || "", index: 999 });
  };

  window.studioDelete = function (index) {
    send("ws_delete", { index: index });
  };

  window.studioSelect = function (kind, extra) {
    var payload = Object.assign({ kind: kind || "none" }, extra || {});
    send("ws_select", payload);
  };

  window.studioAssign = function (name) {
    send("ws_assign", { name: name || "" });
  };

  document.addEventListener("keydown", function (e) {
    if (e.key !== "Delete" && e.key !== "Backspace") return;
    if (e.target && (e.target.isContentEditable || /INPUT|TEXTAREA|SELECT/.test(e.target.tagName))) return;
    var tok = document.querySelector("#ws-canvas .tok.is-selected");
    if (!tok) return;
    e.preventDefault();
    studioDelete(tok.getAttribute("data-i"));
  });
})();
