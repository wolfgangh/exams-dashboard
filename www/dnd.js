(function () {
  function loadScript(src, cb) {
    if (window.Sortable) { cb(); return; }
    var s = document.createElement("script");
    s.src = src;
    s.onload = cb;
    document.head.appendChild(s);
  }

  function destroySortable(el) {
    if (el && el._sortable) {
      el._sortable.destroy();
      el._sortable = null;
    }
  }

  function send(id, payload) {
    payload.nonce = Date.now();
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, payload, { priority: "event" });
    }
  }

  function initPalette(el) {
    destroySortable(el);
    el._sortable = Sortable.create(el, {
      group: { name: "ws", pull: "clone", put: false },
      sort: false,
      animation: 120
    });
  }

  function initCanvas(el) {
    destroySortable(el);
    el._sortable = Sortable.create(el, {
      group: { name: "ws", pull: true, put: true },
      animation: 120,
      draggable: ".tok-var, .tok-gap, .tok-math, .pal-item",
      onAdd: function (evt) {
        var item = evt.item;
        var payload = {
          action: item.getAttribute("data-action") || "place-var",
          name: item.getAttribute("data-name") || "",
          index: evt.newIndex + 1
        };
        item.parentNode && item.parentNode.removeChild(item);
        send("ws_drop", payload);
      },
      onEnd: function (evt) {
        if (evt.from !== evt.to) return;
        var kids = Array.prototype.slice.call(evt.to.querySelectorAll(":scope > .tok"));
        send("ws_reorder", {
          order: kids.map(function (el) {
            return {
              kind: el.getAttribute("data-kind"),
              name: el.getAttribute("data-name"),
              id: el.getAttribute("data-id"),
              text: el.getAttribute("data-text") || el.textContent,
              i: el.getAttribute("data-i")
            };
          })
        });
      }
    });

    el.addEventListener("click", function (e) {
      var tok = e.target.closest(".tok");
      if (!tok) {
        send("ws_select", { kind: "none" });
        return;
      }
      send("ws_select", {
        kind: tok.getAttribute("data-kind"),
        name: tok.getAttribute("data-name"),
        id: tok.getAttribute("data-id"),
        index: tok.getAttribute("data-i"),
        text: tok.getAttribute("data-text") || tok.textContent
      });
    });

    el.addEventListener("focusout", function (e) {
      var tok = e.target.closest(".tok-text");
      if (!tok) return;
      send("ws_text", {
        index: tok.getAttribute("data-i"),
        text: tok.textContent
      });
    });
  }

  function initAssign(el) {
    destroySortable(el);
    el._sortable = Sortable.create(el, {
      group: { name: "ws", pull: false, put: true },
      animation: 120,
      onAdd: function (evt) {
        var item = evt.item;
        var name = item.getAttribute("data-name") || "";
        item.parentNode && item.parentNode.removeChild(item);
        send("ws_assign", { name: name });
      }
    });
  }

  function initWorkspace() {
    loadScript("https://cdn.jsdelivr.net/npm/sortablejs@1.15.6/Sortable.min.js", function () {
      var pal = document.getElementById("ws-palette");
      var canvas = document.getElementById("ws-canvas");
      var assign = document.getElementById("ws-assign");
      if (pal) initPalette(pal);
      if (canvas) initCanvas(canvas);
      if (assign) initAssign(assign);
    });
  }

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("ws_ready", function () { initWorkspace(); });
  }
  document.addEventListener("DOMContentLoaded", initWorkspace);
  $(document).on("shiny:value", function (e) {
    if (e.name === "canvas_ui" || e.name === "palette_ui" || e.name === "inspector_ui") {
      setTimeout(initWorkspace, 30);
    }
  });
})();
