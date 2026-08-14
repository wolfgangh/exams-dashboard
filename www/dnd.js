(function () {
  var inited = { palette: null, canvas: null, assign: null };

  function send(id, payload) {
    payload.nonce = Date.now();
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, payload, { priority: "event" });
    }
  }

  function destroy(key) {
    if (inited[key] && inited[key].s) {
      try { inited[key].s.destroy(); } catch (e) {}
    }
    inited[key] = null;
  }

  var fallbackOpts = {
    animation: 120,
    forceFallback: true,
    fallbackOnBody: true,
    fallbackTolerance: 3,
    swapThreshold: 0.65
  };

  function initPalette(el) {
    if (!window.Sortable || !el) return;
    if (inited.palette && inited.palette.el === el) return;
    destroy("palette");
    var s = Sortable.create(el, Object.assign({}, fallbackOpts, {
      group: { name: "ws", pull: "clone", put: false },
      sort: false,
      draggable: ".pal-item"
    }));
    inited.palette = { el: el, s: s };
    if (!el.dataset.clickBound) {
      el.dataset.clickBound = "1";
      el.addEventListener("click", function (e) {
        var item = e.target.closest(".pal-item");
        if (!item || s.el && item.closest(".sortable-fallback")) return;
        send("ws_drop", {
          action: item.getAttribute("data-action") || "place-var",
          name: item.getAttribute("data-name") || "",
          index: 999
        });
      });
    }
  }

  function initCanvas(el) {
    if (!window.Sortable || !el) return;
    if (inited.canvas && inited.canvas.el === el) return;
    destroy("canvas");
    var s = Sortable.create(el, Object.assign({}, fallbackOpts, {
      group: { name: "ws", pull: true, put: true },
      draggable: ".tok-var, .tok-gap, .tok-math, .pal-item",
      onAdd: function (evt) {
        var item = evt.item;
        send("ws_drop", {
          action: item.getAttribute("data-action") || "place-var",
          name: item.getAttribute("data-name") || "",
          index: evt.newIndex + 1
        });
        if (item.parentNode) item.parentNode.removeChild(item);
      },
      onEnd: function (evt) {
        if (evt.from !== evt.to) return;
        var kids = Array.prototype.slice.call(evt.to.querySelectorAll(":scope > .tok"));
        if (!kids.length) return;
        send("ws_reorder", {
          order: kids.map(function (node) {
            return {
              kind: node.getAttribute("data-kind"),
              name: node.getAttribute("data-name"),
              id: node.getAttribute("data-id"),
              text: node.getAttribute("data-text") || node.textContent,
              i: node.getAttribute("data-i")
            };
          })
        });
      }
    }));
    inited.canvas = { el: el, s: s };
    if (!el.dataset.clickBound) {
      el.dataset.clickBound = "1";
      el.addEventListener("click", function (e) {
        var xbtn = e.target.closest(".tok-x");
        if (xbtn) {
          e.preventDefault();
          e.stopPropagation();
          send("ws_delete", { index: xbtn.getAttribute("data-i") });
          return;
        }
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
      el.addEventListener("keydown", function (e) {
        if (e.key !== "Delete" && e.key !== "Backspace") return;
        if (e.target.closest(".tok-text")) return;
        var tok = el.querySelector(".tok.is-selected");
        if (!tok) return;
        e.preventDefault();
        send("ws_delete", { index: tok.getAttribute("data-i") });
      });
      el.addEventListener("focusout", function (e) {
        var tok = e.target.closest(".tok-text");
        if (!tok) return;
        send("ws_text", { index: tok.getAttribute("data-i"), text: tok.textContent });
      });
    }
  }

  function initAssign(el) {
    if (!window.Sortable || !el) return;
    if (inited.assign && inited.assign.el === el) return;
    destroy("assign");
    var s = Sortable.create(el, Object.assign({}, fallbackOpts, {
      group: { name: "ws", pull: false, put: true },
      onAdd: function (evt) {
        var item = evt.item;
        send("ws_assign", { name: item.getAttribute("data-name") || "" });
        if (item.parentNode) item.parentNode.removeChild(item);
      }
    }));
    inited.assign = { el: el, s: s };
  }

  function initWorkspace() {
    if (!window.Sortable) return;
    var pal = document.getElementById("ws-palette");
    var canvas = document.getElementById("ws-canvas");
    var assign = document.getElementById("ws-assign");
    if (pal) initPalette(pal);
    if (canvas) initCanvas(canvas);
    if (assign) initAssign(assign);
  }

  function boot() {
    if (window.jQuery) {
      $(document).on("shiny:connected shiny:idle", function () {
        setTimeout(initWorkspace, 0);
      });
    }
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler("ws_ready", function () {
        setTimeout(initWorkspace, 0);
      });
    }
    if (window.MutationObserver) {
      var obs = new MutationObserver(function () {
        if (document.getElementById("ws-palette") || document.getElementById("ws-canvas")) {
          initWorkspace();
        }
      });
      obs.observe(document.body || document.documentElement, { childList: true, subtree: true });
    }
    setTimeout(initWorkspace, 50);
    setTimeout(initWorkspace, 400);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})();
