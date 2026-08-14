(function () {
  function send(id, payload) {
    payload.nonce = Date.now();
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, payload, { priority: "event" });
    }
  }

  window.studioDrop = function (action, name, index) {
    send("ws_drop", {
      action: action || "",
      name: name || "",
      index: index == null || index === "" ? 999 : index
    });
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

  function tokenIndexFromPoint(x, y) {
    var el = document.elementFromPoint(x, y);
    var tok = el && el.closest ? el.closest("#ws-canvas .tok") : null;
    if (!tok) return 999;
    var i = parseInt(tok.getAttribute("data-i"), 10);
    if (!i) return 999;
    var r = tok.getBoundingClientRect();
    return x > r.left + r.width / 2 ? i + 1 : i;
  }

  function markCanvas(on) {
    var el = document.getElementById("ws-canvas");
    if (el) el.classList.toggle("drag-over", !!on);
  }

  var skipTextSave = false;
  var lastDelete = { index: null, at: 0 };

  function deleteFromControl(el) {
    var index = el.getAttribute("data-delete");
    var now = Date.now();
    if (lastDelete.index === index && now - lastDelete.at < 400) return;
    lastDelete = { index: index, at: now };
    skipTextSave = true;
    studioDelete(index);
    setTimeout(function () {
      skipTextSave = false;
    }, 50);
  }

  document.addEventListener("pointerdown", function (e) {
    var del = e.target.closest ? e.target.closest("[data-delete]") : null;
    if (!del) return;
    e.preventDefault();
    e.stopPropagation();
    deleteFromControl(del);
  });

  document.addEventListener("click", function (e) {
    var del = e.target.closest ? e.target.closest("[data-delete]") : null;
    if (del) {
      e.preventDefault();
      e.stopPropagation();
      deleteFromControl(del);
      return;
    }

    var assign = e.target.closest ? e.target.closest("[data-assign]") : null;
    if (assign) {
      studioAssign(assign.getAttribute("data-assign"));
      return;
    }

    var pal = e.target.closest ? e.target.closest("#ws-palette [data-action], .ws-palette [data-action]") : null;
    if (pal) {
      studioDrop(pal.getAttribute("data-action"), pal.getAttribute("data-name") || "");
      return;
    }

    var tok = e.target.closest ? e.target.closest("#ws-canvas .tok") : null;
    if (!tok) return;
    if (tok.getAttribute("data-kind") === "text") return;
    var kind = tok.getAttribute("data-kind") || "text";
    var extra = { index: parseInt(tok.getAttribute("data-i"), 10) };
    if (kind === "var") extra.name = tok.getAttribute("data-name") || "";
    if (kind === "gap") extra.id = parseInt(tok.getAttribute("data-id"), 10);
    if (kind === "math") extra.text = tok.getAttribute("data-text") || "";
    studioSelect(kind, extra);
  });

  document.addEventListener("dragstart", function (e) {
    var pal = e.target.closest ? e.target.closest("[data-action]") : null;
    if (!pal || !e.dataTransfer) return;
    e.dataTransfer.setData(
      "text/plain",
      JSON.stringify({
        action: pal.getAttribute("data-action"),
        name: pal.getAttribute("data-name") || ""
      })
    );
    e.dataTransfer.effectAllowed = "copy";
  });

  document.addEventListener("dragover", function (e) {
    if (!(e.target.closest && e.target.closest("#ws-canvas, .ws-main"))) return;
    e.preventDefault();
    if (e.dataTransfer) e.dataTransfer.dropEffect = "copy";
    markCanvas(true);
  });

  document.addEventListener("dragleave", function (e) {
    var canvas = document.getElementById("ws-canvas");
    if (!canvas) return;
    if (e.target === canvas || (e.relatedTarget && !canvas.contains(e.relatedTarget))) {
      markCanvas(false);
    }
  });

  document.addEventListener("drop", function (e) {
    if (!(e.target.closest && e.target.closest("#ws-canvas, .ws-main"))) return;
    e.preventDefault();
    markCanvas(false);
    var raw = e.dataTransfer ? e.dataTransfer.getData("text/plain") : "";
    if (!raw) return;
    var data;
    try {
      data = JSON.parse(raw);
    } catch (err) {
      return;
    }
    studioDrop(data.action, data.name || "", tokenIndexFromPoint(e.clientX, e.clientY));
  });

  document.addEventListener("focusout", function (e) {
    if (skipTextSave) return;
    var next = e.relatedTarget;
    if (next && next.closest && next.closest("[data-delete]")) return;
    var edit = e.target.closest ? e.target.closest("#ws-canvas .tok-edit") : null;
    if (!edit) return;
    var tok = edit.closest(".tok");
    if (!tok) return;
    send("ws_text", {
      index: parseInt(tok.getAttribute("data-i"), 10),
      text: edit.textContent || ""
    });
  });

  document.addEventListener("keydown", function (e) {
    if (e.key !== "Delete" && e.key !== "Backspace") return;
    if (e.target && (e.target.isContentEditable || /INPUT|TEXTAREA|SELECT/.test(e.target.tagName))) return;
    var tok = document.querySelector("#ws-canvas .tok.is-selected");
    if (!tok) return;
    e.preventDefault();
    studioDelete(tok.getAttribute("data-i"));
  });
})();
