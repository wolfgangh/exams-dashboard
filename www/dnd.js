(function () {
  var lastCaret = null;

  function send(id, payload) {
    payload.nonce = Date.now();
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, payload, { priority: "event" });
    }
  }

  window.studioDrop = function (action, name, index, offset, hostText) {
    var payload = {
      action: action || "",
      name: name || "",
      index: index == null || index === "" ? 999 : index,
      offset: offset == null || offset === "" ? "" : offset,
      host_text: hostText || ""
    };
    send("ws_drop", payload);
  };

  window.studioDelete = function (index) {
    send("ws_delete", { index: index });
  };

  window.studioSelect = function (kind, extra) {
    send("ws_select", Object.assign({ kind: kind || "none" }, extra || {}));
  };

  window.studioAssign = function (name) {
    send("ws_assign", { name: name || "" });
  };

  function editPlainText(el) {
    if (!el) return "";
    var raw = typeof el.innerText === "string" ? el.innerText : el.textContent || "";
    return raw.replace(/\r\n/g, "\n").replace(/\r/g, "\n");
  }

  function textOffsetIn(root, node, nodeOffset) {
    try {
      var range = document.createRange();
      range.setStart(root, 0);
      range.setEnd(node, nodeOffset);
      var tmp = document.createElement("div");
      tmp.appendChild(range.cloneContents());
      return editPlainText(tmp).length;
    } catch (err) {
      return 0;
    }
  }

  function caretRangeAt(x, y) {
    if (document.caretRangeFromPoint) return document.caretRangeFromPoint(x, y);
    if (document.caretPositionFromPoint) {
      var pos = document.caretPositionFromPoint(x, y);
      if (!pos) return null;
      var r = document.createRange();
      r.setStart(pos.offsetNode, pos.offset);
      r.collapse(true);
      return r;
    }
    return null;
  }

  function tokenFromPoint(x, y) {
    var stack = document.elementsFromPoint ? document.elementsFromPoint(x, y) : [document.elementFromPoint(x, y)];
    var i, el, tok;
    for (i = 0; i < stack.length; i++) {
      el = stack[i];
      if (!el || !el.closest) continue;
      if (el.id === "ws-drop-caret") continue;
      tok = el.closest("#ws-canvas .tok");
      if (tok) return tok;
    }
    var nodes = document.querySelectorAll("#ws-canvas .tok");
    var best = null;
    var bestD = Infinity;
    for (i = 0; i < nodes.length; i++) {
      var r = nodes[i].getBoundingClientRect();
      var cx = Math.max(r.left, Math.min(x, r.right));
      var cy = Math.max(r.top, Math.min(y, r.bottom));
      var d = (x - cx) * (x - cx) + (y - cy) * (y - cy);
      if (d < bestD) {
        bestD = d;
        best = nodes[i];
      }
    }
    return best;
  }

  function positionFromPoint(x, y) {
    var tok = tokenFromPoint(x, y);
    if (!tok) return { index: 999, offset: "", hostText: "" };
    var index = parseInt(tok.getAttribute("data-i"), 10) || 999;
    var kind = tok.getAttribute("data-kind");
    if (kind === "text") {
      var edit = tok.querySelector(".tok-edit") || tok;
      var range = caretRangeAt(x, y);
      var offset = 0;
      var host = editPlainText(edit);
      if (range && edit.contains(range.startContainer)) {
        offset = textOffsetIn(edit, range.startContainer, range.startOffset);
      } else {
        var box = tok.getBoundingClientRect();
        offset = x > box.left + box.width / 2 ? host.length : 0;
      }
      return { index: index, offset: offset, hostText: host };
    }
    var r = tok.getBoundingClientRect();
    return { index: x > r.left + r.width / 2 ? index + 1 : index, offset: "", hostText: "" };
  }

  function captureCaretFromEdit(edit) {
    if (!edit) return;
    var tok = edit.closest(".tok");
    if (!tok) return;
    var sel = window.getSelection();
    var offset = editPlainText(edit).length;
    if (sel && sel.rangeCount && edit.contains(sel.anchorNode)) {
      offset = textOffsetIn(edit, sel.anchorNode, sel.anchorOffset);
    }
    lastCaret = {
      index: parseInt(tok.getAttribute("data-i"), 10),
      offset: offset,
      hostText: editPlainText(edit)
    };
  }

  function captureCaret() {
    var sel = window.getSelection();
    if (!sel || !sel.anchorNode) return;
    var node = sel.anchorNode;
    var el = node.nodeType === 1 ? node : node.parentElement;
    var edit = el && el.closest ? el.closest("#ws-canvas .tok-edit") : null;
    if (edit) captureCaretFromEdit(edit);
  }

  function insertAtCaret(action, name) {
    captureCaret();
    if (lastCaret && lastCaret.index) {
      studioDrop(action, name, lastCaret.index, lastCaret.offset, lastCaret.hostText);
      lastCaret = null;
      return;
    }
    studioDrop(action, name);
  }

  function caretMarker() {
    var el = document.getElementById("ws-drop-caret");
    if (!el) {
      el = document.createElement("div");
      el.id = "ws-drop-caret";
      el.className = "ws-drop-caret";
      el.hidden = true;
      document.body.appendChild(el);
    }
    return el;
  }

  function hideCaretMarker() {
    var el = document.getElementById("ws-drop-caret");
    if (el) el.hidden = true;
    markCanvas(false);
  }

  function showCaretMarker(x, y) {
    var pos = positionFromPoint(x, y);
    var marker = caretMarker();
    var range = caretRangeAt(x, y);
    var rect = null;
    if (range) {
      var rects = range.getClientRects();
      if (rects.length) rect = rects[0];
      else {
        var b = range.getBoundingClientRect();
        if (b && (b.height || b.width)) rect = b;
      }
    }
    if (!rect || (!rect.height && !rect.width)) {
      var tok = tokenFromPoint(x, y);
      if (tok) {
        var r = tok.getBoundingClientRect();
        var atRight = pos.index > parseInt(tok.getAttribute("data-i"), 10);
        rect = { left: atRight ? r.right : r.left, top: r.top, height: Math.max(r.height, 18) };
      }
    }
    if (!rect) {
      marker.hidden = true;
      return pos;
    }
    marker.hidden = false;
    marker.style.left = Math.round(rect.left) + "px";
    marker.style.top = Math.round(rect.top) + "px";
    marker.style.height = Math.max(16, Math.round(rect.height || 22)) + "px";
    return pos;
  }

  function markCanvas(on) {
    var el = document.getElementById("ws-canvas");
    if (el) el.classList.toggle("drag-over", !!on);
  }

  document.addEventListener("pointerdown", function (e) {
    if (e.target.closest && e.target.closest("#ws-palette [data-action], .ws-palette [data-action]")) {
      captureCaret();
    }
  }, true);

  document.addEventListener("selectionchange", function () {
    captureCaret();
  });

  document.addEventListener("click", function (e) {
    var del = e.target.closest ? e.target.closest("[data-delete]") : null;
    if (del) {
      e.preventDefault();
      e.stopPropagation();
      studioDelete(del.getAttribute("data-delete"));
      return;
    }

    var assign = e.target.closest ? e.target.closest("[data-assign]") : null;
    if (assign) {
      studioAssign(assign.getAttribute("data-assign"));
      return;
    }

    var pal = e.target.closest ? e.target.closest("#ws-palette [data-action], .ws-palette [data-action]") : null;
    if (pal) {
      insertAtCaret(pal.getAttribute("data-action"), pal.getAttribute("data-name") || "");
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
    showCaretMarker(e.clientX, e.clientY);
  });

  document.addEventListener("dragleave", function (e) {
    var canvas = document.getElementById("ws-canvas");
    if (!canvas) return;
    if (e.target === canvas || (e.relatedTarget && !canvas.contains(e.relatedTarget))) {
      hideCaretMarker();
    }
  });

  document.addEventListener("drop", function (e) {
    if (!(e.target.closest && e.target.closest("#ws-canvas, .ws-main"))) return;
    e.preventDefault();
    var pos = positionFromPoint(e.clientX, e.clientY);
    hideCaretMarker();
    var raw = e.dataTransfer ? e.dataTransfer.getData("text/plain") : "";
    if (!raw) return;
    var data;
    try {
      data = JSON.parse(raw);
    } catch (err) {
      return;
    }
    studioDrop(data.action, data.name || "", pos.index, pos.offset, pos.hostText);
  });

  document.addEventListener("dragend", hideCaretMarker);

  document.addEventListener("focusout", function (e) {
    var next = e.relatedTarget;
    if (next && next.closest && next.closest("[data-delete]")) return;
    var edit = e.target.closest ? e.target.closest("#ws-canvas .tok-edit") : null;
    if (!edit) return;
    captureCaretFromEdit(edit);
    var tok = edit.closest(".tok");
    if (!tok) return;
    send("ws_text", {
      index: parseInt(tok.getAttribute("data-i"), 10),
      text: editPlainText(edit)
    });
  });

  document.addEventListener("keydown", function (e) {
    if (e.key === "Enter" && !e.shiftKey && e.target && e.target.closest && e.target.closest("#ws-canvas .tok-edit")) {
      e.preventDefault();
      if (document.execCommand) document.execCommand("insertLineBreak");
      captureCaret();
      return;
    }
    if (e.key !== "Delete" && e.key !== "Backspace") return;
    if (e.target && (e.target.isContentEditable || /INPUT|TEXTAREA|SELECT/.test(e.target.tagName))) return;
    var tok = document.querySelector("#ws-canvas .tok.is-selected");
    if (!tok) return;
    e.preventDefault();
    studioDelete(tok.getAttribute("data-i"));
  });
})();
