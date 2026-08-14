# Instant student-facing preview (no exams2html).

sample_draw <- function(ex, tries = 80L) {
  indep <- independent_variables(ex)
  seqs <- lapply(indep, function(v) {
    s <- tryCatch(variable_sequence(v), error = function(e) numeric())
    s
  })
  names(seqs) <- vapply(indep, function(v) v$name %||% "", character(1))
  seqs <- seqs[nzchar(names(seqs))]

  build_env <- function(picker) {
    env <- new.env(parent = baseenv())
    for (nm in names(seqs)) {
      s <- seqs[[nm]]
      if (!length(s)) next
      assign(nm, picker(s), envir = env)
    }
    tryCatch(apply_derived(env, ex), error = function(e) env)
    env
  }

  rules_ok <- function(env) {
    if (!length(ex$rules)) return(TRUE)
    all(vapply(ex$rules, function(r) {
      isTRUE(tryCatch(eval_safe(r$expr, env), error = function(e) FALSE))
    }, logical(1)))
  }

  first <- build_env(function(s) s[[1]])
  if (rules_ok(first)) return(first)
  for (i in seq_len(tries)) {
    env <- build_env(function(s) sample(s, 1))
    if (rules_ok(env)) return(env)
  }
  first
}

html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

format_inline_text <- function(text) {
  text <- html_escape(text)
  text <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", text, perl = TRUE)
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  text <- gsub("\n\n+", "</p><p>", text, perl = TRUE)
  text <- gsub("\n", "<br/>", text, fixed = TRUE)
  text
}

live_math_html <- function(text, env, ex) {
  out <- text
  for (v in ex$variables) {
    val <- tryCatch(env[[v$name]], error = function(e) NULL)
    if (is.numeric(val) && length(val) == 1L) {
      shown <- format_number(val, ex$meta$locale %||% "EU", as.integer(v$digits %||% 2L))
      out <- gsub(paste0("\\{", v$name, "\\}"), shown, out, perl = TRUE)
    }
  }
  paste0("$", out, "$")
}

live_var_html <- function(name, env, ex) {
  val <- tryCatch(env[[name]], error = function(e) NULL)
  digits <- 2L
  for (v in ex$variables) {
    if (identical(v$name, name)) digits <- as.integer(v$digits %||% 2L)
  }
  shown <- if (is.numeric(val) && length(val) == 1L) {
    format_number(val, ex$meta$locale %||% "EU", digits)
  } else if (is.null(val)) {
    name
  } else {
    paste0(val, collapse = ", ")
  }
  sprintf(
    "<span class=\"var-chip\" title=\"Variable %s\">%s</span>",
    html_escape(name), html_escape(shown)
  )
}

live_gap_html <- function(it, env, ex) {
  id <- as.integer(it$id)
  unit <- html_escape(it$unit %||% "")
  scale <- as.numeric(it$scale %||% 1)
  if (identical(it$type, "num")) {
    ph <- if (!is.na(scale) && identical(scale, 100)) "z. B. 12,35" else "Zahl"
    sprintf(
      paste0(
        "<span class=\"gap-wrap\">",
        "<span class=\"gap-label\">Lücke %s</span>",
        "<input class=\"gap-input\" type=\"text\" placeholder=\"%s\" aria-label=\"Lücke %s\"/>",
        "%s</span>"
      ),
      id, ph, id,
      if (nzchar(unit)) paste0("<span class=\"gap-unit\">", unit, "</span>") else ""
    )
  } else if (identical(it$type, "schoice")) {
    opts <- vapply(seq_along(it$choices %||% list()), function(k) {
      lab <- choice_preview_label(it$choices[[k]], env, ex)
      sprintf("<option>%s</option>", html_escape(lab))
    }, character(1))
    sprintf(
      paste0(
        "<span class=\"gap-wrap\">",
        "<span class=\"gap-label\">Lücke %s</span>",
        "<select class=\"gap-select\" aria-label=\"Lücke %s\">",
        "<option value=\"\">Bitte wählen …</option>%s</select></span>"
      ),
      id, id, paste(opts, collapse = "")
    )
  } else {
    boxes <- vapply(seq_along(it$choices %||% list()), function(k) {
      lab <- choice_preview_label(it$choices[[k]], env, ex)
      sprintf(
        "<label class=\"gap-check\"><input type=\"checkbox\"/> %s</label>",
        html_escape(lab)
      )
    }, character(1))
    sprintf(
      "<span class=\"gap-wrap gap-wrap-multi\"><span class=\"gap-label\">Lücke %s</span>%s</span>",
      id, paste(boxes, collapse = "")
    )
  }
}

choice_preview_label <- function(ch, env, ex) {
  if (isTRUE(ch$dynamic) && is_safe_expr(ch$text %||% "")) {
    val <- tryCatch(eval_safe(ch$text, env), error = function(e) ch$text)
    if (is.numeric(val) && length(val) == 1L) {
      return(format_number(val, ex$meta$locale %||% "EU", 2L))
    }
    return(paste0(val, collapse = ", "))
  }
  interpolate_text(ch$text %||% "", env, ex$meta$locale %||% "EU")
}

live_question_html <- function(ex) {
  env <- sample_draw(ex)
  tokens <- tokenize_question(ex$question %||% "")
  parts <- vapply(tokens, function(tok) {
    switch(tok$kind,
      text = format_inline_text(tok$text),
      math = live_math_html(tok$text, env, ex),
      var = live_var_html(tok$name, env, ex),
      gap = {
        it <- item_by_id(ex, tok$id)
        if (is.null(it)) {
          sprintf("<span class=\"gap-missing\">Lücke %s fehlt</span>", tok$id)
        } else {
          live_gap_html(it, env, ex)
        }
      },
      html_escape(tok$text %||% "")
    )
  }, character(1))
  html <- paste0("<p>", paste(parts, collapse = ""), "</p>")
  used <- gap_ids_in_text(ex$question)
  extra <- Filter(function(it) !as.integer(it$id) %in% used, ex$items)
  if (length(extra) && !identical(ex$meta$type, "cloze")) {
    extra_html <- paste(vapply(extra, function(it) live_gap_html(it, env, ex), character(1)), collapse = " ")
    html <- paste0(html, "<p>", extra_html, "</p>")
  }
  html
}

live_preview_document <- function(ex) {
  body <- tryCatch(live_question_html(ex), error = function(e) {
    paste0("<p class=\"preview-error\">Vorschau: ", html_escape(e$message), "</p>")
  })
  sol <- tryCatch({
    env <- sample_draw(ex)
    interpolate_text(ex$solution %||% "", env, ex$meta$locale %||% "EU")
  }, error = function(e) "")
  sol_html <- if (nzchar(sol)) {
    paste0("<hr/><h3>Lösungsweg (Beispielwerte)</h3><p>", format_inline_text(sol), "</p>")
  } else {
    ""
  }
  paste0(
    "<!DOCTYPE html><html lang=\"de\"><head><meta charset=\"utf-8\"/>",
    "<script>window.MathJax={tex:{inlineMath:[['$','$'],['\\\\(','\\\\)']]}};</script>",
    "<script src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\" async></script>",
    "<style>",
    "body{font-family:'Segoe UI',Calibri,sans-serif;color:#1c2833;margin:1.1rem;line-height:1.55;}",
    "h2,h3{color:#1b4f72;font-size:1.05rem;margin:0.8rem 0 0.4rem;}",
    ".var-chip{display:inline-block;background:#d4e6f1;color:#1a5276;border-radius:999px;",
    "padding:0.05rem 0.55rem;font-weight:650;margin:0 0.08rem;}",
    ".gap-wrap{display:inline-flex;align-items:center;gap:0.3rem;margin:0.15rem 0.1rem;",
    "padding:0.2rem 0.35rem;border:1.5px dashed #1b4f72;border-radius:8px;background:#f4f8fb;vertical-align:middle;}",
    ".gap-wrap-multi{display:inline-flex;flex-direction:column;align-items:flex-start;}",
    ".gap-label{font-size:0.72rem;font-weight:700;color:#1b4f72;letter-spacing:.02em;}",
    ".gap-input,.gap-select{min-width:7rem;border:1px solid #85929e;border-radius:4px;padding:0.2rem 0.35rem;}",
    ".gap-unit{color:#5d6d7e;font-size:0.9rem;}",
    ".gap-check{display:block;font-size:0.92rem;}",
    ".gap-missing{color:#922b21;font-weight:600;}",
    ".preview-error{color:#922b21;}",
    "hr{border:none;border-top:1px solid #d5d8dc;margin:1rem 0;}",
    "</style></head><body>",
    "<h2>", html_escape(ex$meta$title %||% "Aufgabe"), "</h2>",
    body, sol_html, "</body></html>"
  )
}
