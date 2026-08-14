app_server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    ex = example_exercise(),
    loading = FALSE,
    selection = list(kind = "none"),
    grid = NULL,
    preview = NULL,
    preview_msg = "",
    show_grid = FALSE
  )

  set_ex <- function(ex, selection = NULL) {
    rv$ex <- ex
    if (!is.null(selection)) rv$selection <- selection
  }

  shiny::observeEvent(input$apply_template, {
    rv$loading <- TRUE
    set_ex(template_exercise(input$template), list(kind = "none"))
    shiny::updateTextInput(session, "meta_title", value = rv$ex$meta$title)
    shiny::updateNumericInput(session, "meta_points", value = rv$ex$meta$points)
    shiny::updateSelectInput(session, "locale", selected = rv$ex$meta$locale %||% "EU")
    rv$loading <- FALSE
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$meta_title, {
    if (isTRUE(rv$loading)) return()
    ex <- rv$ex
    ex$meta$title <- input$meta_title
    if (!nzchar(ex$meta$name %||% "") || identical(ex$meta$name, "aufgabe")) {
      ex$meta$name <- file_slug(input$meta_title)
    }
    rv$ex <- ex
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$meta_points, {
    if (isTRUE(rv$loading)) return()
    ex <- rv$ex
    ex$meta$points <- input$meta_points
    rv$ex <- ex
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$locale, {
    if (isTRUE(rv$loading)) return()
    ex <- rv$ex
    ex$meta$locale <- input$locale
    rv$ex <- ex
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$load_rmd, {
    path <- input$load_rmd$datapath
    shiny::req(path)
    loaded <- tryCatch(read_exercise_file(path), error = function(e) {
      shiny::showNotification(paste("Laden fehlgeschlagen:", e$message), type = "error")
      NULL
    })
    if (!is.null(loaded)) {
      rv$loading <- TRUE
      set_ex(loaded, list(kind = "none"))
      shiny::updateTextInput(session, "meta_title", value = loaded$meta$title)
      shiny::updateNumericInput(session, "meta_points", value = loaded$meta$points)
      shiny::updateSelectInput(session, "locale", selected = loaded$meta$locale %||% "EU")
      rv$loading <- FALSE
      if (isTRUE(loaded$partial)) {
        shiny::showNotification("Kein Studio-Modell in der Datei. Nur Text importiert.", type = "warning", duration = 8)
      }
    }
  })

  shiny::observeEvent(input$ws_drop, {
    ev <- input$ws_drop
    res <- apply_palette_drop(rv$ex, ev$action %||% "", index = ev$index %||% 1L, name = nzchar_or_null(ev$name))
    set_ex(res$ex, res$selection)
  })

  shiny::observeEvent(input$ws_reorder, {
    raw <- input$ws_reorder$order
    if (is.null(raw)) return()
    toks <- client_tokens_to_internal(raw)
    if (!length(toks)) return()
    ex <- rv$ex
    ex$question <- tokens_to_question(toks)
    rv$ex <- ex
  })

  shiny::observeEvent(input$ws_select, {
    ev <- input$ws_select
    kind <- ev$kind %||% "none"
    rv$selection <- list(
      kind = kind,
      name = ev$name,
      id = scalar_int(ev$id),
      index = scalar_int(ev$index),
      text = ev$text
    )
  })

  shiny::observeEvent(input$ws_text, {
    ev <- input$ws_text
    idx <- scalar_int(ev$index)
    toks <- tokenize_question(rv$ex$question)
    if (!is.na(idx) && idx >= 1L && idx <= length(toks) && identical(toks[[idx]]$kind, "text")) {
      toks[[idx]]$text <- ev$text %||% ""
      ex <- rv$ex
      ex$question <- tokens_to_question(toks)
      rv$ex <- ex
    }
  })

  shiny::observeEvent(input$ws_delete, {
    idx <- scalar_int(input$ws_delete$index)
    if (is.na(idx)) return()
    set_ex(remove_token_at(rv$ex, idx), list(kind = "none"))
  })

  shiny::observeEvent(input$ws_assign, {
    nm <- input$ws_assign$name
    sel <- rv$selection
    if (!nzchar(nm %||% "") || !identical(sel$kind, "gap")) return()
    set_ex(assign_gap_solution(rv$ex, sel$id, nm), sel)
  })

  output$palette_ui <- shiny::renderUI({
    pal_btn <- function(action, label, extra_class = "", name = "") {
      shiny::tags$button(
        type = "button",
        class = paste("pal-item", extra_class),
        `data-action` = action,
        `data-name` = name,
        draggable = "true",
        label
      )
    }
    ex <- rv$ex
    var_chips <- lapply(ex$variables, function(v) {
      pal_btn("place-var", paste0("⟨", v$name, "⟩"), extra_class = "pal-var", name = v$name)
    })
    shiny::div(
      id = "ws-palette",
      pal_btn("new-var-int", "Neue Zahl"),
      pal_btn("new-var-num", "Neue Dezimalzahl"),
      pal_btn("new-var-derived", "Neue Formel (Summe)"),
      pal_btn("gap-num", "Lücke: Zahl"),
      pal_btn("gap-schoice", "Lücke: Einfachauswahl"),
      pal_btn("gap-mchoice", "Lücke: Mehrfachauswahl"),
      pal_btn("math", "Formel"),
      pal_btn("rule", "Bedingung"),
      if (length(var_chips)) shiny::div(class = "ws-kicker", "Werte — ziehen oder klicken"),
      var_chips
    )
  })

  output$canvas_ui <- shiny::renderUI({
    html <- canvas_token_html(tokenize_question(rv$ex$question), rv$selection)
    shiny::HTML(html)
  })

  output$inspector_ui <- shiny::renderUI({
    inspector_ui(rv$ex, rv$selection, rv$preview_msg, rv$preview)
  })

  output$health_ui <- shiny::renderUI({
    health <- rv$grid
    counts <- if (is.null(health)) list(ok = "–", rule = "–", implausible = "–") else health$counts
    shiny::div(
      class = "insp-card",
      shiny::h5("Werteraum"),
      shiny::div(
        class = "health",
        shiny::div(shiny::strong(counts$ok %||% 0), shiny::span("gültig")),
        shiny::div(shiny::strong(counts$rule %||% 0), shiny::span("Regel")),
        shiny::div(shiny::strong(counts$implausible %||% 0), shiny::span("unplausibel"))
      ),
      shiny::actionButton("toggle_grid", "Diagramm", class = "btn-sm btn-outline-primary mt-2"),
      if (isTRUE(rv$show_grid) && !is.null(rv$grid)) {
        plotly::plotlyOutput("grid_plot", height = "220px")
      }
    )
  })

  shiny::observeEvent(input$toggle_grid, {
    rv$show_grid <- !isTRUE(rv$show_grid)
  })

  shiny::observe({
    ex <- rv$ex
    rv$grid <- tryCatch(check_combinations(ex, max_rows = 800L), error = function(e) NULL)
  })

  output$grid_plot <- plotly::renderPlotly({
    shiny::req(rv$grid)
    df <- rv$grid$data
    nms <- setdiff(names(df), c(".status", ".rule", ".issue"))
    nms <- nms[!startsWith(nms, "item")]
    if (length(nms) < 2L || !nrow(df)) {
      return(plotly::plotly_empty(type = "scatter"))
    }
    cols <- c(ok = "#1e8449", rule = "#b9770e", implausible = "#922b21", error = "#6c3483")
    plotly::plot_ly(
      df, x = df[[nms[[1]]]], y = df[[nms[[2]]]],
      color = ~.status, colors = cols, type = "scatter", mode = "markers",
      marker = list(size = 8, opacity = 0.75)
    ) |> plotly::layout(xaxis = list(title = nms[[1]]), yaxis = list(title = nms[[2]]), margin = list(t = 10))
  })

  bind_inspector_events(input, session, rv)

  output$dl_rmd <- shiny::downloadHandler(
    filename = function() paste0(file_slug(rv$ex$meta$name), ".Rmd"),
    content = function(file) {
      errs <- validate_exercise(rv$ex)
      if (length(errs)) stop(paste(errs, collapse = "\n"))
      write_exercise(rv$ex, file)
    }
  )
  output$dl_moodle <- shiny::downloadHandler(
    filename = function() paste0(file_slug(rv$ex$meta$name), "-moodle.zip"),
    content = function(file) {
      errs <- validate_exercise(rv$ex)
      if (length(errs)) stop(paste(errs, collapse = "\n"))
      tmp <- tempfile("moodle-")
      res <- export_moodle(rv$ex, tmp, n = 5L)
      src <- if (!is.na(res$zip) && file.exists(res$zip)) res$zip else res$xml
      if (is.na(src) || !file.exists(src)) stop("Moodle-Export lieferte keine Datei.")
      file.copy(src, file, overwrite = TRUE)
    }
  )
  output$dl_pdf <- shiny::downloadHandler(
    filename = function() paste0(file_slug(rv$ex$meta$name), ".pdf"),
    content = function(file) {
      errs <- validate_exercise(rv$ex)
      if (length(errs)) stop(paste(errs, collapse = "\n"))
      tmp <- tempfile("pdf-")
      res <- export_pdf(rv$ex, tmp, n = 1L)
      file.copy(res$pdf, file, overwrite = TRUE)
    }
  )
}

inspector_ui <- function(ex, selection, preview_msg = "", preview = NULL) {
  kind <- selection$kind %||% "none"
  if (identical(kind, "var")) {
    return(inspector_var(ex, selection$name))
  }
  if (identical(kind, "gap")) {
    return(inspector_gap(ex, selection$id))
  }
  if (identical(kind, "rule")) {
    return(inspector_rule(ex, selection$id))
  }
  if (identical(kind, "math")) {
    return(shiny::div(
      class = "insp-card",
      shiny::h5("Formel"),
      shiny::textInput("insp_math", "Ohne Dollarzeichen", value = selection$text %||% ""),
      shiny::p(class = "help-muted", "Die App setzt $ … $ selbst.")
    ))
  }
  html <- live_preview_document(ex)
  shiny::div(
    class = "insp-card",
    shiny::h5("So sieht die Aufgabe aus"),
    shiny::tags$iframe(
      srcdoc = html, class = "preview-frame live-frame",
      width = "100%", height = "240px", sandbox = "allow-scripts"
    ),
    shiny::actionButton("do_preview", "Echte exams-Vorschau", class = "btn-sm btn-outline-secondary mt-2"),
    if (nzchar(preview_msg %||% "")) shiny::p(class = "help-muted", preview_msg),
    if (!is.null(preview)) {
      html <- paste(readLines(preview$html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      shiny::tags$iframe(
        srcdoc = html, class = "preview-frame",
        width = "100%", height = "280px", sandbox = "allow-scripts allow-same-origin"
      )
    }
  )
}

inspector_var <- function(ex, name) {
  i <- find_variable(ex, name)
  if (is.na(i)) return(shiny::div(class = "insp-card", "Variable nicht gefunden."))
  v <- ex$variables[[i]]
  shiny::div(
    class = "insp-card",
    shiny::h5(paste("Wert", v$name)),
    shiny::textInput("insp_var_name", "Name", value = v$name),
    shiny::selectInput(
      "insp_var_kind", "Art",
      choices = c("Ganzzahl" = "integer", "Dezimalzahl" = "numeric", "Liste" = "set", "Formel" = "derived"),
      selected = v$kind
    ),
    shiny::conditionalPanel(
      "input.insp_var_kind != 'derived' && input.insp_var_kind != 'set'",
      shiny::numericInput("insp_min", "Minimum", value = v$min %||% 1),
      shiny::numericInput("insp_max", "Maximum", value = v$max %||% 10),
      shiny::numericInput("insp_step", "Schritt", value = v$step %||% 1, min = 0.0001)
    ),
    shiny::conditionalPanel(
      "input.insp_var_kind == 'set'",
      shiny::textInput("insp_values", "Werte, Komma-getrennt", value = if (!is.null(v$values)) paste(v$values, collapse = ", ") else "2, 4, 8")
    ),
    shiny::conditionalPanel(
      "input.insp_var_kind == 'derived'",
      shiny::textInput("insp_expr", "Formel", value = v$expr %||% "a + b")
    ),
    shiny::numericInput("insp_digits", "Nachkommastellen", value = v$digits %||% 0, min = 0, max = 8),
    shiny::actionButton("insp_del_var", "Wert entfernen", class = "btn-sm btn-outline-danger")
  )
}

inspector_gap <- function(ex, id) {
  it <- item_by_id(ex, id)
  if (is.null(it)) return(shiny::div(class = "insp-card", "Lücke nicht gefunden."))
  vars <- variable_choices(ex)
  shiny::div(
    class = "insp-card",
    shiny::h5(paste("Lücke", it$id)),
    shiny::selectInput(
      "insp_gap_type", "Eingabe",
      choices = c("Zahl" = "num", "Einfachauswahl" = "schoice", "Mehrfachauswahl" = "mchoice"),
      selected = it$type
    ),
    shiny::div(
      class = "drop-sol",
      shiny::p(if (nzchar(it$solution %||% "")) paste("Lösung:", it$solution) else "Lösung wählen:"),
      lapply(ex$variables, function(v) {
        shiny::tags$button(
          type = "button",
          class = "pal-item pal-var",
          `data-assign` = v$name,
          paste0("⟨", v$name, "⟩")
        )
      })
    ),
    shiny::conditionalPanel(
      "input.insp_gap_type == 'num'",
      shiny::checkboxInput("insp_percent", "Als Prozent (× 100)", value = isTRUE(as.numeric(it$scale %||% 1) == 100)),
      shiny::textInput("insp_unit", "Einheit", value = it$unit %||% "", placeholder = "EUR, %"),
      shiny::numericInput("insp_gap_digits", "Nachkommastellen", value = it$digits %||% 2, min = 0, max = 8),
      shiny::numericInput("insp_tol", "Toleranz", value = it$tolerance %||% NA, min = 0)
    ),
    if (it$type %in% c("schoice", "mchoice")) inspector_choices(it),
    shiny::actionButton("insp_del_gap", "Lücke entfernen", class = "btn-sm btn-outline-danger")
  )
}

inspector_choices <- function(it) {
  rows <- lapply(seq_along(it$choices), function(k) {
    ch <- it$choices[[k]]
    shiny::div(
      class = "choice-row",
      shiny::textInput(paste0("insp_ch_", k), NULL, value = ch$text %||% ""),
      shiny::checkboxInput(paste0("insp_chok_", k), "richtig", value = isTRUE(ch$correct))
    )
  })
  shiny::tagList(
    shiny::div(class = "ws-kicker", "Alternativen"),
    rows,
    shiny::actionButton("insp_add_ch", "Alternative", class = "btn-sm btn-outline-secondary")
  )
}

inspector_rule <- function(ex, id) {
  r <- NULL
  for (x in ex$rules) if (identical(x$id, id)) r <- x
  if (is.null(r)) {
    if (length(ex$rules)) r <- ex$rules[[length(ex$rules)]] else {
      return(shiny::div(class = "insp-card", "Keine Bedingung."))
    }
  }
  nms <- vapply(ex$variables, function(v) v$name, character(1))
  parts <- tryCatch(parse_rule_parts(r$expr), error = function(e) list(left = nms[1], op = ">", right = nms[1]))
  shiny::div(
    class = "insp-card",
    shiny::h5("Bedingung"),
    shiny::selectInput("insp_rule_left", "Wenn", choices = nms, selected = parts$left),
    shiny::selectInput(
      "insp_rule_op", NULL,
      choices = c("größer als" = ">", "kleiner als" = "<", "ungleich" = "!=", "gleich" = "==", "mindestens" = ">=", "höchstens" = "<="),
      selected = parts$op
    ),
    shiny::selectInput("insp_rule_right", "als", choices = nms, selected = parts$right),
    shiny::actionButton("insp_del_rule", "Bedingung entfernen", class = "btn-sm btn-outline-danger")
  )
}

parse_rule_parts <- function(expr) {
  m <- regexec("^\\s*([A-Za-z][A-Za-z0-9_]*)\\s*(>=|<=|!=|==|>|<)\\s*([A-Za-z][A-Za-z0-9_]*)\\s*$", expr)
  p <- regmatches(expr, m)[[1]]
  if (length(p) < 4L) return(list(left = "a", op = ">", right = "b"))
  list(left = p[[2]], op = p[[3]], right = p[[4]])
}

bind_inspector_events <- function(input, session, rv) {
  shiny::observeEvent(input$insp_var_name, {
    if (isTRUE(rv$loading) || !identical(rv$selection$kind, "var")) return()
    old <- rv$selection$name
    new <- input$insp_var_name
    if (!is_valid_name(new) || identical(old, new)) return()
    i <- find_variable(rv$ex, old)
    if (is.na(i)) return()
    ex <- rv$ex
    ex$variables[[i]]$name <- new
    ex$question <- gsub(paste0("\\{", old, "\\}"), paste0("{", new, "}"), ex$question, perl = TRUE)
    rv$ex <- ex
    rv$selection$name <- new
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$insp_var_kind, {
    if (!identical(rv$selection$kind, "var")) return()
    rv$ex <- update_variable_field(rv$ex, rv$selection$name, "kind", input$insp_var_kind)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_min, {
    if (!identical(rv$selection$kind, "var")) return()
    rv$ex <- update_variable_field(rv$ex, rv$selection$name, "min", input$insp_min)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_max, {
    if (!identical(rv$selection$kind, "var")) return()
    rv$ex <- update_variable_field(rv$ex, rv$selection$name, "max", input$insp_max)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_step, {
    if (!identical(rv$selection$kind, "var")) return()
    rv$ex <- update_variable_field(rv$ex, rv$selection$name, "step", input$insp_step)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_expr, {
    if (!identical(rv$selection$kind, "var")) return()
    rv$ex <- update_variable_field(rv$ex, rv$selection$name, "expr", input$insp_expr)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_values, {
    if (!identical(rv$selection$kind, "var")) return()
    rv$ex <- update_variable_field(rv$ex, rv$selection$name, "values", input$insp_values)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_digits, {
    if (!identical(rv$selection$kind, "var")) return()
    rv$ex <- update_variable_field(rv$ex, rv$selection$name, "digits", as.integer(input$insp_digits))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$insp_del_var, {
    if (!identical(rv$selection$kind, "var")) return()
    i <- find_variable(rv$ex, rv$selection$name)
    if (is.na(i)) return()
    name <- rv$selection$name
    ex <- remove_variable_from_question(rv$ex, name)
    ex$variables <- ex$variables[-i]
    rv$ex <- ex
    rv$selection <- list(kind = "none")
  })

  shiny::observeEvent(input$insp_gap_type, {
    if (!identical(rv$selection$kind, "gap")) return()
    rv$ex <- update_item_field(rv$ex, rv$selection$id, "type", input$insp_gap_type)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_percent, {
    if (!identical(rv$selection$kind, "gap")) return()
    rv$ex <- update_item_field(rv$ex, rv$selection$id, "scale", if (isTRUE(input$insp_percent)) 100 else 1)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_unit, {
    if (!identical(rv$selection$kind, "gap")) return()
    rv$ex <- update_item_field(rv$ex, rv$selection$id, "unit", input$insp_unit)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_gap_digits, {
    if (!identical(rv$selection$kind, "gap")) return()
    rv$ex <- update_item_field(rv$ex, rv$selection$id, "digits", as.integer(input$insp_gap_digits))
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$insp_tol, {
    if (!identical(rv$selection$kind, "gap")) return()
    rv$ex <- update_item_field(rv$ex, rv$selection$id, "tolerance", input$insp_tol)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$insp_del_gap, {
    if (!identical(rv$selection$kind, "gap")) return()
    ex <- rv$ex
    id <- as.integer(rv$selection$id)
    keep <- vapply(ex$items, function(it) !identical(as.integer(it$id), id), logical(1))
    if (sum(keep) < 1L) return()
    ex$items <- ex$items[keep]
    ex$question <- gsub(sprintf("\\[\\[\\s*%s\\s*\\]\\]", id), "", ex$question, perl = TRUE)
    rv$ex <- ex
    rv$selection <- list(kind = "none")
  })

  shiny::observeEvent(input$insp_add_ch, {
    if (!identical(rv$selection$kind, "gap")) return()
    ex <- rv$ex
    id <- as.integer(rv$selection$id)
    for (i in seq_along(ex$items)) {
      if (identical(as.integer(ex$items[[i]]$id), id)) {
        ex$items[[i]]$choices <- c(ex$items[[i]]$choices, list(blank_choice()))
      }
    }
    rv$ex <- ex
  })

  shiny::observe({
    if (!identical(rv$selection$kind, "gap")) return()
    it <- item_by_id(rv$ex, rv$selection$id)
    if (is.null(it) || !length(it$choices)) return()
    changed <- FALSE
    choices <- it$choices
    for (k in seq_along(choices)) {
      txt <- input[[paste0("insp_ch_", k)]]
      ok <- input[[paste0("insp_chok_", k)]]
      if (!is.null(txt) && !identical(txt, choices[[k]]$text)) {
        choices[[k]]$text <- txt
        changed <- TRUE
      }
      if (!is.null(ok) && !identical(isTRUE(ok), isTRUE(choices[[k]]$correct))) {
        choices[[k]]$correct <- isTRUE(ok)
        changed <- TRUE
      }
    }
    if (changed) {
      rv$ex <- update_item_field(rv$ex, rv$selection$id, "choices", choices)
    }
  })

  shiny::observe({
    if (!identical(rv$selection$kind, "rule")) return()
    left <- input$insp_rule_left
    op <- input$insp_rule_op
    right <- input$insp_rule_right
    if (is.null(left) || is.null(op) || is.null(right)) return()
    expr <- paste(left, op, right)
    ex <- rv$ex
    id <- rv$selection$id
    if (!length(ex$rules)) return()
    for (i in seq_along(ex$rules)) {
      hit <- identical(ex$rules[[i]]$id, id) || (is.null(id) && i == length(ex$rules))
      if (!hit) next
      if (identical(ex$rules[[i]]$expr, expr)) return()
      ex$rules[[i]]$expr <- expr
      ex$rules[[i]]$label <- paste(left, op, right)
      rv$ex <- ex
      break
    }
  })

  shiny::observeEvent(input$insp_del_rule, {
    if (!length(rv$ex$rules)) return()
    rv$ex$rules <- rv$ex$rules[-length(rv$ex$rules)]
    rv$selection <- list(kind = "none")
  })

  shiny::observeEvent(input$insp_math, {
    if (!identical(rv$selection$kind, "math")) return()
    idx <- scalar_int(rv$selection$index)
    toks <- tokenize_question(rv$ex$question)
    if (is.na(idx) || idx < 1L || idx > length(toks)) return()
    if (!identical(toks[[idx]]$kind, "math")) return()
    toks[[idx]]$text <- gsub("^\\$|\\$$", "", input$insp_math %||% "")
    ex <- rv$ex
    ex$question <- tokens_to_question(toks)
    rv$ex <- ex
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$do_preview, {
    res <- tryCatch(preview_exercise(rv$ex, seed = 1L), error = function(e) e)
    if (inherits(res, "error")) {
      rv$preview <- NULL
      rv$preview_msg <- res$message
    } else {
      rv$preview <- res
      rv$preview_msg <- "OK"
    }
  })
}

nzchar_or_null <- function(x) {
  if (is.null(x) || !nzchar(trimws(as.character(x)))) NULL else x
}
