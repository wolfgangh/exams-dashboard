app_server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    ex = example_exercise(),
    loading = FALSE,
    var_n = 3L,
    rule_n = 1L,
    item_n = 1L,
    grid = NULL,
    preview = NULL,
    preview_msg = ""
  )

  current_ex <- shiny::reactive({
    collect_exercise(input, rv)
  })

  shiny::observeEvent(input$apply_template, {
    load_model(template_exercise(input$template), session, rv, input)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$locale, {
    if (isTRUE(rv$loading)) return()
    ex <- current_ex()
    ex$meta$locale <- input$locale
    rv$ex <- ex
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$meta_type, {
    if (isTRUE(rv$loading)) return()
    ex <- current_ex()
    ex <- convert_type(ex, input$meta_type)
    rv$ex <- ex
    rv$item_n <- length(ex$items)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$add_var, {
    ex <- current_ex()
    base <- "x"
    names_ <- vapply(ex$variables, function(v) v$name, character(1))
    i <- 1L
    while (paste0(base, i) %in% names_) i <- i + 1L
    ex <- add_variable(ex, paste0(base, i), kind = input$new_var_kind %||% "integer")
    rv$ex <- ex
    rv$var_n <- length(ex$variables)
  })

  shiny::observeEvent(input$add_rule, {
    ex <- current_ex()
    expr <- input$rule_preset
    if (!nzchar(expr %||% "")) expr <- "a > b"
    label <- names(which(c(
      "a > b" = "a groesser als b",
      "a != b" = "a ungleich b",
      "a %% 2 == 0" = "a gerade",
      "a %% 2 == 1" = "a ungerade",
      "abs(a - b) >= 1" = "Abstand mindestens 1"
    ) == expr))
    if (!length(label)) label <- expr
    ex <- tryCatch(add_rule(ex, expr, label = label), error = function(e) {
      shiny::showNotification(e$message, type = "error")
      ex
    })
    rv$ex <- ex
    rv$rule_n <- length(ex$rules)
  })

  shiny::observeEvent(input$add_item, {
    ex <- current_ex()
    ex <- add_item(ex, type = "num", solution = "sol")
    rv$ex <- ex
    rv$item_n <- length(ex$items)
  })

  shiny::observeEvent(input$insert_var, {
    ex <- current_ex()
    nm <- if (length(ex$variables)) ex$variables[[1]]$name else "a"
    shiny::updateTextAreaInput(session, "question", value = paste0(input$question, " {", nm, "}"))
  })

  shiny::observeEvent(input$insert_gap, {
    ex <- current_ex()
    n <- length(ex$items)
    shiny::updateTextAreaInput(session, "question", value = paste0(input$question, " [[", n, "]]"))
    if (!identical(input$meta_type, "cloze")) {
      shiny::updateRadioButtons(session, "meta_type", selected = "cloze")
    }
  })

  shiny::observe({
    n <- rv$var_n
    lapply(seq_len(n), function(i) {
      shiny::observeEvent(input[[paste0("var_del_", i)]], {
        ex <- current_ex()
        if (i <= length(ex$variables)) {
          ex$variables <- ex$variables[-i]
          rv$ex <- ex
          rv$var_n <- length(ex$variables)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  shiny::observe({
    n <- rv$rule_n
    lapply(seq_len(n), function(i) {
      shiny::observeEvent(input[[paste0("rule_del_", i)]], {
        ex <- current_ex()
        if (i <= length(ex$rules)) {
          ex$rules <- ex$rules[-i]
          rv$ex <- ex
          rv$rule_n <- length(ex$rules)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  shiny::observe({
    n <- rv$item_n
    lapply(seq_len(n), function(i) {
      shiny::observeEvent(input[[paste0("item_del_", i)]], {
        ex <- current_ex()
        if (i <= length(ex$items) && length(ex$items) > 1L) {
          ex$items <- ex$items[-i]
          for (k in seq_along(ex$items)) ex$items[[k]]$id <- k
          rv$ex <- ex
          rv$item_n <- length(ex$items)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  shiny::observeEvent(input$load_rmd, {
    path <- input$load_rmd$datapath
    shiny::req(path)
    loaded <- tryCatch(read_exercise_file(path), error = function(e) {
      shiny::showNotification(paste("Laden fehlgeschlagen:", e$message), type = "error")
      NULL
    })
    if (!is.null(loaded)) {
      load_model(loaded, session, rv, input)
      if (isTRUE(loaded$partial)) {
        shiny::showNotification(
          "Die Datei enthaelt kein Studio-Modell. Nur Text wurde importiert.",
          type = "warning", duration = 8
        )
      }
    }
  })

  output$vars_ui <- shiny::renderUI({
    ex <- rv$ex
    n <- rv$var_n
    if (!n) return(shiny::p(class = "help-muted", "Noch keine Variablen."))
    cards <- lapply(seq_len(n), function(i) {
      v <- if (i <= length(ex$variables)) ex$variables[[i]] else blank_variable(paste0("x", i))
      bslib::card(
        class = "studio-card",
        bslib::card_header(paste("Variable", i)),
        bslib::card_body(
          shiny::fluidRow(
            shiny::column(4, shiny::textInput(paste0("var_name_", i), "Name", value = v$name)),
            shiny::column(4, shiny::selectInput(
              paste0("var_kind_", i), "Art",
              choices = c(
                "Ganzzahl" = "integer", "Dezimalzahl" = "numeric",
                "Feste Liste" = "set", "Formel" = "derived"
              ),
              selected = v$kind
            )),
            shiny::column(4, shiny::numericInput(paste0("var_digits_", i), "Anzeigestellen", value = v$digits %||% 0, min = 0, max = 8))
          ),
          shiny::conditionalPanel(
            sprintf("input.var_kind_%s != 'derived' && input.var_kind_%s != 'set'", i, i),
            shiny::fluidRow(
              shiny::column(4, shiny::numericInput(paste0("var_min_", i), "Minimum", value = v$min %||% 1)),
              shiny::column(4, shiny::numericInput(paste0("var_max_", i), "Maximum", value = v$max %||% 10)),
              shiny::column(4, shiny::numericInput(paste0("var_step_", i), "Schritt", value = v$step %||% 1, min = 0.0001))
            )
          ),
          shiny::conditionalPanel(
            sprintf("input.var_kind_%s == 'set'", i),
            shiny::textInput(
              paste0("var_values_", i), "Werte (Komma-getrennt)",
              value = if (!is.null(v$values)) paste(v$values, collapse = ", ") else "2, 4, 8"
            )
          ),
          shiny::conditionalPanel(
            sprintf("input.var_kind_%s == 'derived'", i),
            shiny::textInput(paste0("var_expr_", i), "Formel (R-Ausdruck)", value = v$expr %||% "a + b")
          ),
          shiny::actionButton(paste0("var_del_", i), "Entfernen", class = "btn-sm btn-outline-danger")
        )
      )
    })
    do.call(shiny::tagList, cards)
  })

  output$rules_ui <- shiny::renderUI({
    ex <- rv$ex
    n <- rv$rule_n
    if (!n) return(shiny::p(class = "help-muted", "Keine Regeln — alle Kombinationen gelten als zulaessig."))
    cards <- lapply(seq_len(n), function(i) {
      r <- if (i <= length(ex$rules)) ex$rules[[i]] else blank_rule(paste0("r", i))
      bslib::card(
        class = "studio-card",
        bslib::card_header(paste("Regel", r$id %||% i)),
        bslib::card_body(
          shiny::textInput(paste0("rule_label_", i), "Beschreibung", value = r$label),
          shiny::textInput(paste0("rule_expr_", i), "R-Ausdruck muss TRUE sein", value = r$expr),
          shiny::actionButton(paste0("rule_del_", i), "Entfernen", class = "btn-sm btn-outline-danger")
        )
      )
    })
    do.call(shiny::tagList, cards)
  })

  output$items_ui <- shiny::renderUI({
    ex <- rv$ex
    n <- rv$item_n
    lapply(seq_len(max(n, 1L)), function(i) {
      it <- if (i <= length(ex$items)) ex$items[[i]] else blank_item(i, input$meta_type %||% "num")
      item_editor_ui(i, it, show_delete = n > 1L)
    })
  })

  output$axis_ui <- shiny::renderUI({
    ex <- current_ex()
    nms <- vapply(ex$variables, `[[`, character(1), "name")
    if (!length(nms)) return(NULL)
    shiny::tagList(
      shiny::selectInput("axis_x", "Achse X", choices = nms, selected = nms[[1]]),
      shiny::selectInput("axis_y", "Achse Y", choices = nms, selected = if (length(nms) > 1) nms[[2]] else nms[[1]])
    )
  })

  shiny::observeEvent(input$run_grid, {
    ex <- current_ex()
    errs <- validate_exercise(ex)
    if (length(errs)) {
      shiny::showNotification(paste(errs, collapse = "\n"), type = "error", duration = 10)
      return()
    }
    res <- tryCatch(
      check_combinations(ex, max_rows = as.integer(input$max_rows %||% 4000)),
      error = function(e) {
        shiny::showNotification(e$message, type = "error")
        NULL
      }
    )
    rv$grid <- res
  })

  output$grid_summary <- shiny::renderText({
    if (is.null(rv$grid)) return("Noch keine Pruefung. Bitte „Kombinationen pruefen“ klicken.")
    grid_summary_text(rv$grid, input$locale %||% "EU")
  })

  output$grid_boxes <- shiny::renderUI({
    if (is.null(rv$grid)) return(NULL)
    c <- rv$grid$counts
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box("Gueltig", c$ok %||% 0, theme = "success"),
      bslib::value_box("Regelverstoss", c$rule %||% 0, theme = "warning"),
      bslib::value_box("Unplausibel", c$implausible %||% 0, theme = "danger"),
      bslib::value_box("Fehler", c$error %||% 0, theme = "purple")
    )
  })

  output$grid_plot <- plotly::renderPlotly({
    shiny::req(rv$grid)
    df <- rv$grid$data
    x <- input$axis_x
    y <- input$axis_y
    if (!length(df) || !nrow(df)) {
      return(plotly::plotly_empty(type = "scatter") |> plotly::layout(title = "Keine Daten"))
    }
    cols <- c(ok = "#1e8449", rule = "#b9770e", implausible = "#922b21", error = "#6c3483")
    if (!is.null(x) && !is.null(y) && x %in% names(df) && y %in% names(df) && !identical(x, y)) {
      plotly::plot_ly(
        df,
        x = df[[x]], y = df[[y]],
        color = ~.status, colors = cols,
        type = "scatter", mode = "markers",
        text = ~paste0(.status, "<br>", ifelse(is.na(.issue), "", .issue)),
        hoverinfo = "text",
        marker = list(size = 9, opacity = 0.75)
      ) |> plotly::layout(xaxis = list(title = x), yaxis = list(title = y))
    } else {
      nm <- if (!is.null(x) && x %in% names(df)) x else names(df)[[1]]
      plotly::plot_ly(
        df, x = df[[nm]], color = ~.status, colors = cols,
        type = "histogram"
      ) |> plotly::layout(barmode = "stack", xaxis = list(title = nm))
    }
  })

  output$grid_table <- DT::renderDT({
    shiny::req(rv$grid)
    df <- rv$grid$data
    DT::datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 12, scrollX = TRUE)
    )
  })

  shiny::observeEvent(input$do_preview, {
    ex <- current_ex()
    errs <- validate_exercise(ex)
    if (length(errs)) {
      rv$preview_msg <- paste(errs, collapse = "\n")
      shiny::showNotification(rv$preview_msg, type = "error", duration = 10)
      return()
    }
    rv$preview_msg <- "Erzeuge Vorschau …"
    res <- tryCatch(
      preview_exercise(ex, seed = as.integer(input$preview_seed %||% 1)),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      rv$preview <- NULL
      rv$preview_msg <- paste("Vorschau fehlgeschlagen:", res$message)
    } else {
      rv$preview <- res
      rv$preview_msg <- paste("OK —", basename(res$html))
    }
  })

  output$preview_status <- shiny::renderText(rv$preview_msg)

  output$preview_ui <- shiny::renderUI({
    if (is.null(rv$preview)) {
      return(shiny::p(class = "help-muted", "Noch keine Vorschau erzeugt."))
    }
    html <- paste(readLines(rv$preview$html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    shiny::tags$iframe(
      srcdoc = html,
      class = "preview-frame",
      width = "100%",
      height = "760px",
      sandbox = "allow-scripts allow-same-origin"
    )
  })

  output$rmd_preview <- shiny::renderText({
    ex <- tryCatch(current_ex(), error = function(e) NULL)
    if (is.null(ex)) return("")
    tryCatch(exercise_to_rmd(ex), error = function(e) paste("Noch nicht exportierbar:", e$message))
  })

  output$dl_rmd <- shiny::downloadHandler(
    filename = function() paste0(file_slug(current_ex()$meta$name), ".Rmd"),
    content = function(file) {
      ex <- current_ex()
      errs <- validate_exercise(ex)
      if (length(errs)) stop(paste(errs, collapse = "\n"))
      write_exercise(ex, file)
    }
  )

  output$dl_moodle <- shiny::downloadHandler(
    filename = function() paste0(file_slug(current_ex()$meta$name), "-moodle.zip"),
    content = function(file) {
      ex <- current_ex()
      errs <- validate_exercise(ex)
      if (length(errs)) stop(paste(errs, collapse = "\n"))
      tmp <- tempfile("moodle-")
      res <- export_moodle(ex, tmp, n = as.integer(input$moodle_n %||% 1))
      src <- if (!is.na(res$zip) && file.exists(res$zip)) res$zip else res$xml
      if (is.na(src) || !file.exists(src)) stop("Moodle-Export lieferte keine Datei.")
      file.copy(src, file, overwrite = TRUE)
    }
  )

  output$dl_pdf <- shiny::downloadHandler(
    filename = function() paste0(file_slug(current_ex()$meta$name), ".pdf"),
    content = function(file) {
      ex <- current_ex()
      errs <- validate_exercise(ex)
      if (length(errs)) stop(paste(errs, collapse = "\n"))
      tmp <- tempfile("pdf-")
      res <- export_pdf(ex, tmp, n = as.integer(input$pdf_n %||% 1))
      file.copy(res$pdf, file, overwrite = TRUE)
    }
  )
}

load_model <- function(ex, session, rv, input) {
  rv$loading <- TRUE
  rv$ex <- ex
  rv$var_n <- length(ex$variables)
  rv$rule_n <- length(ex$rules)
  rv$item_n <- length(ex$items)
  rv$grid <- NULL
  shiny::updateTextInput(session, "meta_name", value = ex$meta$name)
  shiny::updateTextInput(session, "meta_title", value = ex$meta$title)
  shiny::updateRadioButtons(session, "meta_type", selected = ex$meta$type)
  shiny::updateNumericInput(session, "meta_points", value = ex$meta$points)
  shiny::updateTextInput(session, "meta_section", value = ex$meta$section %||% "")
  shiny::updateTextAreaInput(session, "question", value = ex$question)
  shiny::updateTextAreaInput(session, "solution_text", value = ex$solution)
  shiny::updateSelectInput(session, "locale", selected = ex$meta$locale %||% "EU")
  rv$loading <- FALSE
}

collect_exercise <- function(input, rv) {
  ex <- rv$ex
  ex$meta$name <- input$meta_name %||% ex$meta$name
  ex$meta$title <- input$meta_title %||% ex$meta$title
  if (!is.null(input$meta_type)) ex$meta$type <- input$meta_type
  if (!is.null(input$meta_points)) ex$meta$points <- input$meta_points
  if (!is.null(input$meta_section)) ex$meta$section <- input$meta_section
  if (!is.null(input$locale)) ex$meta$locale <- input$locale
  if (!is.null(input$question)) ex$question <- input$question
  if (!is.null(input$solution_text)) ex$solution <- input$solution_text

  n <- rv$var_n
  vars <- list()
  for (i in seq_len(n)) {
    nm <- input[[paste0("var_name_", i)]]
    if (is.null(nm)) {
      if (i <= length(ex$variables)) vars <- c(vars, list(ex$variables[[i]]))
      next
    }
    kind <- input[[paste0("var_kind_", i)]] %||% "integer"
    vals <- input[[paste0("var_values_", i)]]
    vars <- c(vars, list(list(
      name = nm,
      kind = kind,
      min = input[[paste0("var_min_", i)]] %||% 1,
      max = input[[paste0("var_max_", i)]] %||% 10,
      step = input[[paste0("var_step_", i)]] %||% 1,
      values = vals,
      expr = input[[paste0("var_expr_", i)]],
      digits = as.integer(input[[paste0("var_digits_", i)]] %||% 0)
    )))
  }
  if (length(vars)) ex$variables <- vars

  n <- rv$rule_n
  rules <- list()
  for (i in seq_len(n)) {
    expr <- input[[paste0("rule_expr_", i)]]
    if (is.null(expr)) {
      if (i <= length(ex$rules)) rules <- c(rules, list(ex$rules[[i]]))
      next
    }
    rules <- c(rules, list(list(
      id = if (i <= length(ex$rules)) ex$rules[[i]]$id else paste0("r", i),
      label = input[[paste0("rule_label_", i)]] %||% expr,
      expr = expr
    )))
  }
  ex$rules <- rules

  n <- rv$item_n
  items <- list()
  for (i in seq_len(n)) {
    typ <- input[[paste0("item_type_", i)]]
    if (is.null(typ)) {
      if (i <= length(ex$items)) items <- c(items, list(ex$items[[i]]))
      next
    }
    items <- c(items, list(collect_item(input, i, typ)))
  }
  if (length(items)) ex$items <- items
  ex
}

collect_item <- function(input, i, typ) {
  nch <- as.integer(input[[paste0("item_nch_", i)]] %||% 0)
  choices <- list()
  if (typ %in% c("schoice", "mchoice")) {
    if (is.na(nch) || nch < 1L) nch <- 3L
    for (k in seq_len(nch)) {
      choices <- c(choices, list(list(
        text = input[[paste0("ch_text_", i, "_", k)]] %||% "",
        correct = isTRUE(input[[paste0("ch_ok_", i, "_", k)]]),
        correct_expr = nzchar_or_null(input[[paste0("ch_okexpr_", i, "_", k)]]),
        dynamic = isTRUE(input[[paste0("ch_dyn_", i, "_", k)]])
      )))
    }
  }
  list(
    id = i,
    type = typ,
    solution = input[[paste0("item_sol_", i)]] %||% "",
    digits = as.integer(input[[paste0("item_digits_", i)]] %||% 2),
    tolerance = input[[paste0("item_tol_", i)]],
    choices = choices
  )
}

nzchar_or_null <- function(x) {
  if (is.null(x) || !nzchar(trimws(as.character(x)))) NULL else x
}

item_editor_ui <- function(i, it, show_delete = FALSE) {
  nch <- max(length(it$choices), if (it$type %in% c("schoice", "mchoice")) 3L else 0L)
  choice_rows <- NULL
  if (it$type %in% c("schoice", "mchoice") || nch > 0L) {
    if (!length(it$choices)) {
      it$choices <- list(blank_choice(TRUE), blank_choice(), blank_choice())
      nch <- 3L
    }
    choice_rows <- lapply(seq_len(nch), function(k) {
      ch <- it$choices[[k]]
      shiny::fluidRow(
        shiny::column(4, shiny::textInput(paste0("ch_text_", i, "_", k), if (k == 1L) "Alternative / Ausdruck" else NULL, value = ch$text)),
        shiny::column(2, shiny::checkboxInput(paste0("ch_dyn_", i, "_", k), "Formel", value = isTRUE(ch$dynamic))),
        shiny::column(2, shiny::checkboxInput(paste0("ch_ok_", i, "_", k), "richtig", value = isTRUE(ch$correct))),
        shiny::column(4, shiny::textInput(paste0("ch_okexpr_", i, "_", k), if (k == 1L) "richtig wenn (optional)" else NULL, value = ch$correct_expr %||% ""))
      )
    })
  }
  shiny::div(
    class = "item-well",
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(
        paste0("item_type_", i), paste("Luecke", i, "— Typ"),
        choices = c("Numerisch" = "num", "Einfachauswahl" = "schoice", "Mehrfachauswahl" = "mchoice"),
        selected = it$type
      )),
      shiny::column(4, shiny::textInput(paste0("item_sol_", i), "Loesungsformel", value = it$solution %||% "")),
      shiny::column(2, shiny::numericInput(paste0("item_digits_", i), "Stellen", value = it$digits %||% 2, min = 0, max = 8)),
      shiny::column(2, shiny::numericInput(paste0("item_tol_", i), "Toleranz", value = it$tolerance %||% NA, min = 0))
    ),
    if (!is.null(choice_rows)) shiny::div(choice_rows),
    shiny::numericInput(paste0("item_nch_", i), NULL, value = nch, min = 0),
    shiny::tags$style(sprintf("#item_nch_%s { display:none; }", i)),
    if (show_delete) shiny::actionButton(paste0("item_del_", i), "Luecke entfernen", class = "btn-sm btn-outline-danger")
  )
}
