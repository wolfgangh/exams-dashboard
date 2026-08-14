# Structured exercise model — source of truth for the studio.

new_exercise <- function(name = "aufgabe",
                         type = c("num", "schoice", "mchoice", "cloze"),
                         title = "",
                         question = "",
                         solution = "",
                         locale = c("EU", "US"),
                         points = 1,
                         section = "") {
  type <- match.arg(type)
  locale <- match.arg(locale)
  list(
    schema = STUDIO_SCHEMA,
    meta = list(
      name = file_slug(name),
      title = title %||% name,
      type = type,
      points = as.numeric(points)[1],
      section = section %||% "",
      locale = locale
    ),
    question = question %||% "",
    solution = solution %||% "",
    variables = list(),
    rules = list(),
    items = list(blank_item(1L, if (type == "cloze") "num" else type))
  )
}

example_exercise <- function() {
  template_exercise("num")
}

template_exercise <- function(which = c("num", "schoice", "mchoice", "cloze")) {
  which <- match.arg(which)
  ex <- new_exercise(
    name = switch(which,
      num = "summe",
      schoice = "summe-wahl",
      mchoice = "zahleigenschaften",
      cloze = "summe-differenz"
    ),
    type = which,
    title = switch(which,
      num = "Summe zweier Zahlen",
      schoice = "Summe als Einfachauswahl",
      mchoice = "Eigenschaften zweier Zahlen",
      cloze = "Summe und Differenz"
    ),
    locale = "EU",
    points = 1
  )
  ex$variables <- list(
    list(name = "a", kind = "integer", min = 4, max = 12, step = 1, values = NULL, expr = NULL, digits = 0L),
    list(name = "b", kind = "integer", min = 2, max = 9, step = 1, values = NULL, expr = NULL, digits = 0L),
    list(name = "sol", kind = "derived", min = NA, max = NA, step = NA, values = NULL, expr = "a + b", digits = 0L)
  )
  ex$rules <- list(
    list(id = "r1", label = "a ist groesser als b", expr = "a > b")
  )

  if (which == "num") {
    ex$question <- "Berechnen Sie ${a} + {b}$."
    ex$solution <- "Addieren Sie die beiden Zahlen: ${a} + {b} = {sol}$."
    ex$items <- list(list(
      id = 1L, type = "num", solution = "sol", digits = 0L, tolerance = 0,
      scale = 1, unit = "", choices = list()
    ))
  } else if (which == "schoice") {
    ex$question <- "Welches Ergebnis hat ${a} + {b}$?"
    ex$solution <- "Die Summe ist ${sol}$."
    ex$items <- list(list(
      id = 1L, type = "schoice", solution = "sol", digits = 0L, tolerance = NULL,
      scale = 1, unit = "",
      choices = list(
        list(text = "sol", correct = TRUE, correct_expr = NULL, dynamic = TRUE),
        list(text = "sol + 1", correct = FALSE, correct_expr = NULL, dynamic = TRUE),
        list(text = "sol - 1", correct = FALSE, correct_expr = NULL, dynamic = TRUE),
        list(text = "a * b", correct = FALSE, correct_expr = NULL, dynamic = TRUE)
      )
    ))
  } else if (which == "mchoice") {
    ex$question <- "Welche Aussagen zu $a = {a}$ und $b = {b}$ stimmen?"
    ex$solution <- "Pruefen Sie jede Aussage anhand der gezogenen Werte."
    ex$items <- list(list(
      id = 1L, type = "mchoice", solution = "", digits = 0L, tolerance = NULL,
      scale = 1, unit = "",
      choices = list(
        list(text = "a ist groesser als b", correct = TRUE, correct_expr = "a > b", dynamic = FALSE),
        list(text = "a + b ist gerade", correct = FALSE, correct_expr = "(a + b) %% 2 == 0", dynamic = FALSE),
        list(text = "b ist mindestens 2", correct = TRUE, correct_expr = "b >= 2", dynamic = FALSE),
        list(text = "a ist negativ", correct = FALSE, correct_expr = "a < 0", dynamic = FALSE)
      )
    ))
  } else {
    ex$variables <- c(
      ex$variables,
      list(list(
        name = "diff", kind = "derived", min = NA, max = NA, step = NA,
        values = NULL, expr = "a - b", digits = 0L
      ))
    )
    ex$question <- paste0(
      "Gegeben sind $a = {a}$ und $b = {b}$.\n\n",
      "- Summe $a + b$ = [[1]]\n",
      "- Differenz $a - b$ = [[2]]\n",
      "- Die Summe ist [[3]]"
    )
    ex$solution <- "Summe = {sol}, Differenz = {diff}."
    ex$items <- list(
      list(id = 1L, type = "num", solution = "sol", digits = 0L, tolerance = 0, scale = 1, unit = "EUR", choices = list()),
      list(id = 2L, type = "num", solution = "diff", digits = 0L, tolerance = 0, scale = 1, unit = "EUR", choices = list()),
      list(
        id = 3L, type = "schoice", solution = "", digits = 0L, tolerance = NULL,
        scale = 1, unit = "",
        choices = list(
          list(text = "gerade", correct = FALSE, correct_expr = "(a + b) %% 2 == 0", dynamic = FALSE),
          list(text = "ungerade", correct = FALSE, correct_expr = "(a + b) %% 2 == 1", dynamic = FALSE)
        )
      )
    )
  }
  ex
}

add_variable <- function(ex, name, kind = "integer", min = 1, max = 10,
                         step = 1, values = NULL, expr = NULL, digits = NULL) {
  stopifnot(is.list(ex))
  if (!is_valid_name(name)) stop("Ungueltiger Variablenname: ", name, call. = FALSE)
  existing <- vapply(ex$variables, `[[`, character(1), "name")
  if (name %in% existing) stop("Variable existiert bereits: ", name, call. = FALSE)
  if (is.null(digits)) digits <- if (identical(kind, "integer")) 0L else 2L
  ex$variables <- c(ex$variables, list(list(
    name = name, kind = kind, min = min, max = max, step = step,
    values = values, expr = expr, digits = as.integer(digits)
  )))
  ex
}

add_rule <- function(ex, expr, label = expr, id = NULL) {
  stopifnot(is.list(ex))
  if (!is_safe_expr(expr)) stop("Ungueltige Regel: ", expr, call. = FALSE)
  if (is.null(id)) id <- unique_id("r", ex$rules)
  ex$rules <- c(ex$rules, list(list(id = id, label = label %||% expr, expr = expr)))
  ex
}

add_item <- function(ex, type = c("num", "schoice", "mchoice"),
                     solution = "", digits = 2L, choices = list(), tolerance = NULL) {
  type <- match.arg(type)
  ids <- vapply(ex$items, function(it) as.integer(it$id), integer(1))
  next_id <- if (length(ids)) max(ids) + 1L else 1L
  item <- list(
    id = next_id, type = type, solution = solution,
    digits = as.integer(digits), tolerance = tolerance,
    scale = 1, unit = "", choices = choices
  )
  ex$items <- c(ex$items, list(item))
  if (length(ex$items) > 1L) ex$meta$type <- "cloze"
  ex
}

validate_exercise <- function(ex) {
  errs <- character()
  if (!is.list(ex) || !is.list(ex$meta)) {
    return("Aufgabe hat kein gueltiges Modell.")
  }
  if (!is_valid_name(gsub("-", "_", ex$meta$name))) {
    errs <- c(errs, "Der Aufgabenname darf nur Buchstaben, Ziffern und Bindestriche enthalten.")
  }
  if (!ex$meta$type %in% c("num", "schoice", "mchoice", "cloze")) {
    errs <- c(errs, "Unbekannter Aufgabentyp.")
  }
  if (!nzchar(trimws(ex$question %||% ""))) {
    errs <- c(errs, "Der Fragetext darf nicht leer sein.")
  }
  names_ <- vapply(ex$variables, function(v) v$name %||% "", character(1))
  if (any(!vapply(names_, is_valid_name, logical(1)) & nzchar(names_))) {
    errs <- c(errs, "Mindestens ein Variablenname ist ungueltig.")
  }
  if (any(duplicated(names_[nzchar(names_)]))) {
    errs <- c(errs, "Variablennamen muessen eindeutig sein.")
  }
  for (v in ex$variables) {
    if (identical(v$kind, "derived")) {
      if (!is_safe_expr(v$expr %||% "")) {
        errs <- c(errs, paste0("Formel von ", v$name, " ist ungueltig oder unsicher."))
      }
    } else if (identical(v$kind, "set")) {
      if (is.null(v$values) || !length(v$values)) {
        errs <- c(errs, paste0("Werteliste von ", v$name, " ist leer."))
      }
    } else {
      if (!is.numeric(v$min) || !is.numeric(v$max) || !is.numeric(v$step)) {
        errs <- c(errs, paste0("Bereich von ", v$name, " ist unvollstaendig."))
      } else if (isTRUE(v$max < v$min)) {
        errs <- c(errs, paste0("Maximum von ", v$name, " ist kleiner als das Minimum."))
      } else if (!isTRUE(v$step > 0)) {
        errs <- c(errs, paste0("Schrittweite von ", v$name, " muss positiv sein."))
      }
    }
  }
  for (r in ex$rules) {
    if (!is_safe_expr(r$expr %||% "")) {
      errs <- c(errs, paste0("Regel ", r$id %||% "?", " ist ungueltig oder unsicher."))
    }
  }
  if (!length(ex$items)) {
    errs <- c(errs, "Mindestens ein Antwortfeld ist noetig.")
  }
  for (it in ex$items) {
    if (it$type == "num" && !nzchar(trimws(it$solution %||% ""))) {
      errs <- c(errs, paste0("Luecke ", it$id, ": Loesungsformel fehlt."))
    }
    if (it$type == "num" && !is_safe_expr(it$solution %||% "NA")) {
      errs <- c(errs, paste0("Luecke ", it$id, ": Loesungsformel ist ungueltig."))
    }
    if (it$type %in% c("schoice", "mchoice")) {
      if (!length(it$choices)) {
        errs <- c(errs, paste0("Luecke ", it$id, ": Keine Antwortalternativen."))
      }
      n_true <- sum(vapply(it$choices, function(ch) {
        isTRUE(ch$correct) || nzchar(ch$correct_expr %||% "")
      }, logical(1)))
      if (it$type == "schoice" && !any(vapply(it$choices, function(ch) isTRUE(ch$correct), logical(1))) &&
          !any(nzchar(vapply(it$choices, function(ch) ch$correct_expr %||% "", character(1))))) {
        errs <- c(errs, paste0("Luecke ", it$id, ": Keine korrekte Alternative markiert."))
      }
    }
    for (ch in it$choices %||% list()) {
      if (isTRUE(ch$dynamic) && !nzchar(ch$text %||% "") && !nzchar(ch$correct_expr %||% "")) {
        errs <- c(errs, paste0("Luecke ", it$id, ": dynamische Alternative ohne Ausdruck."))
      }
      expr <- ch$text
      if (isTRUE(ch$dynamic) && nzchar(expr %||% "") && !is_safe_expr(expr)) {
        errs <- c(errs, paste0("Luecke ", it$id, ": Alternativen-Ausdruck ungueltig: ", expr))
      }
      if (nzchar(ch$correct_expr %||% "") && !is_safe_expr(ch$correct_expr)) {
        errs <- c(errs, paste0("Luecke ", it$id, ": Korrektheitsausdruck ungueltig."))
      }
    }
  }
  if (has_legacy_answer_tags(ex$question) || has_legacy_answer_tags(ex$solution)) {
    errs <- c(errs, "Die alte ##ANSWERn##-Syntax wird nicht mehr verwendet. Bitte Lücken über die Schaltfläche einfügen (add_cloze).")
  }
  if (identical(ex$meta$type, "cloze")) {
    ids <- gap_ids_in_text(ex$question)
    if (!length(ids) && length(ex$items) > 1L) {
      errs <- c(errs, "Fügen Sie die Lücken über die Schaltfläche „Lücke einfügen“ in den Fragetext ein.")
    }
  }
  errs
}

convert_type <- function(ex, new_type) {
  new_type <- match.arg(new_type, c("num", "schoice", "mchoice", "cloze"))
  old <- ex$meta$type
  ex$meta$type <- new_type
  if (identical(old, new_type)) return(ex)
  if (new_type != "cloze") {
    it <- if (length(ex$items)) ex$items[[1]] else blank_item(1L, new_type)
    it$id <- 1L
    it$type <- new_type
    if (new_type == "num") {
      if (!nzchar(it$solution %||% "")) it$solution <- "sol"
      it$choices <- list()
    } else if (!length(it$choices)) {
      it$choices <- list(blank_choice(TRUE), blank_choice(), blank_choice())
    }
    ex$items <- list(it)
  } else if (!length(ex$items)) {
    ex$items <- list(blank_item(1L, "num"))
  }
  if (identical(new_type, "cloze") && !grepl("\\[\\[", ex$question %||% "")) {
    ex$question <- paste0(ex$question, "\n\n[[1]]")
  }
  ex
}

independent_variables <- function(ex) {
  Filter(function(v) !identical(v$kind, "derived"), ex$variables)
}

derived_variables <- function(ex) {
  Filter(function(v) identical(v$kind, "derived"), ex$variables)
}

variable_sequence <- function(v) {
  if (identical(v$kind, "set")) {
    vals <- v$values
    if (is.character(vals) && length(vals) == 1L) {
      vals <- trimws(unlist(strsplit(vals, "[,;\\s]+")))
      vals <- vals[nzchar(vals)]
      nums <- suppressWarnings(as.numeric(vals))
      if (all(!is.na(nums))) vals <- nums
    }
    return(unique(vals))
  }
  if (identical(v$kind, "integer")) {
    return(seq(as.integer(v$min), as.integer(v$max), by = as.integer(v$step)))
  }
  seq(as.numeric(v$min), as.numeric(v$max), by = as.numeric(v$step))
}
