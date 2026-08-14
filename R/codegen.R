# Emit modern R/exams Rmd (add_cloze / format_metainfo).

exercise_to_rmd <- function(ex) {
  errs <- validate_exercise(ex)
  if (length(errs)) {
    stop(paste(errs, collapse = "\n"), call. = FALSE)
  }
  locale <- ex$meta$locale %||% "EU"
  marks <- locale_marks(locale)
  slug <- file_slug(ex$meta$name)

  data_lines <- c(
    "```{r data, include=FALSE}",
    "exams::initialize_exercise()",
    sprintf(
      ".fmt <- function(x, digits = 2) exams::fmt(x, digits = digits, decimal.mark = \"%s\", big.mark = \"%s\")",
      marks$decimal.mark, marks$big.mark
    ),
    sprintf(".locale <- \"%s\"", locale),
    codegen_sampling(ex),
    codegen_choice_prep(ex),
    "```"
  )

  question_body <- render_question_rmd(ex)
  solution_body <- render_inline_rmd(ex$solution %||% "", ex)

  meta_lines <- c(
    "Meta-information",
    "================",
    sprintf("exname: %s", slug),
    sprintf("extype: `r exams::format_metainfo(\"type\")`"),
    sprintf("exsolution: `r exams::format_metainfo(\"solution\")`"),
    sprintf("extol: `r exams::format_metainfo(\"tolerance\")`")
  )
  if (is.numeric(ex$meta$points) && !is.na(ex$meta$points)) {
    meta_lines <- c(meta_lines, sprintf("expoints: %s", ex$meta$points))
  }
  if (nzchar(ex$meta$section %||% "")) {
    meta_lines <- c(meta_lines, sprintf("exsection: %s", ex$meta$section))
  }

  rmd <- paste(
    c(
      embed_model_comment(ex),
      data_lines,
      "",
      "Question",
      "========",
      question_body,
      "",
      "Answerlist",
      "----------",
      "`r exams::format_metainfo(\"answerlist\")`",
      "",
      "Solution",
      "========",
      solution_body,
      "",
      meta_lines,
      ""
    ),
    collapse = "\n"
  )
  rmd
}

codegen_sampling <- function(ex) {
  indep <- independent_variables(ex)
  derived <- derived_variables(ex)
  draw <- character()
  for (v in indep) {
    draw <- c(draw, sprintf("%s <- sample(%s, 1)", v$name, sequence_code(v)))
  }
  for (v in derived) {
    draw <- c(draw, sprintf("%s <- %s", v$name, trimws(v$expr)))
  }
  if (!length(ex$rules)) {
    return(paste(draw, collapse = "\n"))
  }
  cond <- paste(
    vapply(ex$rules, function(r) paste0("isTRUE(", trimws(r$expr), ")"), character(1)),
    collapse = " && "
  )
  paste(
    c(
      ".ok <- FALSE",
      "for (.try in seq_len(10000L)) {",
      paste0("  ", draw),
      sprintf("  if (%s) { .ok <- TRUE; break }", cond),
      "}",
      "if (!.ok) stop(\"Keine gueltige Variablenkombination gefunden.\")"
    ),
    collapse = "\n"
  )
}

sequence_code <- function(v) {
  if (identical(v$kind, "set")) {
    vals <- variable_sequence(v)
    if (is.numeric(vals)) {
      return(sprintf("c(%s)", paste(vals, collapse = ", ")))
    }
    return(sprintf("c(%s)", paste(sprintf("\"%s\"", escape_rmd_string(vals)), collapse = ", ")))
  }
  if (identical(v$kind, "integer")) {
    return(sprintf("seq(%s, %s, by = %s)", as.integer(v$min), as.integer(v$max), as.integer(v$step)))
  }
  sprintf("seq(%s, %s, by = %s)", as.numeric(v$min), as.numeric(v$max), as.numeric(v$step))
}

codegen_choice_prep <- function(ex) {
  lines <- character()
  for (it in ex$items) {
    if (!it$type %in% c("schoice", "mchoice")) next
    ch <- it$choices %||% list()
    lab_bits <- vapply(seq_along(ch), function(k) choice_label_code(ch[[k]]), character(1))
    cor_bits <- vapply(seq_along(ch), function(k) choice_correct_code(ch[[k]]), character(1))
    lines <- c(
      lines,
      sprintf(".lab_%s <- c(%s)", it$id, paste(lab_bits, collapse = ", ")),
      sprintf(".ok_%s <- c(%s)", it$id, paste(cor_bits, collapse = ", ")),
      sprintf("names(.ok_%s) <- .lab_%s", it$id, it$id)
    )
    if (identical(it$type, "schoice")) {
      lines <- c(lines, sprintf(".sol_%s <- .lab_%s[which(.ok_%s)[1]]", it$id, it$id, it$id))
    }
  }
  paste(lines, collapse = "\n")
}

choice_label_code <- function(ch) {
  if (isTRUE(ch$dynamic) && is_safe_expr(ch$text %||% "")) {
    sprintf("as.character(%s)", trimws(ch$text))
  } else {
    sprintf("\"%s\"", escape_rmd_string(strip_var_braces(ch$text %||% "")))
  }
}

choice_correct_code <- function(ch) {
  if (nzchar(ch$correct_expr %||% "") && is_safe_expr(ch$correct_expr)) {
    sprintf("isTRUE(%s)", trimws(ch$correct_expr))
  } else if (isTRUE(ch$correct)) {
    "TRUE"
  } else {
    "FALSE"
  }
}

strip_var_braces <- function(text) {
  gsub("\\{([A-Za-z][A-Za-z0-9_]*)\\}", "`r .fmt(\\1)`", text, perl = TRUE)
}

render_inline_rmd <- function(text, ex) {
  if (!nzchar(text %||% "")) return("")
  digits_map <- setNames(
    vapply(ex$variables, function(v) as.integer(v$digits %||% 2L), integer(1)),
    vapply(ex$variables, `[[`, character(1), "name")
  )
  out <- text
  for (nm in names(digits_map)) {
    repl <- sprintf("`r .fmt(%s, %s)`", nm, digits_map[[nm]])
    out <- gsub(paste0("\\{", nm, "\\}"), repl, out, perl = TRUE)
  }
  out
}

render_question_rmd <- function(ex) {
  body <- render_inline_rmd(ex$question, ex)
  used <- gregexpr("\\[\\[\\s*([0-9]+)\\s*\\]\\]", body, perl = TRUE)
  matches <- regmatches(body, used)[[1]]
  ids_in_text <- integer()
  if (length(matches)) {
    ids_in_text <- as.integer(gsub("[^0-9]", "", matches))
    for (id in unique(ids_in_text)) {
      it <- item_by_id(ex, id)
      if (is.null(it)) next
      body <- gsub(
        sprintf("\\[\\[\\s*%s\\s*\\]\\]", id),
        add_cloze_inline(it),
        body,
        perl = TRUE
      )
    }
  }
  missing <- Filter(function(it) !it$id %in% ids_in_text, ex$items)
  if (length(missing)) {
    extra <- vapply(missing, add_cloze_inline, character(1))
    body <- paste(c(body, "", extra), collapse = "\n")
  }
  body
}

item_by_id <- function(ex, id) {
  id <- as.integer(id)
  for (it in ex$items) if (identical(as.integer(it$id), id)) return(it)
  NULL
}

add_cloze_inline <- function(it) {
  extra <- character()
  if (identical(it$type, "num") && !is.null(it$tolerance) && !is.na(it$tolerance)) {
    extra <- c(extra, sprintf("tolerance = %s", it$tolerance))
  }
  if (identical(it$type, "num") && !is.null(it$digits) && !is.na(it$digits)) {
    extra <- c(extra, sprintf("digits = %s", as.integer(it$digits)))
  }
  extra_txt <- if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""
  switch(it$type,
    num = sprintf("`r exams::add_cloze(%s%s)`", trimws(it$solution), extra_txt),
    schoice = sprintf("`r exams::add_cloze(.sol_%s, .lab_%s, type = \"schoice\")`", it$id, it$id),
    mchoice = sprintf("`r exams::add_cloze(.ok_%s, type = \"mchoice\")`", it$id),
    sprintf("`r exams::add_cloze(%s)`", trimws(it$solution %||% "NA"))
  )
}

escape_rmd_string <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x
}

embed_model_comment <- function(ex) {
  yaml_txt <- yaml::as.yaml(ex)
  b64 <- gsub("\\s+", "", jsonlite::base64_enc(charToRaw(enc2utf8(yaml_txt))), perl = TRUE)
  paste0("<!-- ", STUDIO_MARKER, ": ", b64, " -->")
}

write_exercise <- function(ex, path) {
  rmd <- exercise_to_rmd(ex)
  writeLines(rmd, path, useBytes = TRUE)
  invisible(path)
}
