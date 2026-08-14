# Load studio Rmd files (lossless via embedded model) or best-effort.

rmd_to_exercise <- function(text) {
  if (length(text) > 1L) text <- paste(text, collapse = "\n")
  text <- enc2utf8(text)
  model <- extract_embedded_model(text)
  if (!is.null(model)) {
    model$partial <- FALSE
    return(normalize_exercise(model))
  }
  normalize_exercise(parse_rmd_best_effort(text))
}

read_exercise_file <- function(path) {
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  rmd_to_exercise(txt)
}

extract_embedded_model <- function(text) {
  pat <- paste0("<!--\\s*", STUDIO_MARKER, ":\\s*([A-Za-z0-9+/=\\s]+)\\s*-->")
  m <- regexec(pat, text, perl = TRUE)
  parts <- regmatches(text, m)[[1]]
  if (length(parts) < 2L) return(NULL)
  payload <- gsub("\\s+", "", parts[[2]], perl = TRUE)
  raw <- tryCatch(jsonlite::base64_dec(payload), error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  yaml_txt <- rawToChar(raw)
  Encoding(yaml_txt) <- "UTF-8"
  obj <- tryCatch(yaml::yaml.load(yaml_txt), error = function(e) NULL)
  obj
}

normalize_exercise <- function(ex) {
  base <- new_exercise(
    name = ex$meta$name %||% "aufgabe",
    type = match.arg(ex$meta$type %||% "num", c("num", "schoice", "mchoice", "cloze")),
    title = ex$meta$title %||% "",
    question = ex$question %||% "",
    solution = ex$solution %||% "",
    locale = match.arg(ex$meta$locale %||% "EU", c("EU", "US")),
    points = ex$meta$points %||% 1,
    section = ex$meta$section %||% ""
  )
  if (!is.null(ex$variables)) base$variables <- lapply(ex$variables, normalize_variable)
  if (!is.null(ex$rules)) base$rules <- lapply(ex$rules, normalize_rule)
  if (!is.null(ex$items) && length(ex$items)) base$items <- lapply(ex$items, normalize_item)
  base$partial <- isTRUE(ex$partial)
  base$schema <- STUDIO_SCHEMA
  base
}

normalize_variable <- function(v) {
  list(
    name = v$name %||% "x",
    kind = v$kind %||% "integer",
    min = v$min %||% 1,
    max = v$max %||% 10,
    step = v$step %||% 1,
    values = v$values,
    expr = v$expr,
    digits = as.integer(v$digits %||% if (identical(v$kind, "integer")) 0L else 2L)
  )
}

normalize_rule <- function(r) {
  list(id = r$id %||% "r1", label = r$label %||% r$expr %||% "", expr = r$expr %||% "")
}

normalize_item <- function(it) {
  choices <- lapply(it$choices %||% list(), function(ch) {
    list(
      text = ch$text %||% "",
      correct = isTRUE(ch$correct),
      correct_expr = ch$correct_expr,
      dynamic = isTRUE(ch$dynamic)
    )
  })
  list(
    id = as.integer(it$id %||% 1L),
    type = it$type %||% "num",
    solution = it$solution %||% "",
    digits = as.integer(it$digits %||% 2L),
    tolerance = it$tolerance,
    choices = choices
  )
}

parse_rmd_best_effort <- function(text) {
  ex <- new_exercise(name = "import", type = "num", title = "Importierte Aufgabe")
  ex$partial <- TRUE
  q <- extract_section(text, "Question")
  s <- extract_section(text, "Solution")
  if (!is.null(q)) ex$question <- trimws(q)
  if (!is.null(s)) ex$solution <- trimws(s)
  meta <- extract_section(text, "Meta-information")
  if (!is.null(meta)) {
    nm <- sub(".*(?:^|\\n)exname:\\s*([^\\n]+).*", "\\1", meta, perl = TRUE)
    if (!identical(nm, meta)) ex$meta$name <- file_slug(trimws(nm))
  }
  ex$meta$title <- paste("Import:", ex$meta$name)
  ex
}

extract_section <- function(text, header) {
  pat <- paste0("(?ms)^", header, "\\s*\\n=+\\s*\\n(.*?)(?=\\n[A-Za-z].*\\n=+\\s*|\\z)")
  m <- regexec(pat, text, perl = TRUE)
  parts <- regmatches(text, m)[[1]]
  if (length(parts) < 2L) return(NULL)
  parts[[2]]
}
