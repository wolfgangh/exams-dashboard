# Friendly question tokens: hide {a} / [[1]] / $...$ from authors.

normalize_gap_markers <- function(text) {
  if (!nzchar(text %||% "")) return("")
  text <- gsub("##ANSWER([0-9]+)##", "[[\\1]]", text, perl = TRUE)
  text <- gsub("#+#ANSWER([0-9]+)#+#", "[[\\1]]", text, perl = TRUE)
  text <- gsub("〔Lücke\\s*([0-9]+)〕", "[[\\1]]", text, perl = TRUE)
  text <- gsub("\\[Lücke\\s*([0-9]+)\\]", "[[\\1]]", text, perl = TRUE)
  text
}

from_author_text <- function(text) {
  text <- text %||% ""
  text <- gsub("⟨([A-Za-z][A-Za-z0-9_]*)⟩", "{\\1}", text, perl = TRUE)
  normalize_gap_markers(text)
}

to_author_text <- function(text) {
  text <- normalize_gap_markers(text %||% "")
  text <- gsub("\\{([A-Za-z][A-Za-z0-9_]*)\\}", "⟨\\1⟩", text, perl = TRUE)
  text <- gsub("\\[\\[\\s*([0-9]+)\\s*\\]\\]", "〔Lücke \\1〕", text, perl = TRUE)
  text
}

var_token <- function(name) sprintf("⟨%s⟩", name)
gap_token <- function(id) sprintf("〔Lücke %s〕", as.integer(id))

wrap_math <- function(txt) {
  txt <- trimws(txt %||% "")
  if (!nzchar(txt)) return("")
  if (grepl("^\\$.*\\$$", txt, perl = TRUE)) return(txt)
  paste0("$", txt, "$")
}

gap_ids_in_text <- function(text) {
  text <- normalize_gap_markers(text %||% "")
  m <- gregexpr("\\[\\[\\s*([0-9]+)\\s*\\]\\]", text, perl = TRUE)
  hits <- regmatches(text, m)[[1]]
  if (!length(hits)) return(integer())
  unique(as.integer(gsub("[^0-9]", "", hits)))
}

next_gap_id <- function(ex) {
  used <- unique(c(
    gap_ids_in_text(ex$question %||% ""),
    vapply(ex$items, function(it) as.integer(it$id), integer(1))
  ))
  used <- used[!is.na(used)]
  if (!length(used)) 1L else max(used) + 1L
}



variable_choices <- function(ex) {
  if (!length(ex$variables)) return(c("(keine Variable)" = ""))
  labels <- vapply(ex$variables, function(v) {
    extra <- if (identical(v$kind, "derived") && nzchar(v$expr %||% "")) {
      paste0(" = ", v$expr)
    } else {
      ""
    }
    paste0(v$name, extra)
  }, character(1))
  stats::setNames(vapply(ex$variables, `[[`, character(1), "name"), labels)
}

tokenize_question <- function(text) {
  text <- normalize_gap_markers(text %||% "")
  if (!nzchar(text)) {
    return(list(list(kind = "text", text = "")))
  }
  pat <- "\\$[^$]+\\$|\\{[A-Za-z][A-Za-z0-9_]*\\}|\\[\\[\\s*[0-9]+\\s*\\]\\]"
  m <- gregexpr(pat, text, perl = TRUE)
  starts <- as.integer(m[[1]])
  if (identical(starts, -1L)) {
    return(list(list(kind = "text", text = text)))
  }
  lens <- attr(m[[1]], "match.length")
  tokens <- list()
  cursor <- 1L
  for (i in seq_along(starts)) {
    if (starts[[i]] > cursor) {
      tokens <- c(tokens, list(list(kind = "text", text = substr(text, cursor, starts[[i]] - 1L))))
    }
    raw <- substr(text, starts[[i]], starts[[i]] + lens[[i]] - 1L)
    if (startsWith(raw, "$")) {
      tokens <- c(tokens, list(list(kind = "math", text = substring(raw, 2L, nchar(raw) - 1L))))
    } else if (startsWith(raw, "{")) {
      tokens <- c(tokens, list(list(kind = "var", name = gsub("[{}]", "", raw))))
    } else {
      tokens <- c(tokens, list(list(kind = "gap", id = as.integer(gsub("[^0-9]", "", raw)))))
    }
    cursor <- starts[[i]] + lens[[i]]
  }
  if (cursor <= nchar(text)) {
    tokens <- c(tokens, list(list(kind = "text", text = substring(text, cursor))))
  }
  tokens
}

item_solution_code <- function(it) {
  expr <- trimws(it$solution %||% "")
  scale <- as.numeric(it$scale %||% 1)
  if (!nzchar(expr)) return("NA")
  if (!is.na(scale) && !identical(scale, 1)) {
    return(sprintf("(%s) * %s", expr, scale))
  }
  expr
}
