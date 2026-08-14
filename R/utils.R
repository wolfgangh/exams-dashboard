# Shared helpers — no package-level side effects.

STUDIO_SCHEMA <- "examsstudio/v1"
STUDIO_MARKER <- "examsstudio-v1"

FORBIDDEN_FUNS <- c(
  "system", "system2", "shell", "Sys.setenv", "Sys.chmod",
  "install.packages", "download.file", "setwd", "unlink",
  "dir.create", "library", "require", "requireNamespace",
  "readLines", "writeLines", "write.table", "saveRDS", "readRDS",
  "eval", "parse", "source"
)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) return(y)
  if (length(x) == 1L && is.character(x) && (is.na(x) || !nzchar(x))) return(y)
  x
}

is_valid_name <- function(x) {
  is.character(x) && length(x) == 1L && grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
}

collect_call_names <- function(expr) {
  if (is.symbol(expr)) return(as.character(expr))
  if (!is.call(expr)) return(character())
  head <- expr[[1]]
  name <- if (is.symbol(head)) {
    as.character(head)
  } else if (is.call(head) && identical(head[[1]], as.name("::"))) {
    paste(as.character(head[[2]]), as.character(head[[3]]), sep = "::")
  } else {
    NA_character_
  }
  rest <- unlist(lapply(as.list(expr)[-1], collect_call_names), use.names = FALSE)
  c(name, rest)
}

is_safe_expr <- function(txt) {
  if (!is.character(txt) || length(txt) != 1L) return(FALSE)
  txt <- trimws(txt)
  if (!nzchar(txt)) return(FALSE)
  if (grepl(":::", txt, fixed = TRUE)) return(FALSE)
  parsed <- tryCatch(parse(text = txt), error = function(e) NULL)
  if (is.null(parsed) || length(parsed) != 1L) return(FALSE)
  used <- unique(collect_call_names(parsed[[1]]))
  used <- used[!is.na(used)]
  bare <- sub("^.*::", "", used)
  !any(used %in% FORBIDDEN_FUNS) && !any(bare %in% FORBIDDEN_FUNS)
}

eval_safe <- function(txt, env) {
  if (!is_safe_expr(txt)) {
    stop("Unsicherer oder ungueltiger Ausdruck: ", txt, call. = FALSE)
  }
  eval(parse(text = txt, keep.source = FALSE), envir = env)
}

unique_id <- function(prefix, existing) {
  i <- 1L
  ids <- vapply(existing, function(x) as.character(x$id %||% ""), character(1))
  repeat {
    cand <- paste0(prefix, i)
    if (!cand %in% ids) return(cand)
    i <- i + 1L
  }
}

file_slug <- function(name) {
  slug <- tolower(gsub("[^A-Za-z0-9]+", "-", name %||% "aufgabe"))
  slug <- gsub("(^-+|-+$)", "", slug)
  if (!nzchar(slug)) "aufgabe" else slug
}

locale_marks <- function(locale = c("EU", "US")) {
  locale <- match.arg(locale)
  if (identical(locale, "EU")) {
    list(decimal.mark = ",", big.mark = ".")
  } else {
    list(decimal.mark = ".", big.mark = ",")
  }
}

format_number <- function(x, locale = "EU", digits = 2) {
  marks <- locale_marks(locale)
  if (!is.numeric(x)) return(as.character(x))
  exams::fmt(
    x,
    digits = digits,
    decimal.mark = marks$decimal.mark,
    big.mark = marks$big.mark
  )
}

blank_choice <- function(correct = FALSE) {
  list(text = "", correct = isTRUE(correct), correct_expr = NULL, dynamic = FALSE)
}

blank_item <- function(id = 1L, type = "num") {
  list(
    id = as.integer(id),
    type = type,
    solution = if (type == "num") "sol" else "",
    digits = 2L,
    tolerance = NULL,
    choices = if (type %in% c("schoice", "mchoice")) {
      list(blank_choice(TRUE), blank_choice(), blank_choice())
    } else {
      list()
    }
  )
}

blank_variable <- function(name = "x", kind = "integer") {
  list(
    name = name,
    kind = kind,
    min = 1,
    max = 10,
    step = 1,
    values = NULL,
    expr = if (identical(kind, "derived")) "a + b" else NULL,
    digits = if (identical(kind, "integer")) 0L else 2L
  )
}

blank_rule <- function(id = "r1") {
  list(id = id, label = "", expr = "")
}

compact_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}
