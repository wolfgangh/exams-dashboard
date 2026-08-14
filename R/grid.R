# Expand variable grids and classify combinations.

expand_combinations <- function(ex, max_rows = 20000L) {
  indep <- independent_variables(ex)
  if (!length(indep)) {
    df <- data.frame(row.names = 1L)
    return(list(data = df, n_possible = 1L, sampled = FALSE, sequences = list()))
  }
  seqs <- lapply(indep, variable_sequence)
  names(seqs) <- vapply(indep, `[[`, character(1), "name")
  lengths_ <- vapply(seqs, length, integer(1))
  if (any(lengths_ < 1L)) {
    stop("Mindestens eine Variable hat eine leere Wertemenge.", call. = FALSE)
  }
  n_possible <- prod(as.numeric(lengths_))
  sampled <- FALSE
  if (is.finite(n_possible) && n_possible <= max_rows) {
    df <- expand.grid(seqs, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  } else {
    sampled <- TRUE
    n <- as.integer(max_rows)
    df <- as.data.frame(
      lapply(seqs, function(s) sample(s, n, replace = TRUE)),
      stringsAsFactors = FALSE
    )
  }
  list(
    data = df,
    n_possible = n_possible,
    sampled = sampled,
    sequences = seqs
  )
}

apply_derived <- function(row_env, ex) {
  for (v in derived_variables(ex)) {
    row_env[[v$name]] <- eval_safe(v$expr, row_env)
  }
  row_env
}

evaluate_combination_row <- function(env, ex, derived_names) {
  box <- new.env(parent = emptyenv())
  box$res <- list(
    status = "ok",
    rule = NA_character_,
    issue = NA_character_,
    derived = list(),
    items = list()
  )
  mark <- function(st, iss, rule = NA_character_) {
    box$res$status <- st
    box$res$issue <- iss
    box$res$rule <- rule
  }
  tryCatch({
    apply_derived(env, ex)
    for (nm in derived_names) {
      val <- env[[nm]]
      box$res$derived[[nm]] <- if (is.numeric(val) && length(val) == 1L) as.numeric(val) else NA_real_
      if (!identical(classify_value(val), "ok") && identical(box$res$status, "ok")) {
        mark("implausible", paste0(nm, " ist unplausibel"))
      }
    }
    for (r in ex$rules) {
      ok <- tryCatch(isTRUE(eval_safe(r$expr, env)), error = function(e) NA)
      if (is.na(ok)) {
        mark("error", paste0("Regel ", r$id, " konnte nicht ausgewertet werden"), r$id)
        return(box$res)
      }
      if (!ok) {
        mark("rule", r$label %||% r$expr, r$id)
        return(box$res)
      }
    }
    for (it in ex$items) {
      col <- paste0("item", it$id)
      if (identical(it$type, "num")) {
        val <- eval_safe(it$solution, env)
        box$res$items[[col]] <- val
        if (!identical(classify_value(val), "ok") && identical(box$res$status, "ok")) {
          mark("implausible", paste0("Loesung Luecke ", it$id, " ist unplausibel"))
        }
      } else {
        labels <- character(length(it$choices))
        correct <- logical(length(it$choices))
        for (k in seq_along(it$choices)) {
          ch <- it$choices[[k]]
          if (isTRUE(ch$dynamic) && is_safe_expr(ch$text %||% "")) {
            labels[k] <- paste0(eval_safe(ch$text, env), collapse = "")
          } else {
            labels[k] <- interpolate_text(ch$text %||% "", env, ex$meta$locale)
          }
          if (nzchar(ch$correct_expr %||% "")) {
            correct[k] <- isTRUE(eval_safe(ch$correct_expr, env))
          } else {
            correct[k] <- isTRUE(ch$correct)
          }
        }
        if (anyDuplicated(labels) && identical(box$res$status, "ok")) {
          mark("implausible", paste0("Luecke ", it$id, ": doppelte Alternativen"))
        }
        if (identical(it$type, "schoice") && sum(correct) != 1L && identical(box$res$status, "ok")) {
          mark("implausible", paste0("Luecke ", it$id, ": nicht genau eine richtige Alternative"))
        }
        box$res$items[[col]] <- paste(labels[correct], collapse = " | ")
      }
    }
  }, error = function(e) {
    mark("error", e$message)
  })
  box$res
}

classify_value <- function(x) {
  if (length(x) != 1L) return("implausible")
  if (is.null(x) || (is.atomic(x) && is.na(x))) return("implausible")
  if (is.numeric(x) && (is.nan(x) || is.infinite(x))) return("implausible")
  if (is.numeric(x) && abs(as.numeric(x)) > 1e8) return("implausible")
  "ok"
}

check_combinations <- function(ex, max_rows = 20000L) {
  errs <- validate_exercise(ex)
  if (length(errs)) stop(paste(errs, collapse = "\n"), call. = FALSE)
  expanded <- expand_combinations(ex, max_rows = max_rows)
  df <- expanded$data
  n <- nrow(df)
  if (n < 1L) n <- 1L

  derived_names <- vapply(derived_variables(ex), `[[`, character(1), "name")
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    env <- new.env(parent = baseenv())
    if (ncol(df) > 0L) {
      for (nm in names(df)) assign(nm, df[[nm]][i], envir = env)
    }
    rows[[i]] <- evaluate_combination_row(env, ex, derived_names)
  }

  out <- df
  for (nm in derived_names) {
    out[[nm]] <- vapply(rows, function(r) {
      val <- r$derived[[nm]]
      if (is.numeric(val) && length(val) == 1L) as.numeric(val) else NA_real_
    }, numeric(1))
  }
  for (it in ex$items) {
    col <- paste0("item", it$id)
    out[[col]] <- vapply(rows, function(r) {
      val <- r$items[[col]]
      if (is.null(val) || length(val) != 1L) NA_character_ else as.character(val)
    }, character(1))
  }
  status <- vapply(rows, `[[`, character(1), "status")
  out$.status <- factor(status, levels = c("ok", "rule", "implausible", "error"))
  out$.rule <- vapply(rows, function(r) r$rule %||% NA_character_, character(1))
  out$.issue <- vapply(rows, function(r) r$issue %||% NA_character_, character(1))

  counts <- as.list(table(factor(status, levels = c("ok", "rule", "implausible", "error"))))
  list(
    data = out,
    n_possible = expanded$n_possible,
    n_checked = n,
    sampled = expanded$sampled,
    counts = counts,
    sequences = expanded$sequences
  )
}

grid_summary_text <- function(result, locale = "EU") {
  c <- result$counts
  total <- result$n_checked
  ok <- c$ok %||% 0
  paste0(
    format_number(ok, locale, 0), " von ",
    format_number(total, locale, 0), " geprueften Kombinationen sind gueltig",
    if (isTRUE(result$sampled)) {
      paste0(" (Stichprobe; moeglich: ", format_number(result$n_possible, locale, 0), ")")
    } else {
      ""
    },
    "."
  )
}

interpolate_text <- function(text, env, locale = "EU") {
  if (!nzchar(text %||% "")) return("")
  vars <- ls(env)
  out <- text
  for (nm in vars) {
    val <- env[[nm]]
    digits <- if (is.numeric(val) && isTRUE(abs(val - round(val)) < 1e-12)) 0L else 2L
    repl <- if (is.numeric(val) && length(val) == 1L) {
      format_number(val, locale, digits)
    } else {
      paste0(val, collapse = ", ")
    }
    out <- gsub(paste0("\\{", nm, "\\}"), repl, out, perl = TRUE)
  }
  out
}
