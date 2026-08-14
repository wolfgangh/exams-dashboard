# Question canvas: token list <-> question string, drop/reorder/assign.

next_var_name <- function(ex) {
  existing <- vapply(ex$variables, function(v) v$name %||% "", character(1))
  candidates <- c(letters, paste0("x", seq_len(50L)))
  for (nm in candidates) {
    if (!nm %in% existing) return(nm)
  }
  paste0("v", length(existing) + 1L)
}

tokens_to_question <- function(tokens) {
  if (!length(tokens)) return("")
  paste(vapply(tokens, function(tok) {
    switch(tok$kind %||% "text",
      text = tok$text %||% "",
      math = paste0("$", tok$text %||% "", "$"),
      var = paste0("{", tok$name %||% "x", "}"),
      gap = sprintf("[[%s]]", as.integer(tok$id %||% 1L)),
      tok$text %||% ""
    )
  }, character(1)), collapse = "")
}

insert_token_at <- function(tokens, index, token) {
  n <- length(tokens)
  index <- as.integer(index)
  if (is.na(index) || index < 1L) index <- 1L
  if (index > n + 1L) index <- n + 1L
  if (!n) return(list(token))
  if (index == 1L) return(c(list(token), tokens))
  if (index == n + 1L) return(c(tokens, list(token)))
  c(tokens[seq_len(index - 1L)], list(token), tokens[index:n])
}

reorder_tokens <- function(tokens, order_idx) {
  order_idx <- as.integer(order_idx)
  order_idx <- order_idx[order_idx >= 1L & order_idx <= length(tokens)]
  if (length(order_idx) != length(tokens)) return(tokens)
  tokens[order_idx]
}

client_tokens_to_internal <- function(raw) {
  if (!length(raw)) return(list())
  lapply(raw, function(t) {
    kind <- t$kind %||% "text"
    if (identical(kind, "var")) {
      list(kind = "var", name = t$name %||% "x")
    } else if (identical(kind, "gap")) {
      list(kind = "gap", id = as.integer(t$id %||% 1L))
    } else if (identical(kind, "math")) {
      list(kind = "math", text = t$text %||% "")
    } else {
      list(kind = "text", text = t$text %||% "")
    }
  })
}

apply_palette_drop <- function(ex, action, index = 1L, name = NULL) {
  tokens <- tokenize_question(ex$question %||% "")
  selection <- list(kind = "none")
  if (identical(action, "place-var")) {
    nm <- name %||% next_var_name(ex)
    if (!any(vapply(ex$variables, function(v) identical(v$name, nm), logical(1)))) {
      ex <- add_variable(ex, nm, kind = "integer")
    }
    tokens <- insert_token_at(tokens, index, list(kind = "var", name = nm))
    selection <- list(kind = "var", name = nm)
  } else if (identical(action, "new-var-int")) {
    nm <- next_var_name(ex)
    ex <- add_variable(ex, nm, kind = "integer")
    tokens <- insert_token_at(tokens, index, list(kind = "var", name = nm))
    selection <- list(kind = "var", name = nm)
  } else if (identical(action, "new-var-num")) {
    nm <- next_var_name(ex)
    ex <- add_variable(ex, nm, kind = "numeric", digits = 2L)
    tokens <- insert_token_at(tokens, index, list(kind = "var", name = nm))
    selection <- list(kind = "var", name = nm)
  } else if (identical(action, "new-var-derived")) {
    nm <- next_var_name(ex)
    left <- if (length(ex$variables)) ex$variables[[1]]$name else "a"
    right <- if (length(ex$variables) >= 2L) ex$variables[[2]]$name else left
    ex <- add_variable(ex, nm, kind = "derived", expr = paste(left, "+", right))
    tokens <- insert_token_at(tokens, index, list(kind = "var", name = nm))
    selection <- list(kind = "var", name = nm)
  } else if (action %in% c("gap-num", "gap-schoice", "gap-mchoice")) {
    typ <- sub("^gap-", "", action)
    default_sol <- {
      nms <- vapply(derived_variables(ex), function(v) v$name, character(1))
      if (length(nms)) nms[[1]] else if (length(ex$variables)) ex$variables[[1]]$name else "sol"
    }
    ex <- add_item(ex, type = typ, solution = if (identical(typ, "num")) default_sol else "")
    it <- ex$items[[length(ex$items)]]
    if (typ %in% c("schoice", "mchoice") && !length(it$choices)) {
      ex$items[[length(ex$items)]]$choices <- list(blank_choice(TRUE), blank_choice(), blank_choice())
    }
    tokens <- insert_token_at(tokens, index, list(kind = "gap", id = it$id))
    if (length(ex$items) > 1L) ex$meta$type <- "cloze"
    selection <- list(kind = "gap", id = as.integer(it$id))
  } else if (identical(action, "math")) {
    tokens <- insert_token_at(tokens, index, list(kind = "math", text = "a + b"))
    selection <- list(kind = "math")
  } else if (identical(action, "rule")) {
    if (!length(ex$variables)) {
      ex <- add_variable(ex, "a", kind = "integer")
    }
    left <- ex$variables[[1]]$name
    right <- if (length(ex$variables) >= 2L) ex$variables[[2]]$name else left
    ex <- add_rule(ex, paste(left, ">", right), label = paste(left, "groesser als", right))
    selection <- list(kind = "rule", id = ex$rules[[length(ex$rules)]]$id)
    return(list(ex = ex, selection = selection))
  } else if (identical(action, "text")) {
    tokens <- insert_token_at(tokens, index, list(kind = "text", text = " "))
    selection <- list(kind = "text")
  }
  ex$question <- tokens_to_question(tokens)
  list(ex = ex, selection = selection)
}

assign_gap_solution <- function(ex, gap_id, var_name) {
  gap_id <- as.integer(gap_id)
  for (i in seq_along(ex$items)) {
    if (identical(as.integer(ex$items[[i]]$id), gap_id)) {
      ex$items[[i]]$solution <- var_name
      return(ex)
    }
  }
  ex
}

find_variable <- function(ex, name) {
  for (i in seq_along(ex$variables)) {
    if (identical(ex$variables[[i]]$name, name)) return(i)
  }
  NA_integer_
}

update_variable_field <- function(ex, name, field, value) {
  i <- find_variable(ex, name)
  if (is.na(i)) return(ex)
  ex$variables[[i]][[field]] <- value
  ex
}

update_item_field <- function(ex, id, field, value) {
  id <- as.integer(id)
  for (i in seq_along(ex$items)) {
    if (identical(as.integer(ex$items[[i]]$id), id)) {
      ex$items[[i]][[field]] <- value
      return(ex)
    }
  }
  ex
}

remove_token_at <- function(ex, index) {
  toks <- tokenize_question(ex$question %||% "")
  index <- as.integer(index)
  if (is.na(index) || index < 1L || index > length(toks)) return(ex)
  tok <- toks[[index]]
  toks <- toks[-index]
  ex$question <- tokens_to_question(toks)
  if (identical(tok$kind, "gap") && length(ex$items) > 1L) {
    id <- as.integer(tok$id)
    keep <- vapply(ex$items, function(it) !identical(as.integer(it$id), id), logical(1))
    if (any(keep)) ex$items <- ex$items[keep]
  }
  ex
}

remove_variable_from_question <- function(ex, name) {
  ex$question <- gsub(paste0("\\{", name, "\\}"), "", ex$question %||% "", perl = TRUE)
  ex
}

canvas_token_html <- function(tokens, selection = list(kind = "none")) {
  if (!length(tokens)) {
    return("<div id=\"ws-canvas\" class=\"ws-canvas is-empty\">Text schreiben oder etwas von links hierher ziehen.</div>")
  }
  bits <- vapply(seq_along(tokens), function(i) {
    tok <- tokens[[i]]
    sel <- token_is_selected(tok, i, selection)
    cls <- paste("tok", paste0("tok-", tok$kind), if (sel) "is-selected" else "")
    switch(tok$kind,
      var = sprintf(
        "<span class=\"%s\" data-kind=\"var\" data-action=\"place-var\" data-name=\"%s\" data-i=\"%s\" title=\"Variable %s\">⟨%s⟩<button type=\"button\" class=\"tok-x\" data-i=\"%s\" aria-label=\"Entfernen\">×</button></span>",
        cls, html_escape(tok$name), i, html_escape(tok$name), html_escape(tok$name), i
      ),
      gap = sprintf(
        "<span class=\"%s\" data-kind=\"gap\" data-id=\"%s\" data-i=\"%s\">Lücke %s<button type=\"button\" class=\"tok-x\" data-i=\"%s\" aria-label=\"Entfernen\">×</button></span>",
        cls, as.integer(tok$id), i, as.integer(tok$id), i
      ),
      math = sprintf(
        "<span class=\"%s\" data-kind=\"math\" data-text=\"%s\" data-i=\"%s\">$%s$<button type=\"button\" class=\"tok-x\" data-i=\"%s\" aria-label=\"Entfernen\">×</button></span>",
        cls, html_escape(tok$text %||% ""), i, html_escape(tok$text %||% ""), i
      ),
      sprintf(
        "<span class=\"%s\" contenteditable=\"true\" data-kind=\"text\" data-i=\"%s\">%s</span>",
        cls, i, html_escape(tok$text %||% "")
      )
    )
  }, character(1))
  paste0("<div id=\"ws-canvas\" class=\"ws-canvas\">", paste(bits, collapse = ""), "</div>")
}

token_is_selected <- function(tok, index, selection) {
  kind <- selection$kind %||% "none"
  if (identical(kind, "var") && identical(tok$kind, "var")) {
    return(identical(tok$name, selection$name))
  }
  if (identical(kind, "gap") && identical(tok$kind, "gap")) {
    return(identical(as.integer(tok$id), as.integer(selection$id)))
  }
  if (kind %in% c("text", "math") && identical(tok$kind, kind)) {
    return(identical(as.integer(selection$index %||% 0L), as.integer(index)))
  }
  FALSE
}
