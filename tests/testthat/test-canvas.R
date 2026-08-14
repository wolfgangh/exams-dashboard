test_that("tokens roundtrip to the question string", {
  ex <- template_exercise("cloze")
  toks <- tokenize_question(ex$question)
  expect_equal(tokens_to_question(toks), normalize_gap_markers(ex$question))
})

test_that("dropping a new integer variable inserts a chip and selects it", {
  ex <- new_exercise(question = "Summe: ")
  res <- apply_palette_drop(ex, "new-var-int", index = 2L)
  expect_match(res$ex$question, "\\{a\\}")
  expect_equal(res$selection$kind, "var")
  expect_equal(res$selection$name, "a")
  expect_equal(res$ex$variables[[1]]$kind, "integer")
})

test_that("dropping a numeric gap creates an add_cloze item", {
  ex <- example_exercise()
  n0 <- length(ex$items)
  res <- apply_palette_drop(ex, "gap-num", index = 99L)
  expect_equal(length(res$ex$items), n0 + 1L)
  expect_equal(res$selection$kind, "gap")
  expect_match(res$ex$question, "\\[\\[")
})

test_that("assign_gap_solution writes the variable onto the item", {
  ex <- template_exercise("cloze")
  ex <- assign_gap_solution(ex, 1L, "diff")
  expect_equal(item_by_id(ex, 1L)$solution, "diff")
})

test_that("remove_token_at drops a gap chip and its item", {
  ex <- template_exercise("cloze")
  toks <- tokenize_question(ex$question)
  gap_i <- which(vapply(toks, function(t) identical(t$kind, "gap") && identical(as.integer(t$id), 1L), logical(1)))[1]
  n_items <- length(ex$items)
  ex2 <- remove_token_at(ex, gap_i)
  expect_false(grepl("\\[\\[1\\]\\]", ex2$question))
  expect_lt(length(ex2$items), n_items)
})

test_that("canvas chips do not use native HTML5 draggable", {
  html <- canvas_token_html(tokenize_question(example_exercise()$question))
  expect_false(grepl("draggable=\"true\"", html, fixed = TRUE))
  expect_match(html, "ws-canvas")
  expect_match(html, "tok-x")
})

test_that("insert and reorder keep all tokens", {
  toks <- list(
    list(kind = "text", text = "A"),
    list(kind = "var", name = "a"),
    list(kind = "text", text = "B")
  )
  toks2 <- insert_token_at(toks, 2L, list(kind = "gap", id = 1L))
  expect_equal(length(toks2), 4L)
  expect_equal(toks2[[2]]$kind, "gap")
  expect_equal(tokens_to_question(reorder_tokens(toks, c(3L, 2L, 1L))), "B{a}A")
})
