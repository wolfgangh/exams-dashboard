test_that("author tokens convert to internal placeholders", {
  raw <- "Summe $⟨a⟩ + ⟨b⟩$ = 〔Lücke 1〕 und 〔Lücke 2〕"
  inner <- from_author_text(raw)
  expect_equal(inner, "Summe ${a} + {b}$ = [[1]] und [[2]]")
  expect_equal(gap_ids_in_text(raw), c(1L, 2L))
  back <- to_author_text(inner)
  expect_match(back, "⟨a⟩", fixed = TRUE)
  expect_match(back, "〔Lücke 1〕", fixed = TRUE)
})

test_that("legacy ANSWER tags are rejected", {
  expect_true(has_legacy_answer_tags("Wert ##ANSWER1## EUR"))
  ex <- example_exercise()
  ex$question <- "Ergebnis: ##ANSWER1##"
  errs <- validate_exercise(ex)
  expect_true(any(grepl("ANSWER", errs)))
})

test_that("wrap_math adds dollar signs once", {
  expect_equal(wrap_math("A_0"), "$A_0$")
  expect_equal(wrap_math("$a + b$"), "$a + b$")
  expect_equal(wrap_math("  "), "")
})

test_that("tokenize splits text, math, vars and gaps", {
  toks <- tokenize_question("Wert ${a}$ = [[1]]")
  kinds <- vapply(toks, `[[`, character(1), "kind")
  expect_equal(kinds, c("text", "math", "text", "gap"))
})

test_that("live preview shows value chips and numeric gaps", {
  ex <- template_exercise("cloze")
  ex$question <- "Gegeben sind a = {a} und b = {b}. Summe = [[1]], Art = [[3]]"
  html <- live_question_html(ex)
  expect_match(html, "var-chip")
  expect_match(html, "gap-input")
  expect_match(html, "gap-select")
})

test_that("percent scale is compiled into add_cloze", {
  ex <- template_exercise("num")
  ex$items[[1]]$scale <- 100
  rmd <- exercise_to_rmd(ex)
  expect_match(rmd, "(sol) * 100", fixed = TRUE)
})

test_that("sample_draw respects a > b", {
  ex <- example_exercise()
  env <- sample_draw(ex)
  expect_true(env$a > env$b)
  expect_equal(env$sol, env$a + env$b)
})
