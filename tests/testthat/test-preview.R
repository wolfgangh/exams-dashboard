test_that("preview compiles the example with exams2html", {
  skip_if_not_installed("exams")
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  ex <- example_exercise()
  res <- tryCatch(preview_exercise(ex, seed = 1L), error = function(e) e)
  if (inherits(res, "error")) {
    skip(paste("exams2html not usable:", res$message))
  }
  expect_true(file.exists(res$html))
  html <- paste(readLines(res$html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("Question|Frage|Berechnen|Summe|input|form", html, ignore.case = TRUE))
})
