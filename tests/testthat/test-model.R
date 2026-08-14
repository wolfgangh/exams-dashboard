test_that("example exercise validates", {
  ex <- example_exercise()
  expect_equal(ex$meta$type, "num")
  expect_length(validate_exercise(ex), 0)
})

test_that("templates validate", {
  for (kind in c("num", "schoice", "mchoice", "cloze")) {
    expect_length(validate_exercise(template_exercise(kind)), 0)
  }
})

test_that("add_variable rejects duplicates and bad names", {
  ex <- new_exercise()
  ex <- add_variable(ex, "a")
  expect_error(add_variable(ex, "a"))
  expect_error(add_variable(ex, "1x"))
})

test_that("add_rule rejects unsafe expressions", {
  ex <- example_exercise()
  expect_error(add_rule(ex, "system('dir')"))
  expect_error(add_rule(ex, "eval(1)"))
})

test_that("locale marks differ for EU and US", {
  eu <- locale_marks("EU")
  us <- locale_marks("US")
  expect_equal(eu$decimal.mark, ",")
  expect_equal(us$decimal.mark, ".")
  expect_match(format_number(1234.5, "EU", 1), ",", fixed = TRUE)
  expect_match(format_number(1234.5, "US", 1), ".", fixed = TRUE)
})

test_that("convert_type to cloze inserts a gap marker", {
  ex <- template_exercise("num")
  ex$question <- "Nur Text ohne Luecke."
  ex <- convert_type(ex, "cloze")
  expect_equal(ex$meta$type, "cloze")
  expect_match(ex$question, "\\[\\[1\\]\\]")
})
