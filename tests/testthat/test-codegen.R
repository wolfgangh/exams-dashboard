test_that("sampling assigns variables before the constraint check", {
  rmd <- exercise_to_rmd(example_exercise())
  expect_match(rmd, "a <- sample\\(.*\\n(?:.*\\n)*\\s*if \\(isTRUE\\(a > b\\)\\)", perl = TRUE)
})

test_that("codegen uses modern exams API", {
  rmd <- exercise_to_rmd(example_exercise())
  expect_match(rmd, "exams::add_cloze", fixed = TRUE)
  expect_match(rmd, "exams::format_metainfo", fixed = TRUE)
  expect_match(rmd, "exams::initialize_exercise", fixed = TRUE)
  expect_match(rmd, "decimal.mark = \",\"", fixed = TRUE)
  expect_match(rmd, "examsstudio-v1")
})

test_that("US locale switches fmt marks", {
  ex <- example_exercise()
  ex$meta$locale <- "US"
  rmd <- exercise_to_rmd(ex)
  expect_match(rmd, "decimal.mark = \".\"", fixed = TRUE)
})

test_that("cloze codegen emits several add_cloze calls", {
  rmd <- exercise_to_rmd(template_exercise("cloze"))
  expect_gte(length(gregexpr("add_cloze", rmd)[[1]]), 3)
})

test_that("roundtrip via embedded model is lossless for core fields", {
  ex <- template_exercise("cloze")
  rmd <- exercise_to_rmd(ex)
  back <- rmd_to_exercise(rmd)
  expect_false(isTRUE(back$partial))
  expect_equal(back$meta$name, ex$meta$name)
  expect_equal(back$meta$type, ex$meta$type)
  expect_equal(back$question, ex$question)
  expect_equal(length(back$variables), length(ex$variables))
  expect_equal(length(back$items), length(ex$items))
  expect_equal(back$rules[[1]]$expr, ex$rules[[1]]$expr)
})

test_that("write and read exercise file", {
  ex <- template_exercise("schoice")
  path <- tempfile(fileext = ".Rmd")
  write_exercise(ex, path)
  back <- read_exercise_file(path)
  expect_equal(back$meta$type, "schoice")
  expect_equal(length(back$items[[1]]$choices), length(ex$items[[1]]$choices))
})
