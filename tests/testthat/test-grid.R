test_that("full integer grid expands", {
  ex <- example_exercise()
  grid <- expand_combinations(ex)
  expect_false(grid$sampled)
  expect_gt(nrow(grid$data), 10)
  expect_true(all(c("a", "b") %in% names(grid$data)))
})

test_that("rule a > b filters combinations", {
  ex <- example_exercise()
  res <- check_combinations(ex)
  ok <- res$data[res$data$.status == "ok", , drop = FALSE]
  expect_true(nrow(ok) > 0)
  expect_true(all(ok$a > ok$b))
  ruled <- res$data[res$data$.status == "rule", , drop = FALSE]
  expect_true(nrow(ruled) > 0)
  expect_true(all(ruled$a <= ruled$b))
})

test_that("derived sol equals a + b on valid rows", {
  ex <- example_exercise()
  res <- check_combinations(ex)
  ok <- res$data[res$data$.status == "ok", , drop = FALSE]
  expect_equal(ok$sol, ok$a + ok$b)
})

test_that("unsafe rule is rejected before eval", {
  ex <- example_exercise()
  ex$rules[[1]]$expr <- "system('echo hi')"
  expect_error(check_combinations(ex), regexp = "unsicher|ungueltig")
})

test_that("sampling kicks in above max_rows", {
  ex <- example_exercise()
  res <- expand_combinations(ex, max_rows = 5)
  expect_true(res$sampled)
  expect_equal(nrow(res$data), 5)
})
