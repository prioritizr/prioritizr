context("loglinear interpolation")

test_that("loglinear_interpolation", {
  x <- seq(0, 1000)
  y <- loglinear_interpolation(x, 200, 100, 900, 15)
  expect_true(all(y[x <= 200] == 100))
  expect_true(all(y[x >= 900] == 15))
  expect_true(all(y[x > 200 & x < 900] < 100))
  expect_true(all(y[x > 200 & x < 900] > 15))
})

test_that("invalid inputs", {
  expect_tidy_error(loglinear_interpolation(seq_len(100), NA, 100, 900, 15))
  expect_tidy_error(loglinear_interpolation(seq_len(100), 200, NA, 900, 15))
  expect_tidy_error(loglinear_interpolation(seq_len(100), 200, 100, NA, 15))
  expect_tidy_error(loglinear_interpolation(seq_len(100), 200, 100, 900, NA))
})
