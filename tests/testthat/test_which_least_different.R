test_that("expected results", {
  # simulate values
  x <- matrix(seq_len(12), ncol = 3, byrow = TRUE)
  # run tests
  ## only one parameter value
  expect_equal(
    which_least_different(
      x[1, , drop = FALSE],
      matrix(100, ncol = 3, nrow = 1)
    ),
    1L
  )
  ## multiple parameter values
  expect_equal(
    which_least_different(
      x,
      matrix(5, ncol = 3, nrow = 1)
    ),
    2L
  )
  expect_equal(
    which_least_different(
      x,
      matrix(11, ncol = 3, nrow = 1)
    ),
    4L
  )
  expect_equal(
    which_least_different(
      x,
      matrix(-5, ncol = 3, nrow = 1)
    ),
    1L
  )
})

test_that("invalid inputs", {
  # run tests
  expect_error(
    which_least_different(
      matrix(seq_len(12), ncol = 2),
      matrix(seq_len(3), ncol = 3)
    ),
    "ncol\\(x\\)"
  )
  expect_error(
    which_least_different(
      matrix(seq_len(12), ncol = 2),
      matrix(seq_len(2), ncol = 2, nrow = 2)
    ),
    "nrow\\(y\\)"
  )
  expect_error(
    which_least_different(
      matrix(letters[seq_len(12)], ncol = 2),
      matrix(seq_len(2), ncol = 2)
    ),
    "numeric"
  )
  expect_error(
    which_least_different(
      matrix(seq_len(12), ncol = 2),
      matrix(c("a", "b", "c"), ncol = 3, nrow = 2)
    ),
    "numeric"
  )
})
