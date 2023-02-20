context("assertions (vector)")

test_that("is_match_of", {
  expect_true(is_match_of(1, c(1, 2)))
  expect_true(is_match_of("1", c("1", "2")))
  expect_true(is_match_of(1L, c(1L, 2L)))
  expect_false(is_match_of(1, c(0, 2)))
  expect_false(is_match_of("1", c("0", "2")))
  expect_false(is_match_of(1L, c(0L, 2L)))
  expect_error(assert(is_match_of(1, c(0, 2))), "must be")
  expect_error(assert(is_match_of(1, seq(3, 300))), "values")
})

test_that("all_match_of", {
  expect_true(all_match_of(c(1, 2), c(1, 2, 3)))
  expect_true(all_match_of(c("1", "2"), c("1", "2", "3")))
  expect_true(all_match_of(c(1L, 2L), c(1L, 2L, 3L)))
  expect_false(all_match_of(c(1, 4), c(0, 2)))
  expect_false(all_match_of(c("1", "4"), c("0", "2")))
  expect_false(all_match_of(c(1L, 4L), c(0L, 2L)))
  expect_error(assert(all_match_of(c(1, 4), c(0, 2))), "must be")
  expect_error(assert(all_match_of(c(1, 4), seq(3, 300))), "values")
})

test_that("no_duplicates", {
  expect_true(no_duplicates(c(1, 2, 3)))
  expect_true(no_duplicates(c("1", "2", "3")))
  expect_true(no_duplicates(c(1L, 2L, 3L)))
  expect_false(no_duplicates(c(1, 1, 2)))
  expect_false(no_duplicates(c("1", "1", "2")))
  expect_false(no_duplicates(c(1L, 1L, 2L)))
  expect_error(assert(no_duplicates(c(1, 1, 2))), "duplicate")
})

test_that("is_count_vector", {
  expect_true(is_count_vector(c(1, 2, 3)))
  expect_true(is_count_vector(c(1L, 2L, 3L)))
  expect_false(is_count_vector(c(-1, 1, 2, 3)))
  expect_false(is_count_vector(c(-1L, 1L, 2L, 3L)))
  expect_error(assert(is_count_vector(c("1", "2", "3"))), "internal")
  expect_error(assert(is_count_vector(c(-1, 1, 2, 3))), "integer")
})
