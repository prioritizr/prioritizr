context("assertions")

test_that("assert", {
  f <- function(x) x
  assertthat::on_failure(f) <- function(call, env) {
    "x is not TRUE"
  }
  expect_true(assert(1 == 1))
  expect_error(assert(1 == 2))
  expect_error(assert(f(1 == 2)), "not TRUE")
})

test_that("verify", {
  f <- function(x) x
  assertthat::on_failure(f) <- function(call, env) {
    "x is not TRUE"
  }
  expect_true(verify(1 == 1))
  expect_warning(verify(1 == 2))
  expect_warning(verify(f(1 == 2)), "not TRUE")
})
