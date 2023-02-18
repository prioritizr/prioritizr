context("all_finite")

test_that("x = default", {
  expect_tidy_error(all_finite(new_waiver()), "recognized")
})

test_that("x = numeric", {
  expect_true(all_finite(c(0, 1, 2)))
  expect_true(all_finite(c(0L, 1L, 2L)))
  expect_false(all_finite(c(0, NA, 1)))
  expect_false(all_finite(c(0, Inf, 1)))
  expect_false(all_finite(c(0L, NA, 1L)))
  expect_error(assert(all_finite(c(-1, NA, 1))), "finite")
})

test_that("x = character", {
  expect_true(all_finite(c("a", "b", "c")))
  expect_false(all_finite(c("a", NA, "b")))
  expect_error(assert(all_finite(c("a", NA, "b"))), "finite")
})

test_that("x = factor", {
  expect_true(all_finite(factor(c("a", "b", "c"))))
  expect_false(all_finite(factor(c("a", NA, "b"))))
  expect_error(
    assert(all_finite(factor(c("a", NA, "b")))),
    "finite"
  )
})

test_that("x = Matrix", {
  expect_true(all_finite(Matrix::Matrix(c(0, 1, 2))))
  expect_true(all_finite(Matrix::Matrix(c(0L, 1L, 2L))))
  expect_false(all_finite(Matrix::Matrix(c(0, NA, 1))))
  expect_false(all_finite(Matrix::Matrix(c(0, Inf, 1))))
  expect_false(all_finite(Matrix::Matrix(c(0L, NA, 1L))))
  expect_error(
    assert(all_finite(Matrix::Matrix(c(NA, 0, 1)))),
    "finite"
  )
})

test_that("x = matrix", {
  expect_true(all_finite(matrix(c(0, 1, 2))))
  expect_true(all_finite(matrix(c(0L, 1L, 2L))))
  expect_false(all_finite(matrix(c(0, NA, 1))))
  expect_false(all_finite(matrix(c(0, Inf, 1))))
  expect_false(all_finite(matrix(c(0L, NA, 1L))))
  expect_error(
    assert(all_finite(Matrix::Matrix(c(NA, 0, 1)))),
    "finite"
  )
})

test_that("x = array", {
  expect_true(all_finite(array(c(0, 1, 2))))
  expect_false(all_finite(array(c(0, NA, 1))))
  expect_false(all_finite(array(c(0, Inf, 1))))
  expect_error(
    assert(all_finite(array(c(0, NA, 1)))),
    "finite"
  )
})

test_that("x = data.frame", {
  expect_true(all_finite(data.frame(x = c(0, 0.5, 1), y = c(0L, 2L, 1L))))
  expect_false(all_finite(data.frame(x = c(0, NA, 1), y = c(0L, 2L, 1L))))
  expect_false(all_finite(data.frame(x = c(0, Inf, 1), y = c(0L, 2L, 1L))))
  expect_false(all_finite(data.frame(x = c(0, 0.5, 1), y = c(0L, NA, 1L))))
  expect_false(all_finite(data.frame(x = c(0, 0.5, 1), y = c(0L, Inf, 1L))))
  expect_error(
    assert(all_finite(data.frame(x = c(0, NA, -1)))),
    "finite"
  )
})

test_that("x = sf", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, 1), y = c(0L, 1L), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0L, 1L), geom = g))
  w <- sf::st_as_sf(tibble::tibble(x = c(0, Inf), y = c(0L, 1L), geom = g))
  z <- sf::st_as_sf(tibble::tibble(x = c(0, 1), y = c(0L, NA), geom = g))
  # tests
  expect_true(all_finite(x))
  expect_false(all_finite(y))
  expect_false(all_finite(w))
  expect_false(all_finite(z))
  expect_error(assert(all_finite(z)), "finite")
})

test_that("x = Spatial", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, 1), y = c(0L, 1L), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0L, 1L), geom = g))
  w <- sf::st_as_sf(tibble::tibble(x = c(0, Inf), y = c(0L, 1L), geom = g))
  z <- sf::st_as_sf(tibble::tibble(x = c(0, 1), y = c(0L, NA), geom = g))
  # tests
  expect_true(all_finite(sf::as_Spatial(x)))
  expect_false(all_finite(sf::as_Spatial(y)))
  expect_false(all_finite(sf::as_Spatial(w)))
  expect_false(all_finite(sf::as_Spatial(z)))
  expect_error(
    assert(all_finite(sf::as_Spatial(z))),
    "finite"
  )
})
