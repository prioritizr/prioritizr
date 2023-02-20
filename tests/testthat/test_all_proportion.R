context("all_proportion")

test_that("x = default", {
  expect_tidy_error(all_proportion(new_waiver()), "recognized")
})

test_that("x = numeric", {
  expect_true(all_proportion(c(0, 0.5, 1, NA)))
  expect_true(all_proportion(c(0L, 1L, NA)))
  expect_false(all_proportion(c(-1, NA, 0)))
  expect_false(all_proportion(c(-1L, NA, 0L)))
  expect_error(
    assert(all_proportion(c(-1, 0.5, 1, NA))),
    "values"
  )
})

test_that("x = Matrix", {
  expect_true(all_proportion(Matrix::Matrix(c(0, 0.5, 1, NA))))
  expect_false(all_proportion(Matrix::Matrix(c(-1, NA, 0))))
  expect_error(
    assert(all_proportion(Matrix::Matrix(c(-1, NA, 0)))),
    "values"
  )
})

test_that("x = matrix", {
  expect_true(all_proportion(matrix(c(0, 0.5, 1, NA))))
  expect_false(all_proportion(matrix(c(-1, NA, 0))))
  expect_error(
    assert(all_proportion(matrix(c(-1, NA, 0)))),
    "values"
  )
})

test_that("x = data.frame", {
  expect_true(all_proportion(data.frame(x = c(0.5, 1, NA), y = c(0L, 1L, NA))))
  expect_false(all_proportion(data.frame(x = c(0.5, 1, NA), y = c(-1, 0, 1))))
  expect_error(
    assert(all_proportion(data.frame(x = c(0.5, 1, NA), y = c(-1, 0, 1)))),
    "values"
  )
})

test_that("x = sf", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)[rep(1, 3)])))
  x <- sf::st_as_sf(
    tibble::tibble(x = c(0.5, 1, NA), y = c(0L, 1L, NA), geom = g)
  )
  y <- sf::st_as_sf(
    tibble::tibble(x = c(-1, 0, NA), y = c(1, 0.5, 0.2), geom = g)
  )
  # tests
  expect_true(all_proportion(x))
  expect_false(all_proportion(y))
  expect_error(assert(all_proportion(y)), "values")
})

test_that("x = Spatial", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)[rep(1, 3)])))
  x <- sf::st_as_sf(
    tibble::tibble(x = c(0.5, 1, NA), y = c(0L, 1L, NA), geom = g)
  )
  y <- sf::st_as_sf(
    tibble::tibble(x = c(-1, 0, NA), y = c(1, 0.5, 0.2), geom = g)
  )
  # tests
  expect_true(all_proportion(sf::as_Spatial(x)))
  expect_false(all_proportion(sf::as_Spatial(y)))
  expect_error(
    assert(all_proportion(sf::as_Spatial(y))),
    "values"
  )
})
