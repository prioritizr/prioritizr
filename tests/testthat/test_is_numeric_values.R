test_that("x = default", {
  expect_false(is_numeric_values(new_waiver()))
})

test_that("x = numeric", {
  expect_true(is_numeric_values(c(0, 1, 2, NA)))
})

test_that("x = character", {
  expect_false(is_numeric_values(c(letters[1:5])))
})

test_that("x = Matrix", {
  expect_true(is_numeric_values(Matrix::Matrix(c(0, 1, 2, NA))))
})

test_that("x = matrix", {
  expect_true(is_numeric_values(matrix(c(0, 1, 2, NA))))
  expect_false(is_numeric_values(matrix(c(letters[1:5]))))
  expect_error(
    assert(is_numeric_values(matrix(c(letters[1:5])))),
    "numeric"
  )
})

test_that("x = data.frame", {
  expect_true(is_numeric_values(data.frame(x = c(0, 1, NA), y = c(0, 1, 2))))
  expect_false(is_numeric_values(data.frame(x = c(0, 1, NA), y = letters[1:3])))
  expect_error(
    assert(is_numeric_values(data.frame(x = c(0, 1, NA), y = letters[1:3]))),
    "non-numeric"
  )
})

test_that("x = sf", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0, 1), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(0, 2), y = c("a", "b"), geom = g))
  # tests
  expect_true(is_numeric_values(x))
  expect_false(is_numeric_values(y))
  expect_error(assert(is_numeric_values(y)), "non-numeric")
})

test_that("x = Spatial", {
  # create data
  g <- sf::st_sfc(list(sf::st_point(c(1, 0)), sf::st_point(c(0, 1))))
  x <- sf::st_as_sf(tibble::tibble(x = c(0, NA), y = c(0, 1), geom = g))
  y <- sf::st_as_sf(tibble::tibble(x = c(0, 2), y = c("a", "b"), geom = g))
  # tests
  expect_true(is_numeric_values(sf::as_Spatial(x)))
  expect_false(is_numeric_values(sf::as_Spatial(y)))
  expect_error(
    assert(is_numeric_values(sf::as_Spatial(y))),
    "non-numeric"
  )
})
