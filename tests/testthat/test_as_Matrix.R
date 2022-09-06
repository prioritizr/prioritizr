context("as_Matrix")

test_that("dgCMatrix", {
  d <- matrix(round(runif(9), 5), ncol = 3)
  x <- as_Matrix(d, "dgCMatrix")
  expect_equivalent(as.matrix(x), d)
  expect_equal(class(x)[[1]], "dgCMatrix")
})

test_that("dgTMatrix", {
  d <- matrix(round(runif(9), 5), ncol = 3)
  x <- as_Matrix(d, "dgTMatrix")
  expect_equivalent(as.matrix(x), d)
  expect_equal(class(x)[[1]], "dgTMatrix")
})

test_that("dsCMatrix", {
  d <- matrix(round(runif(9), 5), ncol = 3)
  d[lower.tri(d)] <- d[upper.tri(d)]
  x <- as_Matrix(d, "dsCMatrix")
  expect_equivalent(as.matrix(x), d)
  expect_equal(class(x)[[1]], "dsCMatrix")
})

test_that("lgCMatrix", {
  d <- matrix(sample(c(TRUE, FALSE), 9, replace = TRUE), ncol = 3)
  x <- as_Matrix(d, "lgCMatrix")
  expect_equivalent(as.matrix(x), d)
  expect_equal(class(x)[[1]], "lgCMatrix")
})
