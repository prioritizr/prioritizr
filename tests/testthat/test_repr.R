context("repr")

test_that("x = numeric", {
  expect_is(repr(c(1, 2, 3)), "character")
})

test_that("x = logical", {
  expect_is(repr(c(TRUE, FALSE)), "character")
})

test_that("x = character", {
  expect_is(repr(c("a", "b", "c")), "character")
})

test_that("x = matrix", {
  expect_is(repr(matrix(c(1, 2, 3))), "character")
  expect_error(
    stop(repr(matrix(3, ncol = 3, nrow = 3))),
    "symmetric continuous values"
  )
  expect_error(
    stop(repr(matrix(c(1, 2, 3)))),
    "asymmetric continuous values"
  )
  expect_error(
    stop(repr(diag(3))),
    "diagonal matrix"
  )
})

test_that("x = Matrix", {
  expect_is(repr(Matrix::Matrix(c(1, 2, 3))), "character")
})

test_that("x = list", {
  expect_is(repr(list(1)), "character")
})

test_that("x = NULL", {
  expect_is(repr(NULL), "character")
})

test_that("x = bbox", {
  sim_pu_polygons <- get_sim_pu_polygons()
  expect_is(repr(sf::st_bbox(sim_pu_polygons)), "character")
})

test_that("x = crs", {
  expect_is(repr(sf::st_crs(4326)), "character")
  expect_error(stop(repr(sf::st_crs(4326))), "geodetic")
  expect_error(stop(repr(sf::st_crs(NA))), "unknown")
})

test_that("x = phylo", {
  sim_phylogeny <- get_sim_phylogeny()
  expect_is(repr(sim_phylogeny), "character")
})

test_that("x = ConservationModifier", {
  expect_is(repr(ConservationModifier$new()), "character")
})

test_that("x = ConservationProblem", {
  expect_is(repr(ConservationProblem$new(list())), "character")
})
