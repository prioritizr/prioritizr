test_that("x = numeric", {
  expect_inherits(repr(c(1, 2, 3)), "character")
})

test_that("x = logical", {
  expect_inherits(repr(c(TRUE, FALSE)), "character")
})

test_that("x = character (single value)", {
  expect_inherits(repr("a"), "character")
  expect_false(grepl("total", repr("a"), fixed = TRUE))
})

test_that("x = character (multiple values)", {
  expect_inherits(repr(c("a", "b", "c")), "character")
  expect_match(repr(c("a", "b", "c")), "total")
  expect_match(
    repr(c(paste(rep("a", 1000), collapse = ""), "b", "c")),
    "total"
  )
})

test_that("x = matrix (nrow == ncol", {
  expect_inherits(repr(matrix(c(1, 2, 3))), "character")
  # relationship matrices
  expect_match(
    repr(matrix(3, ncol = 3, nrow = 3)),
    "symmetric continuous values"
  )
  expect_match(
    repr(matrix(c(1, 2, 3), ncol = 3, nrow = 3)),
    "asymmetric continuous values"
  )
  expect_match(
    repr(matrix(c(1, 0, 1), ncol = 3, nrow = 3)),
    "asymmetric binary values"
  )
  expect_match(
    repr(matrix(1, ncol = 3, nrow = 3)),
    "symmetric binary values"
  )
  expect_match(
    repr(diag(3)),
    "diagonal matrix"
  )
  expect_match(
    repr(diag(3) * runif(3)),
    "diagonal matrix \\(non-zero values"
  )
})

test_that("x = matrix (nrow != ncol", {
  expect_match(
    repr(matrix(c(1, 2, 3), ncol = 1, nrow = 3)),
    "continuous values"
  )
  expect_false(
    grepl("symmetric", repr(matrix(c(1, 2, 3), ncol = 1, nrow = 3)))
  )
  expect_match(
    repr(matrix(c(1, 0, 1), ncol = 1, nrow = 3)),
    "binary values"
  )
  expect_false(
    grepl("symmetric", repr(matrix(c(1, 0, 1), ncol = 1, nrow = 3)))
  )
})

test_that("x = Matrix", {
  expect_inherits(repr(Matrix::Matrix(c(1, 2, 3))), "character")
})

test_that("x = list", {
  expect_inherits(repr(list(1)), "character")
})

test_that("x = NULL", {
  expect_inherits(repr(NULL), "character")
})

test_that("x = bbox", {
  sim_pu_polygons <- get_sim_pu_polygons()
  expect_inherits(repr(sf::st_bbox(sim_pu_polygons)), "character")
})

test_that("x = crs", {
  expect_inherits(repr(sf::st_crs(4326)), "character")
  expect_match(
    repr(sf::st_crs(4326)),
    "geodetic"
  )
  expect_equal(
    repr(sf::st_crs(NA)),
    "NA (unknown)"
  )
  expect_equal(
    repr(get_crs(terra::rast(matrix(1), crs = ""))),
    "NA (unknown)"
  )
  expect_equal(
    repr(get_crs(get_sim_pu_polygons())),
    "Undefined Cartesian SRS (projected)"
  )
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  expect_match(
    repr(get_crs(prioritizrdata::get_wa_pu())),
    "+proj=laea +lat_0=45",
    fixed = TRUE
  )
})

test_that("x = phylo", {
  sim_phylogeny <- get_sim_phylogeny()
  expect_inherits(repr(sim_phylogeny), "character")
})

test_that("x = ConservationModifier", {
  expect_inherits(repr(ConservationModifier$new()), "character")
})

test_that("x = ConservationProblem", {
  expect_inherits(repr(ConservationProblem$new(list())), "character")
})

test_that("repr_cost", {
  # constant
  x <- repr_cost(c(4, 4, 4, 4))
  expect_inherits(x, "character")
  expect_match(x, "constant")
  expect_match(x, "4")
  # binary
  x <- repr_cost(c(0, 1, 0, 1))
  expect_inherits(x, "character")
  expect_match(x, "binary")
  # continuous
  x <- repr_cost(c(-6, 5, 1, 4))
  expect_inherits(x, "character")
  expect_match(x, "continuous")
  expect_match(x, "-6")
  expect_match(x, "5")
})
