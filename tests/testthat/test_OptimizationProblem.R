context("OptimizationProblem")

test_that("new problem", {
  # data
  x <- new_optimization_problem()
  # tests
  expect_equal(ncell(x), 0)
})

test_that("get methods", {
  # data
  l <- list(
    modelsense = "min",
    A_i = c(0L, 1L, 1L),
    A_j = c(0L, 1L, 2L),
    A_x = c(7, 8, 9),
    obj = c(9, 10, 11),
    lb = c(12, 13, 14),
    ub = c(15, 16, 17),
    rhs = c(18, 19),
    number_of_features = 2,
    number_of_planning_units = 3,
    number_of_zones = 1,
    sense = c("=", "="),
    vtype = c("B", "S", "C"),
    row_ids = c("a", "b"),
    col_ids = c("d", "e", "f"),
    compressed_formulation = FALSE)
  x <- predefined_optimization_problem(l)
  # tests
  expect_equal(nrow(x), 2)
  expect_equal(ncol(x), 3)
  expect_equal(ncell(x), length(l$A_x))
  expect_equal(A(x), Matrix::sparseMatrix(i = l$A_i, j = l$A_j, x = l$A_x,
                                          index1 = FALSE))
  expect_equal(modelsense(x), l$modelsense)
  expect_equal(obj(x), l$obj)
  expect_equal(rhs(x), l$rhs)
  expect_equal(sense(x), l$sense)
  expect_equal(lb(x), l$lb)
  expect_equal(ub(x), l$ub)
  expect_equal(number_of_features(x), l$number_of_features)
  expect_equal(number_of_planning_units(x), l$number_of_planning_units)
  expect_equal(number_of_zones(x), l$number_of_zones)
  expect_equal(col_ids(x), l$col_ids)
  expect_equal(row_ids(x), l$row_ids)
  expect_equal(compressed_formulation(x), l$compressed_formulation)
})

test_that("as.list", {
  # data
  l <- list(
    modelsense = "min",
    A_i = c(0L, 1L, 1L),
    A_j = c(0L, 1L, 2L),
    A_x = c(7, 8, 9),
    obj = c(9, 10, 11),
    lb = c(12, 13, 14),
    ub = c(15, 16, 17),
    rhs = c(18, 19),
    number_of_features = 2,
    number_of_planning_units = 3,
    number_of_zones = 1,
    sense = c("=", "="),
    vtype = c("B", "S", "C"),
    row_ids = c("a", "b"),
    col_ids = c("d", "e", "f"),
    compressed_formulation = FALSE)
  l2 <- as.list(predefined_optimization_problem(l))
  # tests
  expect_equal(l$modelsense, l2$modelsense)
  expect_equal(l$A_i, l2$A_i)
  expect_equal(l$A_j, l2$A_j)
  expect_equal(l$A_x, l2$A_x)
  expect_equal(l$obj, l2$obj)
  expect_equal(l$lb, l2$lb)
  expect_equal(l$ub, l2$ub)
  expect_equal(l$rhs, l2$rhs)
  expect_equal(l$number_of_features, l2$number_of_features)
  expect_equal(l$number_of_planning_units, l2$number_of_planning_units)
  expect_equal(l$number_of_zones, l2$number_of_zones)
  expect_equal(l$sense, l2$sense)
  expect_equal(l$vtype, l2$vtype)
  expect_equal(l$row_ids, l2$row_ids)
  expect_equal(l$col_ids, l2$col_ids)
  expect_equal(l$compressed_formulation, l2$compressed_formulation)
})

test_that("shuffle_columns method", {
  # data
  set.seed(600)
  l <- list(modelsense = "min", number_of_features = 2,
           number_of_planning_units = 3, number_of_zones = 1,
           A_i = c(0L, 1L, 0L, 1L, 0L, 1L), A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
           A_x = c(2, 10, 1, 10, 1, 10), obj = c(1, 2, 2), lb = c(0, 1, 0),
           ub = c(0, 1, 1), rhs = c(2, 10), compressed_formulation = TRUE,
           sense = c(">=", ">="), vtype = c("B", "B", "B"),
           row_ids = c("spp_target", "spp_target"),
           col_ids = c("pu", "pu", "pu"))
  x <- predefined_optimization_problem(l)
  # shuffle columns
  key <- x$shuffle_columns()
  ## check that object integrity is maintained
  # elements that should stay the same after shuffling
  expect_equal(nrow(x), 2)
  expect_equal(ncol(x), 3)
  expect_equal(ncell(x), length(l$A_x))
  expect_equal(modelsense(x), l$modelsense)
  expect_equal(rhs(x), l$rhs)
  expect_equal(sense(x), l$sense)
  expect_equal(number_of_planning_units(x), l$number_of_planning_units)
  expect_equal(number_of_features(x), l$number_of_features)
  expect_equal(number_of_zones(x), l$number_of_zones)
  expect_equal(row_ids(x), l$row_ids)
  expect_equal(compressed_formulation(x), l$compressed_formulation)
  # elements that should change after shuffling
  expect_equal(obj(x)[key], l$obj)
  expect_equal(col_ids(x)[key], l$col_ids)
  expect_equal(vtype(x)[key], l$vtype)
  original_matrix <- Matrix::sparseMatrix(i = l$A_i, j = l$A_j, x = l$A_x,
                                          index1 = FALSE)
  expect_equal(A(x)[, key], original_matrix)
})

test_that("set_ub", {
  # data
  set.seed(600)
  l <- list(modelsense = "min", number_of_features = 2,
           number_of_planning_units = 3, number_of_zones = 1,
           A_i = c(0L, 1L, 0L, 1L, 0L, 1L), A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
           A_x = c(2, 10, 1, 10, 1, 10), obj = c(1, 2, 2), lb = c(0, 1, 0),
           ub = c(0, 1, 1), rhs = c(2, 10), compressed_formulation = TRUE,
           sense = c(">=", ">="), vtype = c("B", "B", "B"),
           row_ids = c("spp_target", "spp_target"),
           col_ids = c("pu", "pu", "pu"))
  x <- predefined_optimization_problem(l)
  # tests
  expect_equal(x$ub(), l$ub)
  x$set_ub(1, 5)
  expect_equal(x$ub(), c(5, 1, 1))
  x$set_ub(2, 6)
  expect_equal(x$ub(), c(5, 6, 1))
  x$set_ub(3, 7)
  expect_equal(x$ub(), c(5, 6, 7))
  x$set_ub(1, 2)
  expect_equal(x$ub(), c(2, 6, 7))
  x$set_ub(c(3, 1), c(56, 78))
  expect_equal(x$ub(), c(78, 6, 56))
  # errors
  expect_error(x$set_ub("a", 2))
  expect_error(x$set_ub(1, "a"))
  expect_error(x$set_ub(-1, "a"))
  expect_error(x$set_ub(1000, 2))
})

test_that("set_lb", {
  # data
  set.seed(600)
  l <- list(modelsense = "min", number_of_features = 2,
           number_of_planning_units = 3, number_of_zones = 1,
           A_i = c(0L, 1L, 0L, 1L, 0L, 1L), A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
           A_x = c(2, 10, 1, 10, 1, 10), obj = c(1, 2, 2), lb = c(0, 1, 0.5),
           ub = c(0, 1, 1), rhs = c(2, 10), compressed_formulation = TRUE,
           sense = c(">=", ">="), vtype = c("B", "B", "B"),
           row_ids = c("spp_target", "spp_target"),
           col_ids = c("pu", "pu", "pu"))
  x <- predefined_optimization_problem(l)
  # tests
  expect_equal(x$lb(), l$lb)
  x$set_lb(1, 5)
  expect_equal(x$lb(), c(5, 1, 0.5))
  x$set_lb(2, 6)
  expect_equal(x$lb(), c(5, 6, 0.5))
  x$set_lb(3, 7)
  expect_equal(x$lb(), c(5, 6, 7))
  x$set_lb(1, 2)
  expect_equal(x$lb(), c(2, 6, 7))
  x$set_lb(c(3, 1), c(56, 78))
  expect_equal(x$lb(), c(78, 6, 56))
  # errors
  expect_error(x$set_lb("a", 2))
  expect_error(x$set_lb(1, "a"))
  expect_error(x$set_lb(-1, "a"))
  expect_error(x$set_lb(1000, 2))
})
