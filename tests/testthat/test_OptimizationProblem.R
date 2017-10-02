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
    A_i = c(1, 2, 3),
    A_j = c(4, 5, 6),
    A_x = c(7, 8, 9),
    obj = c(9, 10, 11),
    lb = c(12, 13, 14),
    ub = c(15, 16, 17),
    rhs = c(18, 19, 20),
    number_of_features = 200,
    number_of_planning_units = 100,
    sense = c("=", "=", "="),
    vtype = c("b", "s", "c"),
    row_ids = c("a", "b", "c"),
    col_ids = c("d", "e", "f"),
    compressed_formulation = FALSE,
    planning_unit_indices = seq_len(500))
  x <- rcpp_predefined_optimization_problem(l)
  # tests
  expect_equal(rcpp_get_optimization_problem_nrow(x), 3)
  expect_equal(rcpp_get_optimization_problem_ncol(x), 3)
  expect_equal(rcpp_get_optimization_problem_ncell(x), length(l$A_x))
  expect_equal(rcpp_get_optimization_problem_planning_unit_indices(x),
               seq_len(500))
  expect_equal(rcpp_get_optimization_problem_A(x),
               list(i=l$A_i, j=l$A_j, x=l$A_x))
  expect_equal(rcpp_get_optimization_problem_modelsense(x), l$modelsense)
  expect_equal(rcpp_get_optimization_problem_obj(x), l$obj)
  expect_equal(rcpp_get_optimization_problem_rhs(x), l$rhs)
  expect_equal(rcpp_get_optimization_problem_sense(x), l$sense)
  expect_equal(rcpp_get_optimization_problem_lb(x), l$lb)
  expect_equal(rcpp_get_optimization_problem_ub(x), l$ub)
  expect_equal(rcpp_get_optimization_problem_number_of_features(x),
               l$number_of_features)
  expect_equal(rcpp_get_optimization_problem_number_of_planning_units(x),
               l$number_of_planning_units)
  expect_equal(rcpp_get_optimization_problem_col_ids(x), l$col_ids)
  expect_equal(rcpp_get_optimization_problem_row_ids(x), l$row_ids)
  expect_equal(rcpp_get_optimization_problem_compressed_formulation(x),
               l$compressed_formulation)
})

test_that("shuffle_columns method", {
  # data
  set.seed(600)
  l <- list(
    modelsense = "min",
    A_i = c(0L, 1L, 0L, 1L, 0L, 1L),
    A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
    A_x = c(2, 10, 1, 10, 1, 10),
    obj = c(1, 2, 2),
    lb = c(0, 1, 0),
    ub = c(0, 1, 1),
    rhs = c(2, 10),
    number_of_features = 2,
    number_of_planning_units = 3,
    sense = c(">=", ">="),
    vtype = c("B", "B", "B"),
    row_ids = c("spp_target", "spp_target"),
    col_ids = c("pu", "pu", "pu"),
    compressed_formulation = FALSE,
    planning_unit_indices = 0:2)
  x <- rcpp_predefined_optimization_problem(l)
  # shuffle columns
  rcpp_set_optimization_problem_shuffled(x)
  ## check that object integrity is maintained
  # elements that should stay the same after shuffling
  expect_equal(rcpp_get_optimization_problem_nrow(x), 2)
  expect_equal(rcpp_get_optimization_problem_ncol(x), 3)
  expect_equal(rcpp_get_optimization_problem_ncell(x), length(l$A_x))
  expect_equal(rcpp_get_optimization_problem_modelsense(x), l$modelsense)
  expect_equal(rcpp_get_optimization_problem_rhs(x), l$rhs)
  expect_equal(rcpp_get_optimization_problem_sense(x), l$sense)
  expect_equal(rcpp_get_optimization_problem_number_of_planning_units(x),
               l$number_of_planning_units)
  expect_equal(rcpp_get_optimization_problem_number_of_features(x),
               l$number_of_features)
  expect_equal(rcpp_get_optimization_problem_row_ids(x), l$row_ids)
  expect_equal(rcpp_get_optimization_problem_compressed_formulation(x),
               l$compressed_formulation)
  expect_equal(rcpp_get_optimization_problem_A(x)$i, l$A_i)
  expect_equal(rcpp_get_optimization_problem_A(x)$x, l$A_x)
  # elements that should change after shuffling
  expect_equal(rcpp_get_optimization_problem_planning_unit_indices(x),
               c(2L, 0L, 1L))
  expect_equal(rcpp_get_optimization_problem_obj(x), c(2, 2, 1))
  expect_equal(rcpp_get_optimization_problem_lb(x), c(1, 0, 0))
  expect_equal(rcpp_get_optimization_problem_ub(x), c(1, 1, 0))
  expect_equal(rcpp_get_optimization_problem_A(x)$j,
                c(2L, 2L, 0L, 0L, 1L, 1L))
  expect_equal(rcpp_get_optimization_problem_col_ids(x), c("pu", "pu", "pu"))
})
