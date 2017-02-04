context('OptimizationProblem')

test_that('new problem', {
  # data
  x <- OptimizationProblem$new()
  # tests
  expect_equal(ncell(x), 0)
})

test_that('get methods', {
  # data
  l <- list(
    modelsense='min',
    A_i = c(1,2,3),
    A_j = c(4,5,6),
    A_x = c(7,8,9),
    obj = c(9,10,11),
    lb = c(12,13,14),
    ub = c(15,16,17),
    rhs = c(18,19,20),
    sense = c('=', '=', '='),
    vtype = c('b', 's', 'c'),
    pu_indices_in_obj = c(21,22,23),
    row_ids = c('a', 'b', 'c'),
    col_ids = c('d', 'e', 'f'))
  x <- rcpp_predefined_optimization_problem(l)
  # tests
  expect_equal(rcpp_get_optimization_problem_nrow(x), max(l$A_i))
  expect_equal(rcpp_get_optimization_problem_ncol(x), max(l$A_j))
  expect_equal(rcpp_get_optimization_problem_ncell(x), length(l$A_x))
  expect_equal(rcpp_get_optimization_problem_A(x), 
              list(i=l$A_i, j=l$A_j, x=l$A_x))
  expect_equal(rcpp_get_optimization_problem_modelsense(x), l$modelsense)
  expect_equal(rcpp_get_optimization_problem_obj(x), l$obj)
  expect_equal(rcpp_get_optimization_problem_rhs(x), l$rhs)
  expect_equal(rcpp_get_optimization_problem_sense(x), l$sense)
  expect_equal(rcpp_get_optimization_problem_lb(x), l$lb)
  expect_equal(rcpp_get_optimization_problem_ub(x), l$ub)
  expect_equal(rcpp_get_optimization_problem_pu_indices_in_obj(x),
               l$pu_indices_in_obj)
  expect_equal(rcpp_get_optimization_problem_col_ids(x), l$col_ids)
  expect_equal(rcpp_get_optimization_problem_row_ids(x), l$row_ids)
})
