test_that("optimization_problem (x = NULL)", {
  # data
  x <- optimization_problem()
  suppressMessages(print(x))
  suppressMessages(x$print())
  suppressMessages(x$show())
  # tests
  expect_equal(x$ncell(), 0)
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
  x <- optimization_problem(l)
  suppressMessages(print(x))
  suppressMessages(x$print())
  suppressMessages(x$show())
  # tests
  expect_equal(x$nrow(), 2)
  expect_equal(x$ncol(), 3)
  expect_equal(x$ncell(), length(l$A_x))
  expect_equal(
    x$A(),
    Matrix::sparseMatrix(i = l$A_i, j = l$A_j, x = l$A_x, index1 = FALSE)
  )
  expect_equal(x$modelsense(), l$modelsense)
  expect_equal(x$obj(), l$obj)
  expect_equal(x$rhs(), l$rhs)
  expect_equal(x$sense(), l$sense)
  expect_equal(x$lb(), l$lb)
  expect_equal(x$ub(), l$ub)
  expect_equal(x$vtype(), l$vtype)
  expect_equal(number_of_features(x), l$number_of_features)
  expect_equal(number_of_planning_units(x), l$number_of_planning_units)
  expect_equal(number_of_zones(x), l$number_of_zones)
  expect_equal(x$col_ids(), l$col_ids)
  expect_equal(x$row_ids(), l$row_ids)
  expect_equal(x$compressed_formulation(), l$compressed_formulation)
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
  l2 <- as.list(optimization_problem(l))
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
  l <- list(
    modelsense = "min",
    number_of_features = 2,
    number_of_planning_units = 3,
    number_of_zones = 1,
    A_i = c(0L, 1L, 0L, 1L, 0L, 1L),
    A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
    A_x = c(2, 10, 1, 10, 1, 10),
    obj = c(1, 2, 2),
    lb = c(0, 1, 0),
    ub = c(0, 1, 1),
    rhs = c(2, 10),
    compressed_formulation = TRUE,
    sense = c(">=", ">="), vtype = c("B", "B", "B"),
    row_ids = c("spp_target", "spp_target"),
    col_ids = c("pu", "pu", "pu")
  )
  x <- optimization_problem(l)
  # shuffle columns
  shuffle_key <- c(3, 1, 2)
  reorder_key <- x$shuffle_columns(shuffle_key)
  # tests
  ## certain elements that should stay the same after shuffling
  expect_equal(x$nrow(), 2)
  expect_equal(x$ncol(), 3)
  expect_equal(x$ncell(), length(l$A_x))
  expect_equal(x$modelsense(), l$modelsense)
  expect_equal(x$rhs(), l$rhs)
  expect_equal(x$sense(), l$sense)
  expect_equal(number_of_planning_units(x), l$number_of_planning_units)
  expect_equal(number_of_features(x), l$number_of_features)
  expect_equal(number_of_zones(x), l$number_of_zones)
  expect_equal(x$row_ids(), l$row_ids)
  expect_equal(x$compressed_formulation(), l$compressed_formulation)
  ## certain elements that should change after shuffling
  expect_equal(x$obj()[reorder_key], l$obj)
  expect_equal(x$ub()[reorder_key], l$ub)
  expect_equal(x$lb()[reorder_key], l$lb)
  expect_equal(x$col_ids()[reorder_key], l$col_ids)
  expect_equal(x$vtype()[reorder_key], l$vtype)
  original_matrix <- Matrix::sparseMatrix(
    i = l$A_i, j = l$A_j, x = l$A_x, index1 = FALSE
  )
  expect_equal(x$A()[, reorder_key], original_matrix)
})

test_that("copy method", {
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
  x <- optimization_problem(l)
  # copy problem
  y <- x$copy()
  # tests
  ## verify y contians same data as x
  expect_equal(as.list(x), as.list(y))
  ## verify that changing y does not alter x
  x_list <- as.list(x)
  y$shuffle_columns(c(3, 2, 1))
  expect_equal(as.list(x), x_list)
})

test_that("set methods", {
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
  x <- optimization_problem(l)
  # obj
  new_obj <- c(100, 102, 111)
  x$set_obj(new_obj)
  expect_equal(x$obj(), new_obj)
  # model sense
  x$set_modelsense("max")
  expect_equal(x$modelsense(), "max")
  # lb
  new_lb <- c(-1, -5, -12)
  x$set_lb(new_lb)
  expect_equal(x$lb(), new_lb)
  # ub
  new_ub <- c(54, 12, 90)
  x$set_ub(new_ub)
  expect_equal(x$ub(), new_ub)
  # result
  l2 <- list(
    modelsense = "max",
    number_of_features = 2,
    number_of_planning_units = 3,
    number_of_zones = 1,
    A_i = c(0L, 1L, 1L),
    A_j = c(0L, 1L, 2L),
    A_x = c(7, 8, 9),
    obj = new_obj,
    lb = new_lb,
    ub = new_ub,
    rhs = c(18, 19),
    compressed_formulation = FALSE,
    sense = c("=", "="),
    vtype = c("B", "S", "C"),
    row_ids = c("a", "b"),
    col_ids = c("d", "e", "f")
  )
  expect_equal(as.list(x), l2)
})

test_that("append methods", {
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
  x <- optimization_problem(l)
  # single new constraint
  c1_A <- Matrix::sparseMatrix(
    i = c(1, 1), j = c(1, 3), x = c(90, -12), dims = c(1, 3), repr = "T"
  )
  c1_rhs <- 123
  c1_sense <- "="
  c1_row_ids <- "c1"
  x$append_linear_constraints(c1_rhs, c1_sense, c1_A, c1_row_ids)
  # multiple new constraints
  c2_A <- Matrix::sparseMatrix(
    i = c(1, 1, 2), j = c(3, 2, 1), x = c(12, 8, 45), dims = c(2, 3), repr = "T"
  )
  c2_rhs <- c(34, 12)
  c2_sense <- c("<=", ">=")
  c2_row_ids <- c("c2", "c3")
  x$append_linear_constraints(c2_rhs, c2_sense, c2_A, c2_row_ids)
  # tests
  expect_equal(
    x$rhs(),
    c(18, 19, c1_rhs, c2_rhs)
  )
  expect_equal(
    x$sense(),
    c("=", "=", c1_sense, c2_sense)
  )
  expect_equal(
    x$row_ids(),
    c("a", "b", c1_row_ids, c2_row_ids)
  )
  expect_equal(
    x$A(),
    rbind(
      Matrix::sparseMatrix(
        i = 1 + c(0L, 1L, 1L),
        j = 1 + c(0L, 1L, 2L),
        x = c(7, 8, 9),
        dims = c(2, 3)
      ),
      c1_A,
      c2_A
    )
  )
})

test_that("remove methods", {
  # data
  l <- list(
    modelsense = "min",
    A_i = c(0L, 1L, 1L, 2L, 2L, 2L),
    A_j = c(0L, 1L, 2L, 0L, 1L, 2L),
    A_x = c(7, 8, 9, 100, 101, 105),
    obj = c(9, 10, 11),
    lb = c(12, 13, 14),
    ub = c(15, 16, 17),
    rhs = c(18, 19, 200),
    number_of_features = 2,
    number_of_planning_units = 3,
    number_of_zones = 1,
    sense = c("=", "=", ">="),
    vtype = c("B", "S", "C"),
    row_ids = c("a", "b", "dd"),
    col_ids = c("d", "e", "f"),
    compressed_formulation = FALSE)
  x <- optimization_problem(l)
  # remove last constraint
  x$remove_last_linear_constraint()
  # tests
  l2 <- list(
    modelsense = "min",
    number_of_features = 2,
    number_of_planning_units = 3,
    number_of_zones = 1,
    A_i = c(0L, 1L, 1L),
    A_j = c(0L, 1L, 2L),
    A_x = c(7, 8, 9),
    obj = c(9, 10, 11),
    lb = c(12, 13, 14),
    ub = c(15, 16, 17),
    rhs = c(18, 19),
    compressed_formulation = FALSE,
    sense = c("=", "="),
    vtype = c("B", "S", "C"),
    row_ids = c("a", "b"),
    col_ids = c("d", "e", "f")
  )
  expect_equal(as.list(x), l2)
})
