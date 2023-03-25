test_that("character (compile)", {
  skip_if_not_installed("data.table")
  # import data
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  pu_data$locked_in <- pu_data$status == 2
  pu_data$locked_out <- pu_data$status == 3
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  bound_data <- read.table(
    file.path(wd, "bound.dat"), header = TRUE, sep = "\t"
  )
  # create marxan problem
  path <- system.file("extdata/marxan/input.dat", package = "prioritizr")
  p1 <- marxan_problem(path)
  o1 <- compile(p1)
  # create equivalent problem
  p2 <-
    problem(pu_data, spec_data, puvspr_data, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets("prop") %>%
    add_locked_in_constraints("locked_in") %>%
    add_locked_out_constraints("locked_out") %>%
    add_boundary_penalties(1, edge_factor = 1, data = bound_data) %>%
    add_binary_decisions()
  o2 <- compile(p2)
  # tests
  expect_equal(o1$obj(), o2$obj())
  expect_true(all(round(o1$A(), 6) == round(o2$A(), 6)))
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$vtype(), o2$vtype())
})

test_that("character (solve)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("data.table")
  # create marxan problem
  path <- system.file("extdata/marxan/input.dat", package = "prioritizr")
  p <-
    marxan_problem(path) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_inherits(s, "data.frame")
  expect_true("solution_1" %in% names(s))
  expect_true(is.numeric(s$solution_1))
})

test_that("data.frame (compile, no status column)", {
  skip_if_not_installed("data.table")
  # import data
  path <- system.file("extdata/marxan/input.dat", package = "prioritizr")
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  pu_data$status <- NULL
  spec_data <- read.table(
    file.path(wd, "spec.dat"), header = TRUE, sep = ","
  )
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  bound_data <- read.table(
    file.path(wd, "bound.dat"), header = TRUE, sep = "\t"
  )
  # create marxan problem
  p1 <- marxan_problem(pu_data, spec_data, puvspr_data, bound_data, blm = 1)
  o1 <- compile(p1)
  # create equivalent problem
  p2 <-
    problem(pu_data, spec_data, puvspr_data, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets("prop") %>%
    add_boundary_penalties(1, edge_factor = 1, data = bound_data) %>%
    add_binary_decisions()
  o2 <- compile(p2)
  #tests
  expect_equal(o1$obj(), o2$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$vtype(), o2$vtype())
})

test_that("data.frame (compile, absolute targets)", {
  skip_if_not_installed("data.table")
  # import data
  path <- system.file("extdata/marxan/input.dat", package = "prioritizr")
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  pu_data$status <- NULL
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  spec_data$prop <- NULL
  spec_data$amount <- seq_len(nrow(spec_data))
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  bound_data <- read.table(
    file.path(wd, "bound.dat"), header = TRUE, sep = "\t"
  )
   # create marxan problem
  p1 <- marxan_problem(pu_data, spec_data, puvspr_data, bound_data, blm = 1)
  o1 <- compile(p1)
  # create equivalent problem
  p2 <-
    problem(pu_data, spec_data, puvspr_data, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets("amount") %>%
    add_boundary_penalties(1, edge_factor = 1, data = bound_data) %>%
    add_binary_decisions()
  o2 <- compile(p2)
  # compare two problems
  expect_equal(o1$obj(), o2$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$vtype(), o2$vtype())
})

test_that("character (solve, absolute INPUTDIR path)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("data.table")
  # set up input.dat with absolute file paths
  path <- file.path(tempfile(fileext = ".dat"))
  f <- readLines(
    system.file("extdata/marxan/input.dat", package = "prioritizr")
  )
  f[grep("INPUTDIR", f, fixed = TRUE)] <- paste("INPUTDIR",
    system.file("extdata/marxan/input", package = "prioritizr"))
  writeLines(f, path)
  # create problem
  p <-
    marxan_problem(path) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_inherits(s, "data.frame")
  expect_true("solution_1" %in% names(s))
  expect_true(is.numeric(s$solution_1))
})

test_that("character (solve, absolute file paths)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("data.table")
  # set up input.dat with absolute file paths
  path <- file.path(tempfile(fileext = ".dat"))
  f <- readLines(
    system.file("extdata/marxan/input.dat", package = "prioritizr")
  )
  f[grep("INPUTDIR", f, fixed = TRUE)] <- ""
  f[grep("SPECNAME", f, fixed = TRUE)] <-
    paste(
      "SPECNAME",
      system.file("extdata/marxan/input/spec.dat", package = "prioritizr")
    )
  f[grep("PUNAME", f, fixed = TRUE)] <-
    paste(
      "PUNAME",
      system.file("extdata/marxan/input/pu.dat", package = "prioritizr")
    )
  f[grep("PUVSPRNAME", f, fixed = TRUE)] <- paste("PUVSPRNAME",
    system.file( "extdata/marxan/input/puvspr.dat", package = "prioritizr"))
  f[grep("BOUNDNAME", f, fixed = TRUE)] <-
    paste(
      "BOUNDNAME",
      system.file("extdata/marxan/input/bound.dat", package = "prioritizr")
    )
  writeLines(f, path)
  # create marxan problem
  p <-
    marxan_problem(path) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  # check that problem can be solved
  s <- solve(p)
  # tests
  expect_inherits(s, "data.frame")
  expect_true("solution_1" %in% names(s))
  expect_true(is.numeric(s$solution_1))
})

test_that("data.frame (compile, boundary penalties)", {
  skip_if_not_installed("data.table")
  # import data
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  bound_data <- read.table(
    file.path(wd, "bound.dat"), header = TRUE, sep = "\t"
  )
  # create marxan problem
  p1 <- marxan_problem(pu_data, spec_data, puvspr_data, bound_data, 3)
  o1 <- compile(p1)
  # create equivalent problem
  pu_data$locked_in <- pu_data$status == 2
  pu_data$locked_out <- pu_data$status == 3
  p2 <-
    problem(pu_data, spec_data, puvspr_data, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets("prop") %>%
    add_locked_in_constraints("locked_in") %>%
    add_locked_out_constraints("locked_out") %>%
    add_boundary_penalties(3, 1, data = bound_data) %>%
    add_binary_decisions()
  o2 <- compile(p2)
  # compare two problems
  expect_equal(o1$obj(), o2$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$vtype(), o2$vtype())
})

test_that("data.frame (compile, no boundary penalties)", {
  skip_if_not_installed("data.table")
  # import data
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  # create marxan problem
  p1 <- marxan_problem(pu_data, spec_data, puvspr_data)
  o1 <- compile(p1)
  # create equivalent problem
  pu_data$locked_in <- pu_data$status == 2
  pu_data$locked_out <- pu_data$status == 3
  p2 <-
    problem(pu_data, spec_data, puvspr_data, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets("prop") %>%
    add_locked_in_constraints("locked_in") %>%
    add_locked_out_constraints("locked_out") %>%
    add_binary_decisions()
  o2 <- compile(p2)
  # tests
  expect_equal(o1$obj(), o2$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$vtype(), o2$vtype())
})

test_that("data.frame (solve, boundary penalties)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("data.table")
  # import data
  path <- system.file("extdata/marxan/input.dat", package = "prioritizr")
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  bound_data <- read.table(
    file.path(wd, "bound.dat"), header = TRUE, sep = "\t"
  )
  # create marxan problem
  p <-
    marxan_problem(pu_data, spec_data, puvspr_data, bound_data, blm = 1) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  # check that problem can be solved
  s <- solve(p)
  # tests
  expect_inherits(s, "data.frame")
  expect_true("solution_1" %in% names(s))
  expect_true(is.numeric(s$solution_1))
})

test_that("data.frame (solve, no boundary penalties)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("data.table")
  # import data
  path <- system.file("extdata/marxan/input.dat", package = "prioritizr")
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  # remove name column from spec_data to verify works without names
  spec_data$name <- NULL
  # create marxan problem
  p <-
    marxan_problem(pu_data, spec_data, puvspr_data) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_inherits(s, "data.frame")
  expect_true("solution_1" %in% names(s))
  expect_true(is.numeric(s$solution_1))
})

test_that("data.frame (compile, asymmetric connectivity data)", {
  skip_if_not_installed("data.table")
  # import data
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  bound_data <- expand.grid(id1 = head(pu_data$id), id2 = head(pu_data$id))
  bound_data$boundary <- runif(nrow(bound_data))
  # create marxan problem
  p1 <- marxan_problem(
    pu_data, spec_data, puvspr_data,
    blm = 1, bound = bound_data, symmetric = FALSE
  )
  o1 <- compile(p1)
  # create equivalent problem
  pu_data$locked_in <- pu_data$status == 2
  pu_data$locked_out <- pu_data$status == 3
  p2 <-
    problem(pu_data, spec_data, puvspr_data, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets("prop") %>%
    add_locked_in_constraints("locked_in") %>%
    add_locked_out_constraints("locked_out") %>%
    add_asym_connectivity_penalties(1, data = bound_data) %>%
    add_binary_decisions()
  o2 <- compile(p2)
  # tests
  expect_equal(o1$obj(), o2$obj())
  expect_true(all(o1$A() == o2$A()))
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$modelsense(), o2$modelsense())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$row_ids(), o2$row_ids())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$vtype(), o2$vtype())
})

test_that("data.frame (solve, asymmetric connectivity data)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("data.table")
  # import data
  path <- system.file("extdata/marxan/input.dat", package = "prioritizr")
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  pu_data <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  spec_data <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  puvspr_data <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  bound_data <- expand.grid(id1 = head(pu_data$id), id2 = head(pu_data$id))
  bound_data$boundary <- runif(nrow(bound_data)) * 1000
  # create marxan problem
  p <-
    marxan_problem(
      pu_data, spec_data, puvspr_data, bound_data,
      blm = 1, symmetric = FALSE
    ) %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_inherits(s, "data.frame")
  expect_true("solution_1" %in% names(s))
  expect_true(is.numeric(s$solution_1))
})

test_that("invalid inputs", {
  skip_if_not_installed("data.table")
  # import data
  wd <- system.file("extdata/marxan/input", package = "prioritizr")
  p <- read.table(file.path(wd, "pu.dat"), header = TRUE, sep = ",")
  s <- read.table(file.path(wd, "spec.dat"), header = TRUE, sep = ",")
  pv <- read.table(
    file.path(wd, "puvspr.dat"), header = TRUE, sep = ","
  )
  b <- read.table(
    file.path(wd, "bound.dat"), header = TRUE, sep = "\t"
  )
  # run tests
  expect_tidy_error(marxan_problem(NULL))
  expect_tidy_error(marxan_problem("a"))
  expect_tidy_error(marxan_problem(p[, -1], s, pv, b, 5))
  expect_tidy_error(marxan_problem(p[-1, ], s, pv, b, 5))
  expect_tidy_error(marxan_problem(`[<-`(p, 1, 1, NA), s, pv, b, 5))
  expect_tidy_error(marxan_problem(p, s[-1, ], pv, b, 5))
  expect_tidy_error(marxan_problem(p, s[, -1], pv, b, 5))
  expect_tidy_error(marxan_problem(p, `[<-`(s, 1, 1, NA), pv, b, 5))
  expect_tidy_error(marxan_problem(p, s, pv[, -1], b, 5))
  expect_tidy_error(marxan_problem(p, s, `[<-`(pv, 1, 1, NA), b, 5))
  expect_tidy_error(marxan_problem(p, s, pv, b[, -1], 5))
  expect_tidy_error(marxan_problem(p, s, pv, `[<-`(b, 1, 1, NA), 5))
  expect_tidy_error(marxan_problem(p, s, pv, b, NA))
  expect_tidy_error(marxan_problem(p, s, pv, b, c(5, 5)))
  expect_warning(marxan_problem(p, s, pv, NULL, 5), "blm")
  expect_warning(
    marxan_problem(p, s, pv, NULL, 0, symmetric = FALSE),
    "symmetric"
  )
})
