test_that("compile (data.frame, no status column)", {
  skip_if_not_installed("vroom")
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

test_that("compile (data.frame, absolute targets)", {
  skip_if_not_installed("vroom")
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

test_that("compile (data.frame, boundary penalties)", {
  skip_if_not_installed("vroom")
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

test_that("compile (data.frame, no boundary penalties)", {
  skip_if_not_installed("vroom")
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

test_that("solve (data.frame, boundary penalties)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("vroom")
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

test_that("solve (data.frame, no boundary penalties)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("vroom")
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

test_that("compile (data.frame, asymmetric connectivity data)", {
  skip_if_not_installed("vroom")
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

test_that("solve (data.frame, asymmetric connectivity data)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("vroom")
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

test_that("compile (character, boundary penalties)", {
  skip_if_not_installed("vroom")
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

test_that("compile (character, absolute INPUTDIR path)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("vroom")
  # set up input.dat with absolute file paths
  path <- file.path(tempfile(fileext = ".dat"))
  f <- readLines(
    system.file("extdata/marxan/input.dat", package = "prioritizr")
  )
  f[grep("INPUTDIR", f, fixed = TRUE)] <- paste("INPUTDIR",
    system.file("extdata/marxan/input", package = "prioritizr"))
  writeLines(f, path)
  # create problem
  p <- marxan_problem(path)
  # tests
  expect_inherits(p, "ConservationProblem")
})

test_that("compile (character, absolute file paths)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("vroom")
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
  p <- marxan_problem(path)
  # tests
  expect_inherits(p, "ConservationProblem")
})

test_that("invalid inputs (data.frame)", {
  skip_if_not_installed("vroom")
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
  expect_tidy_error(
    marxan_problem(NULL),
    "^.*x.*character.*frame.*$"
  )
  expect_tidy_error(
    marxan_problem("a"),
    "^.*a.*exist.*$"
  )
  expect_tidy_error(
    marxan_problem(p[, -1], s, pv, b, 5),
    "^.*pu.*name.*id.*$"
  )
  expect_tidy_error(
    marxan_problem(p[-1, ], s, pv, b, 5),
    "^.*bound.*id1.*values.*pu.*id.*$"
  )
  expect_tidy_error(
    marxan_problem(`[<-`(p, 1, 1, NA), s, pv, b, 5),
    "^.*pu.*id.*missing.*$"
  )
  expect_tidy_error(
    marxan_problem(p, s[-1, ], pv, b, 5),
    "^.*puvspr.*species.*values.*spec.*id.*$"
  )
  expect_tidy_error(
    marxan_problem(p, s[, -1], pv, b, 5),
    "^.*spec.*name.*id.*$"
  )
  expect_tidy_error(
    marxan_problem(p, `[<-`(s, 1, 1, NA), pv, b, 5),
    "^.*spec.*id.*missing.*$"
  )
  expect_tidy_error(
    marxan_problem(p, s, pv[, -1], b, 5),
    "^.*puvspr.*name.*species.*$"
  )
  expect_tidy_error(
    marxan_problem(p, s, `[<-`(pv, 1, 1, NA), b, 5),
    "^.*puvspr.*species.*missing.*$"
  )
  expect_tidy_error(
    marxan_problem(p, s, pv, b[, -1], 5),
    "^.*bound.*name.*id1.*$"
  )
  expect_tidy_error(
    marxan_problem(p, s, pv, `[<-`(b, 1, 1, NA), 5),
    "^.*bound.*id1.*missing.*$"
  )
  expect_tidy_error(
    marxan_problem(p, s, pv, b, NA_real_),
    "^.*blm.*missing.*$"
  )
  expect_tidy_error(
    marxan_problem(p, s, pv, b, c(5, 5)),
    "^.*blm.*number.*$"
  )
  expect_warning(
    marxan_problem(p, s, pv, NULL, 5),
    "^.*bound.*missing.*blm.*$"
  )
  expect_warning(
    marxan_problem(p, s, pv, NULL, 0, symmetric = FALSE),
    "^.*bound.*missing.*symmetric.*$"
  )
})

test_that("invalid inputs (character)", {
  skip_on_cran()
  skip_if_not_installed("vroom")
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

  # run tests for missing mandatory fields
  ## write data
  write_marxan_data(p, s, pv, b, 1, FALSE, tempdir())
  input_params <- readLines(paste0(tempdir(), "/input.dat"))
  ## PUNAME
  writeLines(
    input_params[!startsWith(input_params, "PUNAME")],
    paste0(tempdir(), "/input.dat")
  )
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*x.*missing.*PUNAME.*$"
  )
  ## SPECNAME
  writeLines(
    input_params[!startsWith(input_params, "SPECNAME")],
    paste0(tempdir(), "/input.dat")
  )
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*x.*missing.*SPECNAME.*$"
  )
  ## PUVSPRNAME
  writeLines(
    input_params[!startsWith(input_params, "PUVSPRNAME")],
    paste0(tempdir(), "/input.dat")
  )
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*x.*missing.*PUVSPRNAME.*$"
  )

  # run tests for invalid parameter values
  ## PUNAME
  write_marxan_data(p, s, pv, b, 1, FALSE, tempdir())
  unlink(paste0(tempdir(), "/pu.dat"))
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*PUNAME.*exist.*$"
  )
  ## SPECNAME
  write_marxan_data(p, s, pv, b, 1, FALSE, tempdir())
  unlink(paste0(tempdir(), "/spec.dat"))
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*SPECNAME.*exist.*$"
  )
  ## PUVSPRNAME
  write_marxan_data(p, s, pv, b, 1, FALSE, tempdir())
  unlink(paste0(tempdir(), "/puvspr.dat"))
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*PUVSPRNAME.*exist.*$"
  )
  ## BOUNDNAME
  write_marxan_data(p, s, pv, b, 1, FALSE, tempdir())
  unlink(paste0(tempdir(), "/bound.dat"))
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*BOUNDNAME.*exist.*$"
  )
  ## BLM
  write_marxan_data(p, s, pv, b, "a", FALSE, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*BLM.*number.*$"
  )
  ## ASSYMETRICCONNECTIVITY
  write_marxan_data(p, s, pv, b, 1, "a", tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*ASYMMETRICCONNECTIVITY.*number.*$"
  )

  # run tests for invalid data
  write_marxan_data(p[, -1], s, pv, b, 1, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*PUNAME.*pu.*name.*id.*$"
  )
  write_marxan_data(p[-1, ], s, pv, b, 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*BOUNDNAME.*PUNAME.*bound.*id1.*pu.*id.*$"
  )
  write_marxan_data(`[<-`(p, 1, 1, NA), s, pv, b, 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*PUNAME.*x.*missing.*$"
  )
  write_marxan_data(p, s[-1, ], pv, b, 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*PUVSPRNAME.*SPECNAME.*puvspr.*species.*spec.*id.*$"
  )
  write_marxan_data(p, s[, -1], pv, b, 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*SPECNAME.*x.*name.*id.*$"
  )
  write_marxan_data(p, `[<-`(s, 1, 1, NA), pv, b, 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*SPECNAME.*spec.*id.*missing.*$"
  )
  write_marxan_data(p, s, pv[, -1], b, 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*PUVSPRNAME.*puvspr.*name.*species.*$"
  )
  write_marxan_data(p, s, `[<-`(pv, 1, 1, NA), b, 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*PUVSPRNAME.*puvspr.*species.*missing.*$"
  )
  write_marxan_data(p, s, pv, b[, -1], 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*BOUNDNAME.*bound.*name.*id1.*$"
  )
  write_marxan_data(p, s, pv, `[<-`(b, 1, 1, NA), 5, 0, tempdir())
  expect_tidy_error(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*BOUNDNAME.*bound.*id1.*missing.*$"
  )

  # run tests for warnings
  write_marxan_data(p, s, pv, NULL, 1, 0, tempdir())
  unlink(paste0(tempdir(), "/bound.dat"))
  expect_warning(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*x.*missing.*BOUNDNAME.*BLM.*$"
  )
  write_marxan_data(p, s, pv, NULL, 0, 1, tempdir())
  unlink(paste0(tempdir(), "/bound.dat"))
  expect_warning(
    marxan_problem(paste0(tempdir(), "/input.dat")),
    "^.*x.*missing.*BOUNDNAME.*ASYMMETRICCONNECTIVITY.*$"
  )
})
