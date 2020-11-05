context("cplexAPI interface")

test_that("LP", {
  skip_on_cran()
  skip_if_not_installed("cplexAPI")
  skip_if_not_installed("Rsymphony")
  # create problem
  set.seed(500)
  nv <- 100
  nc <- 30
  modelsense <- "min"
  obj <- runif(nv)
  A <- matrix(runif(nv * nc), ncol = nv, nrow = nc)
  sense <- sample(c("G", "L"), nc, prob = c(0.9, 0.1), replace = TRUE)
  rhs <- rowSums(A) * ifelse(sense == "G", 0.1, 0.95)
  vtype <- rep("C", nv)
  lb <- sample(c(0, 0.2), nv, replace = TRUE)
  ub <- sample(c(1, 0.8), nv, replace = TRUE)
  # generate solutions
  s1 <- Rsymphony::Rsymphony_solve_LP(
    obj = obj, mat = A, dir = ifelse(sense == "G", ">=", "<="),
    rhs = rhs, max = ifelse(modelsense == "max", TRUE, FALSE), types = vtype,
    bounds = list(lower = list(ind = seq_along(lb), val = lb),
                  upper = list(ind = seq_along(ub), val = ub)))
  s2 <- cplex(
    list(modelsense = modelsense,
         obj = obj,
         A = methods::as(A, "dgCMatrix"),
         A2 = cplex_matrix(methods::as(A, "dgCMatrix")),
         vtype = vtype, rhs = rhs, sense = sense, lb = lb, ub = ub),
    list(threads = 1, presolve = 1, gap = 0, time_limit = 1e+10, verbose = 1))
  # compare solutions
  expect_lte(max(abs(s1$objval - s2$objval)), 1e-5)
  expect_lte(max(abs(s1$solution - s2$x)), 1e-5)
  expect_true(names(s1$status) %in%
              c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND"))
  expect_equal(s2$status, "optimal")
})

test_that("ILP", {
  skip_on_cran()
  skip_if_not_installed("cplexAPI")
  skip_if_not_installed("Rsymphony")
  # create problem
  set.seed(500)
  nv <- 100
  nc <- 30
  modelsense <- "min"
  obj <- runif(nv)
  A <- matrix(runif(nv * nc), ncol = nv, nrow = nc)
  sense <- sample(c("G", "L"), nc, prob = c(0.9, 0.1), replace = TRUE)
  rhs <- rowSums(A) * ifelse(sense == "G", 0.1, 0.95)
  vtype <- rep("B", nv)
  lb <- rep(0, nv)
  ub <- rep(1, nv)
  idx_0 <- sample.int(nv, 10)
  idx_1 <- sample.int(nv, 10)
  idx_1 <- setdiff(idx_1, idx_0)
  lb[idx_0] <- 0
  ub[idx_0] <- 0
  lb[idx_1] <- 1
  ub[idx_1] <- 1
  # generate solutions
  s1 <- Rsymphony::Rsymphony_solve_LP(
    obj = obj, mat = A, dir = ifelse(sense == "G", ">=", "<="),
    rhs = rhs, max = ifelse(modelsense == "max", TRUE, FALSE), types = vtype,
    bounds = list(lower = list(ind = seq_along(lb), val = lb),
                  upper = list(ind = seq_along(ub), val = ub)))
  s2 <- cplex(
    list(modelsense = modelsense,
         obj = obj,
         A = methods::as(A, "dgCMatrix"),
         A2 = cplex_matrix(methods::as(A, "dgCMatrix")),
         vtype = vtype, rhs = rhs, sense = sense, lb = lb, ub = ub),
    list(threads = 1, presolve = 1, gap = 0, time_limit = 1e+10, verbose = 0))
  # compare solutions
  expect_lte(max(abs(s1$objval - s2$objval)), 1e-5)
  expect_lte(max(abs(s1$solution - s2$x)), 1e-5)
  expect_true(names(s1$status) %in%
              c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND"))
  expect_equal(s2$status, "integer optimal solution")
})

test_that("MILP", {
  skip_on_cran()
  skip_if_not_installed("cplexAPI")
  skip_if_not_installed("Rsymphony")
  # create problem
  set.seed(500)
  nv <- 100
  nc <- 30
  modelsense <- "min"
  obj <- runif(nv)
  A <- matrix(runif(nv * nc), ncol = nv, nrow = nc)
  sense <- sample(c("G", "L"), nc, prob = c(0.9, 0.1), replace = TRUE)
  rhs <- rowSums(A) * ifelse(sense == "G", 0.1, 0.95)
  vtype <- sample(c("B", "C"), nv, replace = TRUE)
  lb <- sample(c(0, 0.2), nv, replace = TRUE)
  ub <- sample(c(1, 0.8), nv, replace = TRUE)
  idx_0 <- sample.int(nv, 10)
  idx_1 <- sample.int(nv, 10)
  idx_1 <- setdiff(idx_1, idx_0)
  lb[idx_0] <- 0
  ub[idx_0] <- 0
  lb[idx_1] <- 1
  ub[idx_1] <- 1
  lb[vtype == "B"] <- round(lb[vtype == "B"])
  ub[vtype == "B"] <- round(ub[vtype == "B"])
  # generate solutions
  s1 <- Rsymphony::Rsymphony_solve_LP(
    obj = obj, mat = A, dir = ifelse(sense == "G", ">=", "<="),
    rhs = rhs, max = ifelse(modelsense == "max", TRUE, FALSE), types = vtype,
    bounds = list(lower = list(ind = seq_along(lb), val = lb),
                  upper = list(ind = seq_along(ub), val = ub)))
  s2 <- cplex(
    list(modelsense = modelsense,
         obj = obj,
         A = methods::as(A, "dgCMatrix"),
         A2 = cplex_matrix(methods::as(A, "dgCMatrix")),
         vtype = vtype, rhs = rhs, sense = sense, lb = lb, ub = ub),
    list(threads = 1, presolve = 1, gap = 0, time_limit = 1e+10, verbose = 1))
  # compare solutions
  expect_lte(max(abs(s1$objval - s2$objval)), 1e-5)
  expect_lte(max(abs(s1$solution - s2$x)), 1e-5)
  expect_true(names(s1$status) %in%
              c("TM_OPTIMAL_SOLUTION_FOUND", "PREP_OPTIMAL_SOLUTION_FOUND"))
  expect_equal(s2$status, "integer optimal solution")
})
