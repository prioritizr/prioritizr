## ----include = FALSE----------------------------------------------------------
h <- 4.5
w <- 4.5
is_check <-
  ("CheckExEnv" %in% search()) ||
  any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% names(Sys.getenv())) ||
  !identical(Sys.getenv("MY_UNIVERSE"), "")
knitr::opts_knit$set(global.par = TRUE)
knitr::opts_chunk$set(
  fig.align = "center", fig.width = 4.0, fig.height = 3.5,
  eval = !is_check, purl = !is_check
)


## ----include = FALSE----------------------------------------------------------
# set up print method
print <- function(x, ...) {
  if (inherits(x, "ConservationProblem")) {
    prioritizr::knit_print.ConservationProblem(x)
  } else if (inherits(x, "OptimizationProblem")) {
    prioritizr::knit_print.OptimizationProblem(x)
  } else {
    base::print(x)
  }
}


## ----message = FALSE----------------------------------------------------------
# load packages
library(prioritizr)
library(prioritizrdata)
library(sf)
library(terra)

# load planning unit data
salt_pu <- get_salt_pu()

# load biodiversity feature data
salt_features <- get_salt_features()

# load connectivity data
salt_con <- get_salt_con()


## -----------------------------------------------------------------------------
# aggregate data to coarser resolution
salt_pu <- aggregate(salt_pu, fact = 3)
salt_features <- aggregate(salt_features, fact = 3)
salt_con <- aggregate(salt_con, fact = 3)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## -----------------------------------------------------------------------------
# print planning unit data
print(salt_pu)

# plot map showing the planning units costs on a log-scale
plot(log(salt_pu), main = "Planning unit costs (log)", axes = FALSE)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## -----------------------------------------------------------------------------
# print feature data
print(salt_features)

# plot map showing the feature data
plot(salt_features, axes = FALSE)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1), oma = c(0, 0, 0, 0.5))


## -----------------------------------------------------------------------------
# print connectivity data
print(salt_con)

# plot map showing the connectivity data
plot(salt_con, axes = FALSE)


## -----------------------------------------------------------------------------
# create problem
p0 <-
  problem(salt_pu, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_binary_decisions() %>%
  add_default_solver()

# print problem
print(p0)


## ----results = "hide"---------------------------------------------------------
# solve problem
s0 <- solve(p0)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## -----------------------------------------------------------------------------
# print solution
print(s0)

# plot solution
plot(
  s0, main = "Baseline prioritization", axes = FALSE,
  type = "classes", col = c("grey70", "darkgreen")
)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with added neighbor constraints and solve it
s1 <-
  p0 %>%
  add_neighbor_constraints(k = 3) %>%
  solve()

# plot solutions
plot(
  c(s0, s1), main = c("baseline", "neighbors constraints"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with added contiguity constraints and solve it
s2 <-
  p0 %>%
  add_contiguity_constraints() %>%
  add_gurobi_solver(first_feasible = TRUE) %>%
  solve()

# plot solutions
plot(
  c(s0, s2), main = c("baseline", "contiguity constraints"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## -----------------------------------------------------------------------------
# compute threshold for constraints
## here we use a threshold of 30% of the total connectivity values
threshold <- global(salt_con, "sum", na.rm = TRUE)[[1]] * 0.3

# print threshold
print(threshold)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with added linear constraints and solve it
s3 <-
  p0 %>%
  add_linear_constraints(
    data = salt_con, threshold = threshold, sense = ">="
  ) %>%
  solve()

# plot solutions
plot(
  c(s0, s3), main = c("baseline", "linear constraints"),
   axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## -----------------------------------------------------------------------------
# calculate threshold limit
## here we set a threshold limit based on the median
threshold_limit <- global(
  salt_con, fun = quantile, na.rm = TRUE, probs = 0.5
)[[1]]

# convert continuous values to binary values
salt_con_binary <- round(salt_con >= threshold_limit)

# plot binary values
plot(salt_con_binary, main = "salt_con_binary", axes = FALSE)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with added linear constraints and solve it
## note that we use the original threshold computed before,
## to ensure the prioritization covers at least 30% of the total amount
## connectivity values
s4 <-
  p0 %>%
  add_linear_constraints(
    data = salt_con_binary, threshold = threshold, sense = ">="
  ) %>%
  solve()

# plot solutions
plot(
  c(s0, s4), main = c("baseline", "linear constraints (binary)"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----results = "hide"---------------------------------------------------------
# clamp continuous values using the threshold limit we computed before
salt_con_clamp <- salt_con
salt_con_clamp[salt_con <= threshold_limit] <- 0

# plot clamped values
plot(salt_con_clamp, main = "salt_con_clamp", axes = FALSE)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with added linear constraints and solve it
## note that we use the original threshold computed before,
## to ensure the prioritization covers at least 30% of the total amount
## connectivity values
s5 <-
  p0 %>%
  add_linear_constraints(
    data = salt_con_clamp, threshold = threshold, sense = ">="
  ) %>%
  solve()

# plot solutions
plot(
  c(s0, s5), main = c("baseline", "linear constraints (clamped)"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----results = "hide"---------------------------------------------------------
# compute threshold limit
threshold_limit2 <- global(
  salt_con, fun = quantile, na.rm = TRUE, probs = 0.7
)[[1]]

# clamp continuous values using the new threshold limit
salt_con_clamp2 <- salt_con
salt_con_clamp2[salt_con <= threshold_limit2] <- 0

# plot clamped values
plot(salt_con_clamp2, main = "salt_con_clamp", axes = FALSE)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with added linear constraints and solve it
## note that we use the original threshold computed before,
## to ensure the prioritization covers at least 30% of the total amount
## connectivity values
s6 <-
  p0 %>%
  add_linear_constraints(
    data = salt_con_clamp2, threshold = threshold, sense = ">="
  ) %>%
  solve()

# plot solutions
plot(
  c(s0, s6), main = c("baseline", "linear constraints (clamped 2)"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## -----------------------------------------------------------------------------
# precompute the boundary data
salt_boundary_data <- boundary_matrix(salt_pu)

# rescale boundary data
salt_boundary_data <- rescale_matrix(salt_boundary_data)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with added boundary penalties
s7 <-
  p0 %>%
  add_boundary_penalties(penalty = 0.001, data = salt_boundary_data) %>%
  solve()

# plot solutions
plot(
  c(s0, s7), main = c("baseline", "boundary penalties (0.001)"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with increased boundary penalties
s8 <-
  p0 %>%
  add_boundary_penalties(penalty = 10, data = salt_boundary_data) %>%
  solve()

# plot solutions
plot(
  c(s0, s8), main = c("baseline", "boundary penalties (10)"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## -----------------------------------------------------------------------------
# calculate cost of baseline prioritization
eval_cost_summary(p0, s0)

# calculate cost of prioritization with low boundary penalties (i.e., 0.001)
eval_cost_summary(p0, s7)

# calculate cost of prioritization high low boundary penalties (i.e., 0.1)
eval_cost_summary(p0, s8)


## -----------------------------------------------------------------------------
# compute connectivity scores
salt_con_scores <- connectivity_matrix(salt_pu, salt_con)

# rescale scores
salt_con_scores <- rescale_matrix(salt_con_scores)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with added connectivity penalties
s9 <-
  p0 %>%
  add_connectivity_penalties(penalty = 0.0001, data = salt_con_scores) %>%
  solve()

# plot solutions
plot(
  c(s0, s9), main = c("baseline", "connectivity penalties (0.001)"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)


## ----include = FALSE----------------------------------------------------------
par(mar = c(1.1, 4.1, 4.1, 2.1))


## ----fig.width = 7.0, results = "hide"----------------------------------------
# create problem with increased connectivity penalties
s10 <-
  p0 %>%
  add_connectivity_penalties(penalty = 0.0002, data = salt_con_scores) %>%
  solve()

# plot solutions
plot(
  c(s0, s10), main = c("baseline", "connectivity penalties (0.002)"),
  axes = FALSE, type = "classes", col = c("grey70", "darkgreen")
)

