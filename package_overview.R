## ----include = FALSE----------------------------------------------------------
h = 3.2
w = 5.0
is_check <-
  ("CheckExEnv" %in% search()) ||
  any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% names(Sys.getenv())) ||
  !identical(Sys.getenv("MY_UNIVERSE"), "")
knitr::opts_chunk$set(
  fig.height = h,
  fig.width = w,
  fig.align = "center",
  eval = !is_check,
  root.dir = normalizePath("../..")
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


## ----results = "hide", message = FALSE----------------------------------------
# load package
library(prioritizr)


## -----------------------------------------------------------------------------
# load raster planning unit data
sim_pu_raster <- get_sim_pu_raster()

# print data
print(sim_pu_raster)

# plot data
plot(
  sim_pu_raster, main = "Planning unit costs",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## -----------------------------------------------------------------------------
# load polygon planning unit data
sim_pu_polygons <- get_sim_pu_polygons()

# print data
print(sim_pu_polygons)

# plot the planning units, and color them according to cost values
plot(sim_pu_polygons[, "cost"])


## -----------------------------------------------------------------------------
# specify file path for planning unit data
pu_path <- system.file("extdata/marxan/input/pu.dat", package = "prioritizr")

# load in the tabular planning unit data
# note that we use the data.table::fread function, as opposed to the read.csv
# function, because it is much faster
pu_dat <- data.table::fread(pu_path, data.table = FALSE)

# preview first six rows of the tabular planning unit data
# note that it has some extra columns other than id and cost as per the
# Marxan format
head(pu_dat)


## ----fig.width = 6.5, fig.height = 6.0----------------------------------------
# load feature data
sim_features <- get_sim_features()

# plot the distribution of suitable habitat for each feature
plot(
  sim_features, nr = 2,
  axes = FALSE, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)
)


## -----------------------------------------------------------------------------
# create problem
p1 <- problem(sim_pu_raster, sim_features)

# print problem
print(p1)

# print number of planning units
number_of_planning_units(p1)

# print number of features
number_of_features(p1)


## -----------------------------------------------------------------------------
# create problem with spatial vector data
# note that we have to specify which column in the attribute table contains
# the cost data
p2 <- problem(sim_pu_polygons, sim_features, cost_column = "cost")

# print problem
print(p2)


## -----------------------------------------------------------------------------
# set file path for feature data
spec_path <- system.file(
  "extdata/marxan/input/spec.dat", package = "prioritizr"
)

# load in feature data
spec_dat <- data.table::fread(spec_path, data.table = FALSE)

# print first six rows of the data
# note that it contains extra columns
head(spec_dat)

# set file path for planning unit vs. feature data
puvspr_path <- system.file(
  "extdata/marxan/input/puvspr.dat", package = "prioritizr"
)

# load in planning unit vs feature data
puvspr_dat <- data.table::fread(puvspr_path, data.table = FALSE)

# print first six rows of the data
head(puvspr_dat)

# create problem
p3 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvspr_dat)

# print problem
print(p3)


## -----------------------------------------------------------------------------
# create a new problem that has the minimum set objective
p3 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective()

# print problem
print(p3)


## -----------------------------------------------------------------------------
# create a new problem that has the maximum coverage objective and a budget
# of 5000
p4 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_cover_objective(5000)

# print problem
print(p4)


## -----------------------------------------------------------------------------
# create a new problem that has the maximum features objective and a budget
# of 5000
p5 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_features_objective(budget = 5000)

# print problem
print(p5)


## -----------------------------------------------------------------------------
# create a new problem that has the minimum shortfall objective and a budget
# of 5000
p6 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_shortfall_objective(budget = 5000)

# print problem
print(p6)


## -----------------------------------------------------------------------------
# create a new problem that has the minimum largest shortfall objective and a
# budget of 5000
p7 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_largest_shortfall_objective(budget = 5000)

# print problem
print(p7)


## -----------------------------------------------------------------------------
# load simulated phylogeny data
sim_phylogeny <- get_sim_phylogeny()

# create a new problem that has the maximum phylogenetic diversity
# objective and a budget of 5000
p8 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_phylo_div_objective(budget = 5000, tree = sim_phylogeny)

# print problem
print(p8)


## -----------------------------------------------------------------------------
# load simulated phylogeny data
sim_phylogeny <- get_sim_phylogeny()

# create a new problem that has the maximum phylogenetic diversity
# objective and a budget of 5000
p9 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_phylo_end_objective(budget = 5000, tree = sim_phylogeny)

# print problem
print(p9)


## -----------------------------------------------------------------------------
# create a new problem that has the maximum utility objective and a budget
# of 5000
p10 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_utility_objective(budget = 5000)

# print problem
print(p10)


## -----------------------------------------------------------------------------
# create a problem with targets which specify that the solution must conserve
# a sum total of 3 units of suitable habitat for each feature
p11 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_absolute_targets(3)

# print problem
print(p11)


## -----------------------------------------------------------------------------
# create a problem with the minimum set objective and relative targets of 10 %
# for each feature
p12 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1)

# print problem
print(p12)

## -----------------------------------------------------------------------------
# create a problem with targets which specify that we need 10 % of the habitat
# for the first feature, 15 % for the second feature, 20 % for the third feature
# 25 % for the fourth feature and 30 % of the habitat for the fifth feature
targets <- c(0.1, 0.15, 0.2, 0.25, 0.3)
p13 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets)

# print problem
print(p13)


## -----------------------------------------------------------------------------
# create problem with added log-linear targets
p14 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_loglinear_targets(10, 0.9, 100, 0.2)

# print problem
print(p14)


## -----------------------------------------------------------------------------
# create problem with constraints which specify that the first planning unit
# must be selected in the solution
p15 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_locked_in_constraints(1)

# print problem
print(p15)


## -----------------------------------------------------------------------------
# create problem with constraints which specify that the second planning unit
# must not be selected in the solution
p16 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_locked_out_constraints(2)

# print problem
print(p16)


## -----------------------------------------------------------------------------
# create problem with constraints which specify that all selected planning units
# in the solution must have at least 1 neighbor
p17 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_neighbor_constraints(1)

# print problem
print(p17)


## -----------------------------------------------------------------------------
# create problem with constraints which specify that all selected planning units
# in the solution must form a single contiguous unit
p18 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_contiguity_constraints()

# print problem
print(p18)


## -----------------------------------------------------------------------------
# create problem with constraints which specify that the planning units used
# to conserve each feature must form a contiguous unit
p19 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_feature_contiguity_constraints()

# print problem
print(p19)


## -----------------------------------------------------------------------------
# create problem with constraints which specify that the sum of
# values in sim_features[[1]] among selected planning units must not exceed a
# threshold value of 190.
p20 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_shortfall_objective(budget = 1800) %>%
  add_relative_targets(0.1) %>%
  add_linear_constraints(190, "<=", sim_features[[1]])

# print problem
print(p20)


## ----fig.width = 6.5, fig.height = 2.5----------------------------------------
# load data to lock in or lock out planning units
sim_locked_in_raster <- get_sim_locked_in_raster()
sim_locked_out_raster <- get_sim_locked_out_raster()

# plot the locked data
plot(
  c(sim_locked_in_raster, sim_locked_out_raster),
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1),
  axes = FALSE, main = c("Locked in", "Locked out")
)

# create a problem using raster planning unit data and use the locked raster
# data to lock in some planning units and lock out some other planning units
p21 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_locked_in_constraints(sim_locked_in_raster) %>%
  add_locked_out_constraints(sim_locked_out_raster)

# print problem
print(p21)


## -----------------------------------------------------------------------------
# preview sim_pu_polygons object
print(sim_pu_polygons)

# specify locked in data using the field name
p22 <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_locked_in_constraints("locked_in")

# print problem
print(p22)

## -----------------------------------------------------------------------------
# specify locked in data using the values in the field
p23 <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_locked_in_constraints(which(sim_pu_polygons$locked_in))

# print problem
print(p23)


## -----------------------------------------------------------------------------
# create problem with penalties that penalize fragmented solutions with a
# penalty factor of 0.01
p24 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 0.01)

# print problem
print(p24)


## -----------------------------------------------------------------------------
# create problem with penalties for symmetric connectivity,
# here we will use only the first four layers in
# sim_features for the features and we will use the fifth layer in sim_features
# to represent the connectivity data, where the connectivity_matrix function
# will create a matrix showing the average strength of connectivity between
# adjacent planning units using the data in the fifth layer of sim_features
p25 <-
  problem(sim_pu_raster, sim_features[[1:4]]) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_connectivity_penalties(
    penalty = 5,
    data = connectivity_matrix(sim_pu_raster, sim_features[[5]])
  )

# print problem
print(p25)


## -----------------------------------------------------------------------------
# create a matrix containing asymmetric connectivity data,
asym_con_matrix <- matrix(
  runif(ncell(sim_pu_raster)^2),
  nrow = ncell(sim_pu_raster),
  ncol = ncell(sim_pu_raster)
)

# create problem with penalties for asymmetric connectivity
p26 <-
  problem(sim_pu_raster, sim_features[[1:4]]) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_asym_connectivity_penalties(penalty = 5, data = asym_con_matrix)

# print problem
print(p26)


## -----------------------------------------------------------------------------
# create data for penalizing planning units
pen_raster <- simulate_cost(sim_pu_raster)

# create problem with penalties that penalize solutions that select
# planning units with high values in the pen_raster object,
# here we will use a penalty value of 5 to indicate the trade-off (scaling)
# between the penalty values (in the sim_pu_raster) and the main objective
# (i.e., the cost of the solution)
p27 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_linear_penalties(penalty = 5, data = pen_raster)

# print problem
print(p27)


## -----------------------------------------------------------------------------
# add binary decisions to a problem
p28 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()

# print problem
print(p28)


## -----------------------------------------------------------------------------
# add proportion decisions to a problem
p29 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_proportion_decisions()

# print problem
print(p29)


## -----------------------------------------------------------------------------
# add semi-continuous decisions to a problem, where we can only manage at most
# 50 % of the area encompassed by a planning unit
p30 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_semicontinuous_decisions(0.5)

# print problem
print(p30)


## -----------------------------------------------------------------------------
# create a problem and specify that Gurobi should be used to solve the problem
# and specify an optimality gap of zero to obtain the optimal solution
p31 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0)

# print problem
print(p31)


## -----------------------------------------------------------------------------
# create a problem and specify that IBM CPLEX should be used to solve the
# problem and specify an optimality gap of zero to obtain the optimal solution
p32 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_cplex_solver(gap = 0)

# print problem
print(p32)


## -----------------------------------------------------------------------------
# create a problem and specify that CBC should be used to solve the
# problem and specify an optimality gap of zero to obtain the optimal solution
p33 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0)

# print problem
print(p33)


## -----------------------------------------------------------------------------
# create a problem and specify that HiGHS should be used to solve the
# problem and specify an optimality gap of zero to obtain the optimal solution
p34 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_highs_solver(gap = 0)

# print problem
print(p34)


## -----------------------------------------------------------------------------
# create a problem and specify that lpsymphony should be used to solve the
# problem and specify an optimality gap of zero to obtain the optimal solution
p35 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_lpsymphony_solver(gap = 0)

# print problem
print(p35)


## -----------------------------------------------------------------------------
# create a problem and specify that Rsymphony should be used to solve the
# problem and specify an optimality gap of zero to obtain the optimal solution
p36 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_rsymphony_solver(gap = 0)

# print problem
print(p36)


## -----------------------------------------------------------------------------
# create a problem and specify that a portfolio should be created by
# finding five solutions within 10% of optimality
p37 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_gap_portfolio(number_solutions = 5, pool_gap = 0.2)

# print problem
print(p37)


## -----------------------------------------------------------------------------
# create a problem and specify that a portfolio should be created using
# the top five solutions
p38 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_top_portfolio(number_solutions = 5)

# print problem
print(p38)


## -----------------------------------------------------------------------------
# create a problem and specify that a portfolio should be created using
# extra solutions found while solving the problem
p39 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_extra_portfolio()

# print problem
print(p39)


## -----------------------------------------------------------------------------
# create a problem and specify that a portfolio containing 10 solutions
# should be created using using Bender's cuts
p40 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_cuts_portfolio(number_solutions = 10)

# print problem
print(p40)


## -----------------------------------------------------------------------------
# create a problem and specify a portfolio should be created that contains
# 10 solutions and that any duplicate solutions should not be removed
p41 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_shuffle_portfolio(number_solutions = 10, remove_duplicates = FALSE)

# print problem
print(p41)


## -----------------------------------------------------------------------------
# formulate the problem
p42 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 500, edge_factor = 0.5) %>%
  add_binary_decisions()

# print problem
print(p42)

# solve the problem (using the default solver)
s42 <- solve(p42)

# plot solution
plot(
  s42, col = c("grey90", "darkgreen"), main = "Solution",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## -----------------------------------------------------------------------------
# extract the objective (numerical value being minimized or maximized)
print(attr(s42, "objective"))

# extract time spent solving solution
print(attr(s42, "runtime"))

# extract state message from the solver that describes why this specific
# solution was returned
print(attr(s42, "status"))


## -----------------------------------------------------------------------------
# calculate statistic
eval_n_summary(p42, s42)


## -----------------------------------------------------------------------------
# calculate statistic
eval_cost_summary(p42, s42)


## -----------------------------------------------------------------------------
# calculate statistics
eval_feature_representation_summary(p42, s42)


## -----------------------------------------------------------------------------
# calculate statistics
eval_target_coverage_summary(p42, s42)


## -----------------------------------------------------------------------------
# calculate statistic
eval_boundary_summary(p42, s42)


## -----------------------------------------------------------------------------
# create symmetric connectivity data
# here we parametrize connectivity based on which planning units are
# adjacent to each other
cm <- adjacency_matrix(sim_pu_raster)

# calculate statistic
eval_connectivity_summary(p42, s42, data = cm)


## -----------------------------------------------------------------------------
# create asymmetric connectivity data
# here we parametrize connectivity by randomly simulating values
acm <- matrix(runif(ncell(sim_pu_raster) ^ 2), ncol = ncell(sim_pu_raster))

# calculate statistic
eval_asym_connectivity_summary(p42, s42, data = acm)


## ----fig.height = h, fig.width = w--------------------------------------------
# formulate the problem
p43 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()

# solve the problem
s43 <- solve(p43)

# plot solution
plot(
  s43, col = c("grey90", "darkgreen"), main = "Solution",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## ----fig.height = h, fig.width = w--------------------------------------------
# calculate replacement cost scores and make the solver quiet
rc43 <-
  p43 %>%
  add_default_solver(gap = 0, verbose = FALSE) %>%
  eval_replacement_importance(s43)

# plot replacement cost scores
plot(
  rc43, main = "Replacement cost scores",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## ----fig.height = h, fig.width = w--------------------------------------------
# calculate Ferrier scores and extract total score
fs43 <- eval_ferrier_importance(p43, s43)[["total"]]

# plot Ferrier scores
plot(
  fs43, main = "Ferrier scores",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## ----fig.height = h, fig.width = w--------------------------------------------
# calculate rarity weighted richness scores
rwr43 <- eval_rare_richness_importance(p43, s43)

# plot rarity weighted richness scores
plot(
  rwr43, main = "Rarity weighted richness scores",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## -----------------------------------------------------------------------------
# set file path for Marxan input file
minput <- system.file("extdata/marxan/input.dat", package = "prioritizr")

# read Marxan input file
mp <- marxan_problem(minput)

# print problem
print(mp)

# solve the problem
ms <- solve(mp)

# since the Marxan data was in a tabular format, the solution is also returned
# in a tabular format, so we will print the first six rows of the table
# containing the solution
head(ms)


## -----------------------------------------------------------------------------
# load data
pu <-
  system.file("extdata/marxan/input/pu.dat", package = "prioritizr") %>%
  read.table(sep = ",", header = TRUE)
features <-
  system.file("extdata/marxan/input/spec.dat", package = "prioritizr") %>%
  read.table(sep = ",", header = TRUE)
bound <-
  system.file("extdata/marxan/input/bound.dat", package = "prioritizr") %>%
  read.table(sep = "\t", header = TRUE)
rij <-
  system.file("extdata/marxan/input/puvspr.dat", package = "prioritizr") %>%
  read.table(sep = ",", header = TRUE)

# build Marxan problem using data.frame objects
mp2 <- marxan_problem(
  x = pu, spec = features, puvspr = rij, bound = bound, blm = 0
)

# print problem
print(mp2)

