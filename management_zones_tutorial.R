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
  eval = !is_check, purl = !is_check,
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


## ----results = "hide"---------------------------------------------------------
# load packages
library(prioritizr)
library(terra)
library(tibble)


## -----------------------------------------------------------------------------
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create targets for each of the five features
t1 <- rep(0.1, 5)

# build single-zone problem
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(t1) %>%
  add_binary_decisions()

# print problem
print(p1)

# solve problem
s1 <- solve(p1)

# calculate feature representation
r1 <- eval_feature_representation_summary(p1, s1)
print(r1)

# plot solution
plot(
  s1, main = "solution",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## -----------------------------------------------------------------------------
# create a matrix with the targets
# here each column corresponds to a different zone,
# each row corresponds to a different feature, and
# each cell value corresponds to the target
t2 <- matrix(NA, ncol = 2, nrow = 5)
t2[, 1] <- 0.1
t2[, 2] <- 0.05

# print targets
print(t2)

# create a zones object that contains the amount of each feature
# in each planning unit when it is allocated to each zone
# since our zones pertain to the same habitat data, we will
# specify the same habitat data for each zone
z2 <- zones("zone 1" = sim_features, "zone 2" = sim_features)

# print zones
print(z2)

# create a multi-layer raster with the planning unit data
## since our planning unit costs are the same for each zone,
## we will create a raster with two replicates of the cost data
pu2 <- c(sim_pu_raster, sim_pu_raster)

# print planning units
print(pu2)

# build two-zone problem
p2 <-
  problem(pu2, z2) %>%
  add_min_set_objective() %>%
  add_relative_targets(t2) %>%
  add_binary_decisions()

# print problem
print(p2)

# solve problem
s2 <- solve(p2)

# calculate feature representation
r2 <- eval_feature_representation_summary(p2, s2)
print(r2)

# plot solution
# here we use the category layer function to generate raster showing the zone
# that each planning unit was allocated. Specifically, pixels with the
# value 1 (yellow) are allocated to "zone 1" and pixels with the value 2 (green)
# are allocated to "zone 2". Units depicted in gray are not allocated
# to any zone.
plot(
  category_layer(s2), main = "solution",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## ----fig.width = 6.0----------------------------------------------------------
# create new planning unit and cost data
pu3 <- get_sim_zones_pu_raster()[[1:2]]

# plot cost data
plot(
  pu3, main = c("zone 1", "zone 2"),
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## -----------------------------------------------------------------------------
# create problem
p3 <-
  problem(pu3, zones(sim_features, sim_features * 0.5)) %>%
  add_min_set_objective() %>%
  add_relative_targets(t2) %>%
  add_binary_decisions()

# print problem
print(p3)

# solve problem
s3 <- solve(p3)

# calculate feature representation
r3 <- eval_feature_representation_summary(p3, s3)
print(r3)

# plot solution
plot(
  category_layer(s3), main = "solution",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## ----fig.width = 6.5----------------------------------------------------------
# create planning unit data with costs
pu4 <- c(
  as.int(!is.na(sim_pu_raster)) * 100,
  as.int(!is.na(sim_pu_raster)) * 300,
  as.int(!is.na(sim_pu_raster)) * 200
)
names(pu4) <- c("few traps", "many traps", "baiting")

# plot planning unit data
plot(
  pu4, nr = 1,
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), axes = FALSE
)


## -----------------------------------------------------------------------------
# create targets
t4 <- tibble(
  feature = names(sim_features),
  zone = list(names(pu4))[rep(1, 5)],
  target = rep(8, 5),
  type = rep("absolute", 5)
)

# print targets
print(t4)

# create problem
p4 <-
  problem(
    pu4,
    zones(
      sim_features * 0.1,
      sim_features * 0.5,
      sim_features * 0.8,
      zone_names = names(pu4),
      feature_names = names(sim_features)
    )
  ) %>%
  add_min_set_objective() %>%
  add_manual_targets(t4) %>%
  add_binary_decisions()

# print problem
print(p4)

# solve problem
s4 <- solve(p4)

# calculate feature representation
r4 <- eval_feature_representation_summary(p4, s4)
print(r4)

# plot solution
plot(category_layer(s4), main = "solution", axes = FALSE)


## -----------------------------------------------------------------------------
# create data.frame to specify that we cannot bait in the first 80 units
l5 <- tibble(pu = seq(1, 80), zone = "baiting", status = 0)

# preview locked data
head(l5)

# create problem
p5 <-
  problem(
    pu4,
    zones(
      sim_features * 0.1,
      sim_features * 0.5,
      sim_features * 0.8,
      zone_names = names(pu4),
      feature_names = names(sim_features)
    )
  ) %>%
  add_min_set_objective() %>%
  add_manual_targets(t4) %>%
  add_manual_locked_constraints(l5) %>%
  add_binary_decisions()

# print problem
print(p5)

# solve problem
s5 <- solve(p5)

# calculate feature representation
r5 <- eval_feature_representation_summary(p5, s5)
print(r5)

# plot solution
plot(category_layer(s5), main = "solution", axes = FALSE)


## -----------------------------------------------------------------------------
# create matrix that describes boundary penalties between planning units
# allocated to different zones
z6 <- diag(3)
z6[1, 2] <- 1
z6[2, 1] <- 1
colnames(z6) <- c("few traps", "many traps", "baiting")
rownames(z6) <- colnames(z6)

# print matrix
print(z6)

# create problem
p6 <-
  problem(
    pu4,
    zones(
      sim_features * 0.1,
      sim_features * 0.5,
      sim_features * 0.8,
      zone_names = names(pu4),
      feature_names = names(sim_features)
    )
  ) %>%
  add_min_set_objective() %>%
  add_manual_targets(t4) %>%
  add_manual_locked_constraints(l5) %>%
  add_boundary_penalties(penalty = 640, zones = z6) %>%
  add_binary_decisions()

# print problem
print(p6)

# solve problem
s6 <- solve(p6)

# calculate feature representation
r6 <- eval_feature_representation_summary(p6, s6)
print(r6)

# plot solution
plot(category_layer(s6), main = "solution", axes = FALSE)


## -----------------------------------------------------------------------------
# create matrix that describe boundary penalties between planning units
# allocated to different zones
p7 <-
  problem(
    pu4,
    zones(
      sim_features * 0.1,
      sim_features * 0.5,
      sim_features * 0.8,
      zone_names = names(pu4),
      feature_names = names(sim_features)
    )
  ) %>%
  add_min_set_objective() %>%
  add_manual_targets(t4) %>%
  add_mandatory_allocation_constraints() %>%
  add_manual_locked_constraints(l5) %>%
  add_boundary_penalties(penalty = 640, zones = z6) %>%
  add_binary_decisions()

# print problem
print(p7)

# solve problem
s7 <- solve(p7)

# calculate feature representation
r7 <- eval_feature_representation_summary(p7, s7)
print(r7)

# plot solution
plot(category_layer(s7), main = "solution", axes = FALSE)

