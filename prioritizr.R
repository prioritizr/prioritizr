## ----include = FALSE----------------------------------------------------------
# define dummy variables so that vignette passes package checks
tas_features <- raster::raster(matrix(1))


## ----include = FALSE----------------------------------------------------------
# define variables for vignette figures and code execution
h <- 3.5
w <- 3.5
is_check <-
  ("CheckExEnv" %in% search()) ||
  any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% names(Sys.getenv())) ||
  !identical(Sys.getenv("MY_UNIVERSE"), "")
knitr::opts_chunk$set(
  fig.align = "center",
  eval = !is_check,
  purl = !is_check,
  R.options = list(width = 80)
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
library(prioritizrdata)
library(prioritizr)
library(sf)
library(terra)
library(vegan)
library(cluster)

# load planning unit data
tas_pu <- get_tas_pu()

# load feature data
tas_features <- get_tas_features()


## ----fig.width = w, fig.height = h--------------------------------------------
# print planning unit data
print(tas_pu)

# plot map of planning unit costs
plot(st_as_sf(tas_pu[, "cost"]), main = "Planning unit costs")

# plot map of planning unit coverage by protected areas
plot(st_as_sf(tas_pu[, "locked_in"]), main = "Protected area coverage")


## ----fig.width = 4.5, fig.height = 4.5----------------------------------------
# print planning unit data
print(tas_features)

# plot map of the first four vegetation classes
plot(tas_features[[1:4]])


## ----fig.width = w, fig.height = h--------------------------------------------
# build problem
p1 <-
  problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 0.005) %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions()

# print problem
print(p1)


## ----fig.width = w, fig.height = h--------------------------------------------
# solve problem
s1 <- solve(p1)

# plot map of prioritization
plot(
  st_as_sf(s1[, "solution_1"]), main = "Prioritization",
  pal = c("grey90", "darkgreen")
)


## ----fig.width = 7------------------------------------------------------------
# create column with existing protected areas
tas_pu$pa <- round(tas_pu$locked_in)

# calculate feature representation statistics based on existing protected areas
tc_pa <- eval_target_coverage_summary(p1, tas_pu[, "pa"])
print(tc_pa)

# calculate  feature representation statistics based on the prioritization
tc_s1 <- eval_target_coverage_summary(p1, s1[, "solution_1"])
print(tc_s1)

# explore representation by existing protected areas
## calculate number of features adequately represented by existing protected
## areas
sum(tc_pa$met)

## summarize representation (values show percent coverage)
summary(tc_pa$relative_held * 100)

## visualize representation  (values show percent coverage)
hist(tc_pa$relative_held * 100,
     main = "Feature representation by existing protected areas",
     xlim = c(0, 100),
     xlab = "Percent coverage of features (%)")


# explore representation by prioritization
## summarize representation (values show percent coverage)
summary(tc_s1$relative_held * 100)

## calculate number of features adequately represented by the prioritization
sum(tc_s1$met)

## visualize representation  (values show percent coverage)
hist(
  tc_s1$relative_held * 100,
  main = "Feature representation by prioritization",
  xlim = c(0, 100),
  xlab = "Percent coverage of features (%)"
)


## ----fig.width = w, fig.height = h--------------------------------------------
# calculate irreplaceability
irrep_s1 <- eval_ferrier_importance(p1, s1["solution_1"])
print(irrep_s1)

# manually coerce values for planning units not selected in prioritization
# to NA, so that they are shown in white
irrep_s1$plot_total <- irrep_s1$total
irrep_s1$plot_total[s1$solution_1 < 0.5] <- NA_real_

# plot map of overall importance scores
plot(st_as_sf(irrep_s1[, "plot_total"]), main = "Overall importance")


## -----------------------------------------------------------------------------
# create new problem with a portfolio added to it
p2 <-
  p1 %>%
  add_gap_portfolio(number_solutions = 1000, pool_gap = 0.2)

# print problem
print(p2)

# generate prioritizations
prt <- solve(p2)
print(prt)


## ----fig.height = 4.5, fig.width = 7, fig.show = "hold"-----------------------
# extract solutions
prt_results <- sf::st_drop_geometry(prt)
prt_results <- prt_results[, startsWith(names(prt_results), "solution_")]

# calculate pair-wise distances between different prioritizations for analysis
prt_dists <- vegan::vegdist(t(prt_results), method = "jaccard", binary = TRUE)

# run cluster analysis
prt_clust <- hclust(as.dist(prt_dists), method = "average")

# visualize clusters
opar <- par()
par(oma = c(0, 0, 0, 0), mar= c(0, 4.1, 1.5, 2.1))
plot(
  prt_clust, labels = FALSE, sub = NA, xlab = "",
  main = "Different prioritizations in portfolio"
)
suppressWarnings(par(opar))


## ----fig.width = 7, fig.height = 5--------------------------------------------
# run k-medoids analysis
prt_med <- pam(prt_dists, k = 6)

# extract names of prioritizations that are most central for each group.
prt_med_names <- prt_med$medoids
print(prt_med_names)

# create a copy of prt and set values for locked in planning units to -1
# so we can easily visualize differences between prioritizations
prt2 <- prt[, prt_med_names]
prt2[which(tas_pu$locked_in > 0.5), prt_med_names] <- -1

# plot a map showing main different prioritizations
# dark grey: locked in planning units
# grey: planning units not selected
# green: selected planning units
plot(st_as_sf(prt2), pal = c("grey60", "grey90", "darkgreen"))


## -----------------------------------------------------------------------------

# import data
## planning unit data
pu_path <- system.file("extdata/marxan/input/pu.dat", package = "prioritizr")
pu_data <- read.csv(pu_path, header = TRUE, stringsAsFactors = FALSE)
print(head(pu_data))

## feature data
spec_path <- system.file(
  "extdata/marxan/input/spec.dat", package = "prioritizr"
)
spec_data <- read.csv(spec_path, header = TRUE, stringsAsFactors = FALSE)
print(head(spec_data))

## amount of each feature within each planning unit data
puvspr_path <- system.file(
  "extdata/marxan/input/puvspr.dat", package = "prioritizr"
)
puvspr_data <- read.csv(puvspr_path, header = TRUE, stringsAsFactors = FALSE)
print(head(puvspr_data))

## boundary data
bound_path <- system.file(
  "extdata/marxan/input/bound.dat", package = "prioritizr"
)
bound_data <- read.table(bound_path, header = TRUE, stringsAsFactors = FALSE)
print(head(bound_data))


## -----------------------------------------------------------------------------
# create problem
p3 <- marxan_problem(
  pu_data, spec_data, puvspr_data, bound_data, blm = 0.0005
)

# print problem
print(p3)

# solve problem
s3 <- solve(p3)

# print first six rows of solution object
print(head(s3))

