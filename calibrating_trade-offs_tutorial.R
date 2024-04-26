## ----include = FALSE----------------------------------------------------------
# define dummy variables so that vignette passes package checks
prelim_penalty <- rep(NA_real_, 100)
threshold <- rep(NA_real_, 100)
topsis_results <- data.frame(
  alt.row = seq_len(3), score = runif(3), rank = seq_len(3)
)


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
library(prioritizrdata)
library(prioritizr)
library(sf)
library(terra)
library(dplyr)
library(tibble)
library(ggplot2)
library(topsis)
library(withr)

# set seed
set.seed(500)

# load planning unit data
tas_pu <- get_tas_pu()
print(tas_pu)

# load feature data
tas_features <- get_tas_features()
print(tas_features)


## ----fig.width = w, fig.height = h--------------------------------------------
# plot map of planning unit costs
plot(tas_pu[, "cost"])

# plot map of planning unit statuses
plot(tas_pu[, "locked_in"])


## ----fig.width = 4.5, fig.height = 4.5----------------------------------------
# plot map of the first four vegetation classes
plot(tas_features[[1:4]])


## -----------------------------------------------------------------------------
# define a problem
p0 <-
  problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions()

# print problem
print(p0)

# solve problem
s0 <- solve(p0)

# print result
print(s0)


## ----fig.width = w, fig.height = h, results = "hide"--------------------------
# create column for making a map of the prioritization
s0$map_1 <- case_when(
  s0$locked_in > 0.5 ~ "locked in",
  s0$solution_1 > 0.5 ~ "priority",
  TRUE ~ "other"
)

# plot map of prioritization
plot(
  s0[, "map_1"], pal = c("purple", "grey90", "darkgreen"),
  main = NULL, key.pos = 1
)


## ----fig.width = w, fig.height = h--------------------------------------------
# set costs for planning units covered by existing protected areas to zero
tas_pu$cost[tas_pu$locked_in > 0.5] <- 0

# plot map of planning unit costs
plot(tas_pu[, "cost"])


## -----------------------------------------------------------------------------
# generate boundary length data for the planning units
tas_bd <- boundary_matrix(tas_pu)

# manually re-scale the boundary length values
tas_bd <- rescale_matrix(tas_bd)


## -----------------------------------------------------------------------------
# define a range of different penalty values
## note that we use a power scale to avoid focusing on very high penalty values
prelim_lower <- -5  # change this for your own data
prelim_upper <- 1.75 # change this for your own data
prelim_penalty <- round(10^seq(prelim_lower, prelim_upper, length.out = 9), 5)

# print penalty values
print(prelim_penalty)


## ----results = "hide"---------------------------------------------------------
# define a problem without boundary penalties
p0 <-
  problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions()

# generate preliminary prioritizations based on each penalty
## note that we specify a relaxed gap and time limit for the solver
prelim_blended_results <- lapply(prelim_penalty, function(x) {
  s <-
    p0 %>%
    add_boundary_penalties(penalty = x, data = tas_bd) %>%
    add_default_solver(gap = 0.2, time_limit = 10 * 60) %>%
    solve()
  s <- data.frame(s = s$solution_1)
  names(s) <- with_options(list(scipen = 30), paste0("penalty_", x))
  s
})

# format results as a single spatial object
prelim_blended_results <- cbind(
  tas_pu, do.call(bind_cols, prelim_blended_results)
)

# preview results
print(prelim_blended_results)


## ----fig.width = 7, fig.height = 5.0------------------------------------------
# plot maps of prioritizations
plot(
  x =
    prelim_blended_results %>%
    dplyr::select(starts_with("penalty_")) %>%
    mutate_if(is.numeric, function(x) {
      case_when(
        prelim_blended_results$locked_in > 0.5 ~ "locked in",
        x > 0.5 ~ "priority",
        TRUE ~ "other"
      )
    }),
  pal = c("purple", "grey90", "darkgreen")
)


## ----"verify-correct-best-guess", echo = FALSE--------------------------------
# this code is used to ensure consistency between the results and the
# text in the vignette. Specifically, the text is written assuming that
# the 7th solution in the preliminary set of prioritizations is the
# prioritization that (1) was generated with the lowest penalty value and
# (2) has all planning units selected

# preliminary calculations
result_nms <- names(prelim_blended_results)
result_nms <- result_nms[startsWith(result_nms, "penalty_")]
result_mtx <- sf::st_drop_geometry(prelim_blended_results)[, result_nms]
result_n_selected <- apply(result_mtx, 2, sum, na.rm = TRUE)

# compute best guess
best_guess_idx <- min(which(result_n_selected == max(result_n_selected)))

# run checks
invisible(
    assertthat::assert_that(
    best_guess_idx == 9,
    msg = paste(
      "inconsistency between results and text,",
      "index of best guess index should be",
      best_guess_idx
    )
  )
)


## ----fig.width = 7, fig.height = 7.5, results = "hide"------------------------
# define a new set of penalty values
penalty <- round(10^seq(-5, log10(prelim_penalty[9]), length.out = 9), 5)

# generate prioritizations based on each penalty
blended_results <- lapply(penalty, function(x) {
  ## generate solution
  s <-
    p0 %>%
    add_boundary_penalties(penalty = x, data = tas_bd) %>%
    solve()
  ## return data frame with solution
  s <- data.frame(s = s$solution_1)
  names(s) <- with_options(list(scipen = 30), paste0("penalty_", x))
  s
})

# format results as a single spatial object
blended_results <- cbind(tas_pu, do.call(bind_cols, blended_results))

# plot maps of prioritizations
plot(
  x =
    blended_results %>%
    dplyr::select(starts_with("penalty_")) %>%
    mutate_if(is.numeric, function(x) {
      case_when(
        blended_results$locked_in > 0.5 ~ "locked in",
        x > 0.5 ~ "priority",
        TRUE ~ "other"
      )
    }),
  pal = c("purple", "grey90", "darkgreen")
)


## ----fig.width = w, fig.height = h, results = "hide"--------------------------
# define a problem without boundary penalties
p1 <-
  problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0)

# solve problem
s1 <- solve(p1)

# add column for making a map of the prioritization
s1$map_1 <- case_when(
  s1$locked_in > 0.5 ~ "locked in",
  s1$solution_1 > 0.5 ~ "priority",
  TRUE ~ "other"
)

# plot map of prioritization
plot(
  s1[, "map_1"], pal = c("purple", "grey90", "darkgreen"),
  main = NULL, key.pos = 1
)


## ----fig.width = w, fig.height = h--------------------------------------------
# calculate cost
s1_cost <- eval_cost_summary(p1, s1[, "solution_1"])$cost

# print cost
print(s1_cost)


## -----------------------------------------------------------------------------
# calculate cost threshold values
threshold <- s1_cost + (s1_cost * seq(1e-5, 4, length.out = 9))
threshold <- ceiling(threshold)

# print cost thresholds
print(threshold)


## ----fig.width = 7, fig.height = 5.0, results = "hide"------------------------
# add a column with zeros
tas_pu$zeros <- 0

# define a problem with zero cost values and boundary penalties
## note that because all the costs are all zero, it doesn't actually
## matter what penalty value is used (as long as the value is > 0)
## and so we just use a value of 1
p2 <-
  problem(tas_pu, tas_features, cost_column = "zeros") %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 1, data = tas_bd) %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions()

# generate prioritizations based on each cost threshold
## note that the prioritizations are solved to within 10% of optimality
## (the default gap) because the gap is not specified
hierarchical_results <- lapply(threshold, function(x) {
  ## generate solution by adding a constraint based on the threshold and
  ## using the "real" cost values (i.e., not zeros)
  s <-
    p2 %>%
    add_linear_constraints(threshold = x, sense = "<=", data = "cost") %>%
    solve()
  ## return data frame with solution
  s <- data.frame(s = s$solution_1)
  names(s) <- paste0("threshold_", x)
  s
})

# format results as a single spatial object
hierarchical_results <- cbind(tas_pu, do.call(bind_cols, hierarchical_results))

# plot maps of prioritizations
plot(
  x =
    hierarchical_results %>%
    dplyr::select(starts_with("threshold_")) %>%
    mutate_if(is.numeric, function(x) {
      case_when(
        hierarchical_results$locked_in > 0.5 ~ "locked in",
        x > 0.5 ~ "priority",
        TRUE ~ "other"
      )
    }),
  pal = c("purple", "grey90", "darkgreen")
)


## -----------------------------------------------------------------------------
# calculate metrics for prioritizations
## note that we use p0 and not p1 so that cost calculations are based
## on the cost values and not zeros
hierarchical_metrics <- lapply(
  grep("threshold_", names(hierarchical_results)), function(x) {
    x <- hierarchical_results[, x]
    data.frame(
      total_cost = eval_cost_summary(p0, x)$cost,
      total_boundary_length = eval_boundary_summary(p0, x)$boundary
    )
  }
)
hierarchical_metrics <- do.call(bind_rows, hierarchical_metrics)
hierarchical_metrics$threshold <- threshold
hierarchical_metrics <- as_tibble(hierarchical_metrics)

# preview metrics
print(hierarchical_metrics)


## -----------------------------------------------------------------------------
# create data for plotting
result_data <-
  hierarchical_metrics %>%
  ## rename threshold column to value column
  rename(value = "threshold") %>%
  ## add column with column names that contain candidate prioritizations
  mutate(name = grep(
    "threshold_", names(hierarchical_results), value = TRUE, fixed = TRUE
  )) %>%
  ## add column with labels for plotting
  mutate(label = paste("Threshold =", value)) %>%
  ## add column to keep track prioritizations selected by different methods
  mutate(method = "none")

# print table
print(result_data)


## ----fig.width = 7, fig.height = 5.0------------------------------------------
# create plot to visualize trade-offs and show selected candidate prioritization
result_plot <-
  ggplot(
    data = result_data,
    aes(x = total_boundary_length, y = total_cost, label = label)
  ) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(hjust = -0.15) +
  scale_color_manual(
    values = c("visual" = "blue", "not selected" ="black")
  ) +
  xlab("Total boundary length of prioritization") +
  ylab("Total cost of prioritization") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.4))) +
  theme(legend.title = element_blank())

# render plot
print(result_plot)


## -----------------------------------------------------------------------------
# specify prioritization selected by visual method
result_data$method[3] <- "visual"


## -----------------------------------------------------------------------------
# calculate TOPSIS scores
topsis_results <- topsis(
  decision =
    hierarchical_metrics %>%
    dplyr::select(total_cost, total_boundary_length) %>%
    as.matrix(),
  weights = c(1, 1),
  impacts = c("-", "-")
)

# print results
print(topsis_results)


## -----------------------------------------------------------------------------
# add column indicating prioritization selected by TOPSIS method
result_data$method[which.max(topsis_results$score)] <- "TOPSIS"


## ----results = "hide"---------------------------------------------------------
# generate ideal prioritization based on cost criteria
## note that this is simply the same as the s1 prioritization we generated
## for the hierarchical approach
p3 <-
  problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0)

# solve problem
s3 <- solve(p3)

# generate ideal prioritization based on spatial fragmentation criteria
## note that any non-zero penalty value would here,
## so we just use a penalty  of 1
p4 <-
  problem(tas_pu, tas_features, cost_column = "zeros") %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 1, data = tas_bd) %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0)

# solve problem
s4 <- solve(p4)


## -----------------------------------------------------------------------------
# generate problem formulation with costs and boundary penalties for
# calculating performance metrics
p5 <-
  problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 1, data = tas_bd) %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions()

# calculate performance metrics for ideal cost prioritization
s3_metrics <- tibble(
  total_cost = eval_cost_summary(p5, s3[, "solution_1"])$cost,
  total_boundary_length =
    eval_boundary_summary(p5, s3[, "solution_1"])$boundary
)

# calculate performance metrics for ideal boundary length prioritization
s4_metrics <- tibble(
  total_cost = eval_cost_summary(p5, s4[, "solution_1"])$cost,
  total_boundary_length =
    eval_boundary_summary(p5, s4[, "solution_1"])$boundary
)


## -----------------------------------------------------------------------------
# calculate penalty value based on Cohon et al. 1979
cohon_penalty <- abs(
  (s3_metrics$total_cost - s4_metrics$total_cost) /
  (s3_metrics$total_boundary_length - s4_metrics$total_boundary_length)
)

# round to 5 decimal places to avoid numerical issues during optimization
cohon_penalty <- round(cohon_penalty, 5)

# print penalty value
print(cohon_penalty)


## ----results = "hide"---------------------------------------------------------
# generate prioritization using penalty value calculated using Cohon et al. 1979
p6 <-
  problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = cohon_penalty, data = tas_bd) %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions()

# solve problem
s6 <- solve(p6)


## -----------------------------------------------------------------------------
# add new row with data for prioritization generated following Cohon et al. 1979
result_data <- bind_rows(
  result_data,
  tibble(
    total_cost = eval_cost_summary(p6, s6[, "solution_1"])$cost,
    total_boundary_length =
      eval_boundary_summary(p6, s6[, "solution_1"])$boundary,
    value = cohon_penalty,
    name = paste0("penalty_", cohon_penalty),
    label = paste0("Penalty = ",  cohon_penalty),
    method = "Cohon"
  )
)


## ----fig.width = 7, fig.height = 5.0------------------------------------------
# create plot to visualize trade-offs and show selected candidate prioritization
result_plot <-
  ggplot(
    data =
      result_data %>%
      mutate(vjust = if_else(method == "Cohon", -1, 0.5)),
    aes(x = total_boundary_length, y = total_cost, label = label)
  ) +
  geom_line() +
  geom_point(aes(color = method), size = 3) +
  geom_text(aes(vjust = vjust, color = method), hjust = -0.1) +
  scale_color_manual(
    name = "Method",
    values = c(
      "visual" = "#984ea3",
      "none" = "#000000",
      "TOPSIS" = "#e41a1c",
      "Cohon" = "#377eb8"
    )
  ) +
  xlab("Total boundary length of prioritization") +
  ylab("Total cost of prioritization") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.4)))

# render plot
print(result_plot)


## ----fig.width = 7.0, fig.height = h------------------------------------------
# extract column names for creating the prioritizations
visual_name <- result_data$name[[which(result_data$method == "visual")]]
topsis_name <- result_data$name[[which(result_data$method == "TOPSIS")]]

# create object with selected prioritizations
solutions  <- bind_cols(
  tas_pu,
  hierarchical_results %>%
    st_drop_geometry() %>%
    dplyr::select(all_of(c(visual_name, topsis_name))) %>%
    setNames(c("Visual", "TOPSIS")),
  s6 %>%
    st_drop_geometry() %>%
    dplyr::select(solution_1) %>%
    rename(Cohon = "solution_1")
)

# plot maps of selected prioritizations
plot(
  x =
    solutions %>%
    dplyr::select(Visual, TOPSIS, Cohon) %>%
    mutate_if(is.numeric, function(x) {
      case_when(
        hierarchical_results$locked_in > 0.5 ~ "locked in",
        x > 0.5 ~ "priority",
        TRUE ~ "other"
      )
    }),
  pal = c("purple", "grey90", "darkgreen")
)

