## ----include = FALSE----------------------------------------------------------
w <- 7
h <- 4
is_check <-
  ("CheckExEnv" %in% search()) ||
  any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% names(Sys.getenv())) ||
  !identical(Sys.getenv("MY_UNIVERSE"), "")
knitr::opts_chunk$set(
  fig.align = "center",  fig.width = w, fig.height = h,
  eval = !is_check, purl = !is_check
)


## ----eval = TRUE, include = FALSE---------------------------------------------
# define variables for CRAN
n_planning_units <- rep(0, 100)
benchmark_results <- data.frame(time_limit = NA_real_)
boundary_penalty_values <- list(
  add_min_set_objective = rep(0, 100),
  add_min_shortfall_objective = rep(0, 100)
)


## ----"load packages", message = FALSE-----------------------------------------
# load packages
library(prioritizr)
library(piggyback)
library(ggplot2)
library(units)
library(dplyr)


## ----"import results", message = FALSE----------------------------------------
# download data to temporary folder
pb_download(
  file = c("solutions.zip", "results.rda"),
  repo = "prioritizr/benchmark", dest = tempdir(), tag = "latest",
  show_progress = FALSE
)

# load benchmark results
load(file.path(tempdir(), "results.rda"))



## ----"benchmark parameters"---------------------------------------------------
# numbers of planning units examined the benchmark analysis
n_planning_units <- sort(unique(benchmark_results$number_of_planning_units))
print(n_planning_units)

# number of features (e.g., number of different species examined)
sort(unique(benchmark_results$number_features))

# representation targets,
# units are proportion of the total amount of each feature (e.g., 0.1 = 10%)
sort(unique(benchmark_results$relative_target))

# number of planning units
sort(unique(benchmark_results$number_of_planning_units))

# objective functions
sort(unique(benchmark_results$objective))

# extract boundary penalty values,
# note that different values were examined for different objective functions
boundary_penalty_values <-
  benchmark_results %>%
  plyr::dlply("objective", function(x) unique(x$boundary_penalty))

## boundary penalty values for min set objective function
sort(boundary_penalty_values$add_min_set_objective)

## boundary penalty values for min shortfall objective function
sort(boundary_penalty_values$add_min_shortfall_objective)

# budgets examined for budget-limited objectives (e.g., 0.1 = 10% of total cost)
## note that the min set objective function does not use a budget,
## and thus it has a NA value
tibble(
  objective = unique(benchmark_results$objective),
  budget = unique(benchmark_results$budget)
)


## ----"preliminary calculations", message = FALSE------------------------------
# define helper function to create plots
plot_benchmark <- function(
  objective, n_pu, boundary_penalty, solver = NULL){
  # assert arguments are valid
  ## verify parameters with no default arguments
  assertthat::assert_that(
    assertthat::is.count(n_pu), assertthat::noNA(n_pu),
    n_pu %in% unique(benchmark_results$number_of_planning_units),
    assertthat::is.number(boundary_penalty), assertthat::noNA(boundary_penalty),
    assertthat::is.string(objective), assertthat::noNA(objective),
    objective %in% unique(benchmark_results$objective)
  )
  ## set default argument for solver if needed
  if (is.null(solver)) {
    solver <- unique(benchmark_results$solver)
  }
  ## verify solver argument
  assertthat::assert_that(
    is.character(solver), all(solver %in% benchmark_results$solver)
  )
  ## verify that only a single set of features was used
  assertthat::assert_that(
    dplyr::n_distinct(benchmark_results$number_features) == 1
  )

  # prepare data for plotting
  ## rename variables to avoid scoping issues
  sol <- solver
  obj <- objective
  bp <- boundary_penalty
  ## subset data relevant for plotting
  plot_data <-
    benchmark_results %>%
    filter(.$objective == obj, .$solver %in% sol,
           .$number_of_planning_units == n_pu,
           .$boundary_penalty == bp
    )
  ## scale run time to helpful units for plotting
  plot_units <-
    dplyr::case_when(
      # show hours if max(run_time) > 3 h
      max(plot_data$run_time, na.rm = TRUE) > 60 * 60 * 3 ~ "hours",
      # show minutes if max(run_time) > 3 M
      max(plot_data$run_time, na.rm = TRUE) > 60 * 3 ~ "minutes",
      # else show seconds
      TRUE ~ "seconds"
    )
   plot_data$min_set_scaled <-
    plot_data$run_time %>%
    units::set_units(s) %>%
    units::set_units(plot_units, mode = "standard") %>%
    as.numeric()
  ## plot labels
  n_f <- unique(benchmark_results$number_features)[1]
  plot_title =
    paste0(
      dplyr::case_when(
        objective == "add_min_set_objective" ~ "Min. set",
        objective == "add_min_shortfall_objective" ~ "Min. shortfall",
        TRUE ~ objective),
      ": ",
      formatC(
        n_f, big.mark = ",", digits = 2, format = "f",
        drop0trailing = TRUE),
      " features, ",
      formatC(
        n_pu, big.mark = ",", digits = 2, format = "f",
        drop0trailing = TRUE),
      " planning units"
  )
  if (bp > 1e-15) {
    plot_title <- paste0(plot_title, ", ", bp, " boundary penalty")
  }
  ## determine colors for solvers (so that solvers always have same color
  solver_names <- unique(benchmark_results$solver)
  solver_colors <- scales::hue_pal()(length(solver_names))
  names(solver_colors) <- solver_names

  # return plot for selected benchmark runs
  # (suppress warnings when a solver doesn't have results)
  suppressWarnings(
    print(
      ggplot(
        data = plot_data,
        mapping = aes(
          x = relative_target, y = min_set_scaled, color = solver
        )
      ) +
      scale_y_continuous(limits = c(0, NA_real_)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(labels = function(x) x * 100) +
      scale_color_manual(values = solver_colors) +
      labs(
        title = plot_title,
        x = "Representation targets (%)",
        y = paste0("Run time (", plot_units, ")"))
    )
  )
}


## ----"preview_results"--------------------------------------------------------
# extract columns from table with relevant data
benchmark_results <-
  benchmark_results %>%
  dplyr::select(
    id, number_of_planning_units, number_features,
    objective, budget, relative_target, boundary_penalty,
    solver, status, total_time, run_time, exceeded_run_time
  )

# preview results
print(benchmark_results)


## ----"overall_average_run_times"----------------------------------------------
# plot overall summary of solver performance
ggplot(
  data =
    benchmark_results %>%
    mutate(
      min_set_scaled = as.numeric(
        set_units(set_units(run_time, "seconds"), "hours")
      )
    ),
  aes(x = solver, y = min_set_scaled)
) +
geom_boxplot() +
theme(axis.text.x = element_text(size = 7)) +
labs(x = "Solver", y = "Run time (hours)")


## ----"min_set_pu_n_1"---------------------------------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[1],
  boundary_penalty = 0
)


## ----"min_set_pu_n_2"---------------------------------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[2],
  boundary_penalty = 0
)


## ----"min_set_pu_n_3"---------------------------------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[3],
  boundary_penalty = 0
)


## ----"min_set_pu_n_4"---------------------------------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = 0
)


## ----"min_set_pu_n_4_fast_only"-----------------------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = 0,
  solver = c("add_cbc_solver", "add_cplex_solver", "add_gurobi_solver")
)


## ----"min_set_pu_n_1_with_low_boundary_penalty"-------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[1],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[2]
)


## ----"min_set_pu_n_2_with_low_boundary_penalty"-------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[2],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[2]
)


## ----"min_set_pu_n_3_with_low_boundary_penalty"-------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[3],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[3]
)


## ----"min_set_pu_n_4_with_low_boundary_penalty"-------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[2]
)


## ----"min_set_pu_n_4_with_low_boundary_penalty_and_fast_solvers"--------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[2],
  solver = c("add_cbc_solver", "add_cplex_solver", "add_gurobi_solver")
)


## ----"min_set_pu_n_1_with_high_boundary_penalty"------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[1],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[3]
)


## ----"min_set_pu_n_2_with_high_boundary_penalty"------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[2],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[3]
)


## ----"min_set_pu_n_3_with_high_boundary_penalty"------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[3],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[3]
)


## ----"min_set_pu_n_4_with_high_boundary_penalty"------------------------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[3]
)


## ----"min_set_pu_n_4_with_high_boundary_penalty_and_fast_solvers"-------------
plot_benchmark(
  objective = "add_min_set_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = boundary_penalty_values$add_min_set_objective[3],
  solver = c("add_cbc_solver", "add_cplex_solver", "add_gurobi_solver")
)


## ----"min_short_pu_n_1"-------------------------------------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[1],
  boundary_penalty = 0
)


## ----"min_short_pu_n_2"-------------------------------------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[2],
  boundary_penalty = 0
)


## ----"min_short_pu_n_3"-------------------------------------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[3],
  boundary_penalty = 0
)


## ----"min_short_pu_n_4"-------------------------------------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = 0
)


## ----"min_short_pu_n_1_with_low_boundary_penalty"-----------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[1],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[2]
)


## ----"min_short_pu_n_2_with_low_boundary_penalty"-----------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[2],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[2]
)


## ----"min_short_pu_n_3_with_low_boundary_penalty"-----------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[3],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[2]
)


## ----"min_short_pu_n_4_with_low_boundary_penalty"-----------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[2]
)


## ----"min_short_pu_n_4_and_boundary_penalty_and_fast_solvers"-----------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[2],
  solver = c("add_cbc_solver", "add_cplex_solver", "add_gurobi_solver")
)


## ----"min_short_pu_n_1_with_high_boundary_penalty"----------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[1],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[3]
)


## ----"min_short_pu_n_2 with boundary penalty"---------------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[2],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[3]
)


## ----"min_short_pu_n_3_with_high_boundary_penalty"----------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[3],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[3]
)


## ----"min_short_pu_n_4_with_high_boundary_penalty"----------------------------
plot_benchmark(
  objective = "add_min_shortfall_objective",
  n_pu = n_planning_units[4],
  boundary_penalty = boundary_penalty_values$add_min_shortfall_objective[3]
)

