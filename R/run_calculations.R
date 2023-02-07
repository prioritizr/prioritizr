#' @include internal.R ConservationProblem-proto.R ConservationModifier-proto.R
NULL

#' Run calculations
#'
#' Execute preliminary calculations in a conservation problem and store the
#' results for later use. This function is useful when creating slightly
#' different versions of the same conservation planning problem that involve
#' the same pre-processing steps (e.g., calculating boundary data), because
#' means that the same calculations will not be run multiple times.
#'
#' @param x [problem()] object.
#'
#' @details This function is used for the effect of modifying the input
#'   [`ConservationProblem-class`] object. As such, it does not return
#'   anything. To use this function with [pipe()] operators, use the
#'   `%T>%` operator and not the `%>%` operator.
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # let us imagine a scenario where we wanted to understand the effect of
#' # setting different targets on our solution.
#'
#' # create a conservation problem with no targets
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_boundary_penalties(10, 0.5) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create a copies of p and add targets
#' p1 <- p %>% add_relative_targets(0.1)
#' p2 <- p %>% add_relative_targets(0.2)
#' p3 <- p %>% add_relative_targets(0.3)
#'
#' # now solve each of the different problems and record the time spent
#' # solving them
#' s1 <- system.time({solve(p1); solve(p2); solve(p3)})
#'
#' # This approach is inefficient. Since these problems all share the same
#' # planning units it is actually performing the same calculations three times.
#' # To avoid this, we can use the "run_calculations" function before creating
#' # the copies. Normally, R runs the calculations just before solving the
#' # problem
#'
#' # recreate a conservation problem with no targets and tell R run the
#' # preliminary calculations. Note how we use the %T>% operator here.
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_boundary_penalties(10, 0.5) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE) %T>%
#'   run_calculations()
#'
#' # create a copies of p and add targets just like before
#' p1 <- p %>% add_relative_targets(0.1)
#' p2 <- p %>% add_relative_targets(0.2)
#' p3 <- p %>% add_relative_targets(0.3)
#'
#' # solve each of the different problems and record the time spent
#' # solving them
#' s2 <- system.time({solve(p1); solve(p2); solve(p3)})
#'
#' # now lets compare the times
#' print(s1) # time spent without running preliminary calculations
#' print(s2) # time spent after running preliminary calculations
#'
#' # As we can see, we can save time by running the preliminary
#' # calculations before making copies of the problem with slightly
#' # different constraints. Although the time saved in this example
#' # is rather small, this is because the example data are very small.
#' # We would expect larger time savings for larger datasets.
#' }
#' @export
run_calculations <- function(x) {
  rlang::check_required(x)
  assert(is_conservation_problem(x))
  if (!is.Waiver(x$decisions))
    x$decisions$calculate(x)
  if (!is.Waiver(x$objective))
    x$objective$calculate(x)
  for (i in seq_along(x$penalties))
    x$penalties[[i]]$calculate(x)
    for (i in seq_along(x$constraints))
    x$constraints[[i]]$calculate(x)
  invisible(TRUE)
}
