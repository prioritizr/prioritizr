#' @include internal.R ConservationProblem-class.R MultiObjConservationProblem-class.R
NULL

#' Evaluate cost of solution
#'
#' Calculate the total cost of a solution to a conservation planning
#' problem.
#' For example, if the planning unit cost data describe land acquisition costs
#' (USD), then the total cost would be net cost (USD) needed to acquire
#' all planning units selected within the solution.
#'
#' @param x [problem()] or [multi_problem()] object.
#'
#' @param solution `numeric`, `matrix`, `data.frame`,
#'  [terra::rast()], or [sf::sf()] object.
#'  Note that `solution` must have the same format as the planning unit
#'  data in `x`.
#'  See the Solution format section for more information.
#'
#' @details
#' This metric is equivalent to the `Cost` metric reported by the
#' [*Marxan* software](https://marxansolutions.org) (Ball *et al.* 2009).
#' Specifically, the cost of a solution is defined as the sum of the cost
#' values, supplied when creating a [problem()] object
#' (e.g., per `cost_column`),
#' weighted by the status of each planning unit in the solution.
#'
#' @section Solution format:
#' Broadly speaking, `solution` must be in the same format as
#' the planning unit data in `x`.
#' Further details on the correct format are listed separately
#' for each of the different planning unit data formats.
#'
#' `r solution_format_documentation("solution")`
#'
#' @return
#' A [tibble::tibble()] object describing the solution cost.
#' It contains the following columns.
#'
#' \describe{
#'
#' \item{problem}{
#' `character` name of problem. Note that this column
#' is only present if `x` is a [multi_problem()] object.
#' }
#'
#' \item{summary}{
#' `character` description of the summary statistic.
#' The statistic associated with the `"overall"` value
#' in this column is calculated using the entire solution
#' (including all management zones if `x` has multiple zones).
#' If `x` has multiple management zones, then summary statistics
#' are also provided for each zone separately
#' (indicated using zone names).
#' }
#'
#' \item{cost}{
#' `numeric` cost value.
#' Greater values correspond to solutions that are more costly
#' to implement.
#' Thus conservation planning exercises typically prefer solutions
#' with smaller values, because they are cheaper to implement
#' (assuming all else is equal).
#' }
#'
#' }
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' @name eval_cost_summary
#'
#' @seealso
#' See [summaries] for an overview of all functions for summarizing solutions.
#'
#' @family summaries
#'
#' @examplesIf prioritizr::do_run_example()
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#' sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # build minimal conservation problem with raster data
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # calculate cost of the solution
#' r1 <- eval_cost_summary(p1, s1)
#' print(r1)
#'
#' # build minimal conservation problem with polygon data
#' p2 <-
#'   problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2[, "solution_1"])
#'
#' # print solution
#' print(s2)
#'
#' # calculate cost of the solution
#' r2 <- eval_cost_summary(p2, s2[, "solution_1"])
#' print(r2)
#'
#' # manually calculate cost of the solution
#' r2_manual <- sum(s2$solution_1 * sim_pu_polygons$cost, na.rm = TRUE)
#' print(r2_manual)
#'
#' # build multi-zone conservation problem with polygon data
#' p3 <-
#'   problem(
#'     sim_zones_pu_polygons, sim_zones_features,
#'     cost_column = c("cost_1", "cost_2", "cost_3")
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s3 <- solve(p3)
#'
#' # print solution
#' print(s3)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s3$solution <- category_vector(
#'   s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' s3$solution <- factor(s3$solution)
#'
#' # plot solution
#' plot(s3[, "solution"])
#'
#' # calculate cost of the solution
#' r3 <- eval_cost_summary(
#'   p3, s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' print(r3)
#'
#' @export
eval_cost_summary <- function(x, solution) {
  assert_required(x)
  assert_required(solution)
  assert(is_generic_conservation_problem(x))
  UseMethod("eval_cost_summary")
}

#' @rdname eval_cost_summary
#' @method eval_cost_summary ConservationProblem
#' @export
eval_cost_summary.ConservationProblem <- function(x, solution) {
  # assert arguments are valid
  assert_required(x)
  assert_required(solution)
  assert(is_conservation_problem(x))
  # calculate costs
  internal_eval_cost_summary(x, planning_unit_solution_status(x, solution))
}

#' @rdname eval_cost_summary
#' @method eval_cost_summary MultiObjConservationProblem
#' @export
eval_cost_summary.MultiObjConservationProblem <- function(x, solution) {
  # assert arguments are valid
  assert_required(x)
  assert_required(solution)
  assert(is_multi_conservation_problem(x))
  # extract solution
  solution <- planning_unit_solution_status(x, solution)
  # calculate costs
  out <- do.call(
    rbind,
    lapply(
      seq_along(x$problems),
      function(i) {
        out <- internal_eval_cost_summary(x$problems[[i]], solution)
        out$problem <- x$problem_names()[[i]]
        out
      }
    )
  )
  out <- tibble::as_tibble(out)
  # return result
  out[, c("problem", setdiff(names(out), "problem")), drop = FALSE]
}

internal_eval_cost_summary <- function(x, status) {
  # calculate overall cost of each planning unit
  cost_data <- x$planning_unit_costs()
  # output costs
  costs <- unname(c(colSums(cost_data * status, na.rm = TRUE)))
  total_cost <- sum(costs)
  if (x$number_of_zones() > 1) {
    out <- tibble::tibble(
      summary = c("overall", zone_names(x)),
      cost = c(total_cost, costs)
    )
  } else {
    out <- tibble::tibble(summary = "overall", cost = total_cost)
  }
  # return result
  out
}
