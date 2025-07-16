#' @include internal.R ConservationProblem-class.R OptimizationProblem-class.R problem.R
NULL

#' Evaluate solution importance using rarity weighted richness scores
#'
#' Calculate importance scores for planning units selected in a solution
#' using rarity weighted richness scores (based on Williams *et al.* 1996).
#'
#' @inheritParams eval_replacement_importance
#'
#' @details Rarity weighted richness scores are calculated using the following
#'   terms. Let \eqn{I} denote the set of planning units (indexed by
#'   \eqn{i}), let \eqn{J} denote the set of conservation features (indexed by
#'   \eqn{j}), let \eqn{r_{ij}} denote the amount of feature \eqn{j}
#'   associated with planning unit \eqn{i}, and let \eqn{m_j} denote the
#'   maximum value of feature \eqn{j} in \eqn{r_{ij}} in all planning units
#'   \eqn{i \in I}. To calculate the rarity weighted richness (*RWR*) for
#'   planning unit \eqn{k}:
#'
#'   \deqn{
#'   \mathit{RWR}_{k} = \sum_{j}^{J} \frac{ \frac{r_{ik}}{m_j} }{
#'                                           \sum_{i}^{I}r_{ij}}
#'   }{
#'   RWRk = sum_j^J ( (rik / mj) / sum_i^I rij)
#'   }
#'
#' @inheritSection eval_cost_summary Solution format
#'
#' @inherit eval_replacement_importance return
#'
#' @details
#' This method is only recommended for large-scaled conservation
#' planning exercises (i.e., more than 100,000 planning units) where
#' importance scores cannot be calculated using other methods in a feasible
#' period of time. This is because rarity weighted richness scores cannot (i)
#' account for the cost of different planning units, (ii) account for multiple
#' management zones, and (iii) identify truly irreplaceable planning units ---
#' unlike the replacement cost metric which does not suffer any of these
#' limitations.
#'
#' @seealso
#' See [importance] for an overview of all functions for evaluating
#' the importance of planning units selected in a solution.
#.
#' @family importances
#'
#' @examples
#' \dontrun{
#' # seed seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#'
#' # create minimal problem with raster planning units
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # calculate importance scores
#' rwr1 <- eval_rare_richness_importance(p1, s1)
#'
#' # print importance scores
#' print(rwr1)
#'
#' # plot importance scores
#' plot(rwr1, main = "rarity weighted richness", axes = FALSE)
#'
#' # create minimal problem with polygon planning units
#' p2 <-
#'   problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' plot(s2[, "solution_1"], main = "solution")
#'
#' # calculate importance scores
#' rwr2 <- eval_rare_richness_importance(p2, s2[, "solution_1"])
#'
#' # plot importance scores
#' plot(rwr2, main = "rarity weighted richness")
#' }
#'
#' @references
#' Williams P, Gibbons D, Margules C, Rebelo A, Humphries C, and Pressey RL
#' (1996) A comparison of richness hotspots, rarity hotspots and complementary
#' areas for conserving diversity using British birds.
#' *Conservation Biology*, 10: 155--174.
#'
#' @export
eval_rare_richness_importance <- function(x, solution, rescale = TRUE) {
  # assert valid arguments
  assert_required(x)
  assert_required(solution)
  assert_required(rescale)
  assert(
    is_conservation_problem(x),
    is_inherits(
      solution,
      c(
        "numeric", "data.frame", "matrix", "sf", "SpatRaster",
        "Spatial", "Raster"
      )
    ),
    assertthat::is.flag(rescale),
    assertthat::noNA(rescale)
  )
  assert(
    isTRUE(number_of_zones(x) == 1),
    msg = c(
      "This function requires that {.arg x} must have a single zone.",
      "i" = paste(
        "This is because the calculations only work",
        "for a limited range of problem formulations."
      ),
      "i" = "Try using {.fn eval_replacement_importance} instead."
    )
  )
  # extract planning unit solution status
  status <- planning_unit_solution_status(x, solution)
  # calculate replacement costs
  v <- internal_eval_rare_richness_importance(x, status, rescale)
  # return formatted values
  planning_unit_solution_format(x, v, solution, prefix = "rwr")
}

internal_eval_rare_richness_importance <- function(x, status, rescale,
                                                   call = fn_caller_env()) {
  # assert valid arguments
  assert(
    is.numeric(status),
    is.matrix(status),
    call = call,
    .internal = TRUE
  )
  # extract indices for solution
  indices <- which(status > 1e-10)
  # calculate rarity weighted richness for each selected planning unit
  rs <- x$feature_abundances_in_total_units()
  m <- matrix(
    apply(x$data$rij_matrix[[1]], 1, max, na.rm = TRUE),
    nrow = nrow(rs),
    ncol = length(indices),
    byrow = FALSE
  )
  out <- x$data$rij_matrix[[1]][, indices, drop = FALSE]
  ## account for divide by zero issues result in NaNs
  out <- (out / m)
  out[!is.finite(out)] <- 0
  ## account for divide by zero issues result in NaNs
  out <- out / rs[, rep.int(1, ncol(out)), drop = FALSE]
  out[!is.finite(out)] <- 0
  out <- Matrix::colSums(out)
  # rescale values if specified
  if (rescale) {
    rescale_ind <- is.finite(out) & (abs(out) > 1e-10)
    out[rescale_ind] <- rescale(out[rescale_ind], to = c(0.01, 1))
  }
  # convert to solution status format
  convert_raw_solution_to_solution_status(x, out, indices)
}
