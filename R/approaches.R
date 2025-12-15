#' @include internal.R
NULL

#' Add approaches
#'
#' An approach can be added to a multi-objective conservation planning problem
#' to determine how multiple conservation planning problems should be combined
#' and traded off. 
#'
#' @details
#' Approaches control the overarching strategy used to solve a 
#' multi-objective problem and how objectives interact. While objectives define 
#' what should be optimized (e.g., minimize cost, maximize utility, minimize 
#' shortfall), approaches define how those objectives are combined, for 
#' example by combining them to a single objective or optimizing them 
#' sequentially.
#' 
#' After constructing a [multi_problem()], the following functions can be used to 
#' specify a multi-objective optimization approach.
#' 
#' \describe{
#'
#' \item{[add_rel_constraint_approach()]}{
#' Add approach to solve objectives sequentially, respecting a priority ordering
#' and passing constraints from earlier solutions to later ones using 
#' a specified relative tolerances (or level of degradation).
#' }
#'
#' \item{[add_weighted_sum_approach()]}{
#' Add approach to combine multiple objectives into a single objective by 
#' assigning a weight to each problem.
#' }
#'
#' }
#'
#' @family overviews
#'
#' @examples
#' \dontrun{
#' # import data
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_features <- get_sim_features()
#'
#' weights <- runif(2)
#'
#' # create multi-object problem
#' p <-
#'   multi_problem(
#'     obj1 = problem(sim_zones_pu_raster[[1]], sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
#'       add_binary_decisions(),
#'     obj2 = problem(sim_zones_pu_raster[[2]], sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(rev(seq_along(terra::nlyr(sim_features)))) %>%
#'       add_binary_decisions()
#'   ) %>%
#'   add_weighted_sum_approach(weights = weights, verbose = FALSE) %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s <- solve(p)
#' }
#'
#' @name approaches
NULL
