#' @include internal.R
NULL

#' Evaluate solutions using summary statistics
#'
#' After generating a solution to a conservation planning problem,
#' it can be useful to evaluate how well it performs. These functions
#' can be used to evaluate a solution according to
#' various different summary statistics.
#'
#' @details
#' The following functions can be used to summarize the performance
#' of a solution to a conservation planning [problem()].
#'
#' \describe{
#'
#' \item{[eval_n_summary()]}{
#' Calculate the number of planning units selected
#' within a solution.
#' }
#'
#' \item{[eval_cost_summary()]}{
#' Calculate the total cost of a solution.
#' }
#'
#' \item{[eval_feature_representation_summary()]}{
#' Calculate how well features are represented by a solution.
#' This function can be used for all problems.
#' }
#'
#' \item{[eval_target_coverage_summary()]}{
#' Calculate how well feature representation [targets] are met by a solution.
#' This function can only be used with problems that contain [targets].
#' }
#'
#' \item{[eval_boundary_summary()]}{
#' Calculate the exposed boundary length (perimeter) associated with a
#' solution.
#' }
#'
#' \item{[eval_connectivity_summary()]}{
#' Calculate the connectivity held within a solution using symmetric data.
#' }
#'
#' \item{[eval_asym_connectivity_summary()]}{
#' Calculate the connectivity held within a solution using asymmetric data.
#' }
#'
#' }
#'
#' @family overviews
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create a minimal problem
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # evaluate number of selected planning units in solution
#' eval_n_summary(p, s)
#'
#' # evaluate solution cost
#' eval_cost_summary(p, s)
#'
#' # evaluate feature representation by solution
#' eval_feature_representation_summary(p, s)
#'
#' # evaluate target coverage by solution
#' eval_target_coverage_summary(p, s)
#'
#' # evaluate exposed boundary (perimeter) length by solution
#' eval_boundary_summary(p, s)
#'
#' # create a symmetric connectivity matrix to describe pair-wise connectivity
#' # values between combinations of planning units,
#' # see ?connectivity_matrix for more information
#'
#' # for brevity, we will do this using the cost data
#  # and assume that pairs of adjacent planning units with high
#' # cost valuers have high connectivity between them
#' cm <- connectivity_matrix(sim_pu_raster, sim_pu_raster)
#'
#' # evaluate connectivity of solution using symmetric data
#' eval_connectivity_summary(p, s, data = cm)
#'
#' # create an asymmetric connectivity matrix to describe pair-wise
#' # connectivity values between combinations of planning units
#'
#' # for brevity, we will just generate a matrix with random values
#' acm <- matrix(
#'   runif(ncell(sim_pu_raster) ^ 2),
#'   ncol = terra::ncell(sim_pu_raster)
#' )
#'
#' # evaluate connectivity of solution using asymmetric data
#' eval_asym_connectivity_summary(p, s, data = acm)
#'
#' }
#' @name summaries
NULL
