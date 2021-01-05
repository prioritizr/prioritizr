#' @include internal.R
NULL

#' Evaluation
#'
#' After generating a solution to a conservation planning [problem()],
#' it can be useful to evaluate how well it performs. The functions
#' summarized here can be used to evaluate a solution according to
#' various different metrics.
#'
#' @details
#' The following functions can be used to evaluate a solution to a
#' conservation planning [problem()]:
#'
#'  \describe{
#'
#'  \item{[eval_n()]}{Calculate the number of planning units selected within a
#'    solution.}
#'
#'  \item{[eval_cost()]}{Calculate the total cost of a solution.}
#'
#'  \item{[eval_feature_representation()]}{Calculate how well features are
#'    represented by a solution. This function can be used for problems
#'    that are built using [targets] and those that are not built
#'    using [targets].}
#'
#'  \item{[eval_target_coverage()]}{Calculate how well feature representation
#'    [targets] are met by a solution. This function can only be used with
#'    problems contain [targets].}
#'
#'  \item{[eval_boundary()]}{Calculate the exposed boundary length (perimeter)
#'    associated with a solution.}
#'
#'  \item{[eval_connectivity()]}{Calculate the connectivity held within a
#'    solution.}
#'
#'  }
#'
#' @seealso [constraints], [decisions],
#'  [objectives] [penalties],
#'  [portfolios], [problem()],
#'  [solvers].
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create a minimal problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(0.1) %>%
#'      add_binary_decisions()
#'
#' \dontrun{
#' # solve problem
#' s <- solve(p)
#'
#' # evaluate number of selected planning units in solution
#' eval_n(p, s)
#'
#' # evaluate solution cost
#' eval_cost(p, s)
#'
#' # evaluate feature representation by solution
#' eval_feature_representation(p, s)
#'
#' # evaluate target coverage by solution
#' eval_target_coverage(p, s)
#'
#'
#' # evaluate exposed boundary (perimeter) length by solution
#' eval_boundary(p, s)
#'
#' # create a connectivity matrix to describe pair-wise connectivity
#' # values between combinations of planning units,
#' # see ?connectivity_matrix for more information
#'
#' # for brevity, we will do this using the cost data
#  # and assume that pairs of adjacent planning units with high
#' # cost valuers have high connectivity between them
#' cm <- connectivity_matrix(sim_pu_raster, sim_pu_raster)
#'
#' # evaluate connectivity of solution
#' eval_connectivity(p, s, data = cm)
#'
#' }
#' @name evaluation
NULL
