#' @include internal.R ConservationProblem-proto.R
NULL

#' Irreplaceability
#'
#' Irreplaceability scores can be used to assess the relative importance
#' of planning units in a solution to a conservation planning
#' \code{\link{problem}}.
#'
#' @details Currently, only one method for calculating irreplaceability scores
#'    is available:
#'
#'   \describe{
#'
#'   \item{\code{\link{replacement_cost}}}{Calculate irreplaceability scores
#'     using the replacement cost method.}
#'
#'   }
#'
#' @seealso \code{\link{problem}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_polygons, sim_features)
#'
#' # build minimal conservation problem with raster data
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#'
#' # solve the problem
#' \donttest{
#' s1 <- solve(p1)
#' }
#'
#' # plot solution
#' \donttest{
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#' }
#'
#' # calculate irreplaceability scores using replacement cost method
#' \donttest{
#' rc1 <- replacement_cost(p1, s1)
#' }
#'
#' # plot irreplaceability scores
#' \donttest{
#' plot(rc1, main = "replacement cost",  axes = FALSE, box = FALSE)
#' }
#' @name irreplaceability
NULL
