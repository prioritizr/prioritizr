#' @include internal.R ConservationProblem-proto.R
NULL

#' Irreplaceability
#'
#' Irreplaceability scores can be used to assess the relative importance
#' of planning units in a solution to a conservation planning
#' \code{\link{problem}}.
#'
#' @details The following methods are available for calculating
#'   irreplaceability scores:
#'   \describe{
#'
#'   \item{\code{\link{replacement_cost}}}{Calculate irreplaceability scores
#'     using the replacement cost method. This method is generally recommended
#'    for calculating irreplaceability scores.}
#'
#'   \item{\code{\link{rarity_weighted_richness}}}{Calculate irreplaceability
#'     scores using rarity weighted richness. This method is only recommended
#'     for particularly large-scale conservation planning problems where
#'     the replacement cost method would take too long to produce scores in
#'     a feasible period of time.}
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
#'       add_binary_decisions() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
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
#' # calculate irreplaceability scores using replacement cost scores
#' \donttest{
#' rc1 <- replacement_cost(p1, s1)
#' }
#'
#' # calculate irreplaceability scores using rarity weighted richness scores
#' \donttest{
#' rc2 <- rarity_weighted_richness(p1, s1)
#' }
#'
#' # plot irreplaceability scores
#' \donttest{
#' plot(stack(rc1, rc2), axes = FALSE, box = FALSE,
#'      main = c("replacement cost", "rarity weighted richness"))
#' }
#' @name irreplaceability
NULL
