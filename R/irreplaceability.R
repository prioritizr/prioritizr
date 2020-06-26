#' @include internal.R ConservationProblem-proto.R
NULL

#' Irreplaceability
#'
#' Irreplaceability scores can be used to assess the relative importance
#' of planning units in a solution to a conservation planning
#' \code{\link{problem}}.
#'
#' @details
#' The following methods are available for calculating irreplaceability scores:
#'
#' \describe{
#'
#' \item{\code{\link{replacement_cost}}}{
#'   The replacement cost scores (based
#'   on Cabeza and Moilanen 2006) quantify the change in the objective
#'   function (e.g. additional costs required to meet feature targets) of the
#'   optimal solution if a given planning unit in a solution cannot be acquired.
#'   They can (i) account for the cost of different planning units, (ii) account
#'   for multiple management zones, (iii) apply to any objective function, and
#'   (iv) identify truly irreplaceable planning units (denoted with infinite
#'   values).}
#'
#' \item{\code{\link{ferrier_score}}}{
#'  The Ferrier scores (Ferrier *et al.* 2000) quantify the importance of
#'  planning units for meeting feature targets. They can only be applied to
#'  conservation problems with a minimum set objective and a single zone (i.e.
#'  the classic *Marxan*-type problem). Furthermore---unlike the
#'  replacement cost scores---the Ferrier irreplaceability scores provide a
#'  score for each feature within each planning unit, providing insight into
#'  why certain planning units are more important than other planning units.}
#'
#' \item{\code{\link{rarity_weighted_richness}}}{
#'  The rarity weighted richness scores (based on Williams *et al.*
#' 1996) are simply a measure of biological diversity. They do not account for
#' planning costs, multiple management zones, objective functions, or feature
#' targets (or weightings). They merely describe the spatial patterns of
#' biodiversity, and do not account for many of the factors needed to quantify
#' the importance of a planning unit for achieving conservation goals.}
#'
#' }
#'
#' Generally speaking, we recommend using replacement cost scores for small and
#' moderate sized problems (e.g. less than 30,000 planning units) when it is
#' feasible to do so. It can take a very long time to compute replacement
#' cost scores, and so it is simply not feasible to compute these scores for
#' particularly large problems. For moderate and large sized problems (e.g.
#' more than 30,000 planning units), we recommend using the Ferrier
#' irreplaceability scores if possible. As mentioned earlier, the Ferrier
#' irreplaceability scores can only be used for a specific type of conservation
#' problem. For large sized problems (e.g. more than 100,000 planning units),
#' we recommend using the rarity weighted richness scores simply because there
#' is no other choice available. It has been known for decades that such
#' static measures of biodiversity lead to poor conservation plans
#' (Kirkpatrick 1983).
#'
#' @references
#' Cabeza M and Moilanen A (2006) Replacement cost: A practical measure of site
#' value for cost-effective reserve planning. *Biological Conservation*,
#' 132:  336--342.
#'
#' Ferrier S, Pressey RL, and Barrett TW (2000) A new predictor of the
#' irreplaceability of areas for achieving a conservation goal, its application
#' to real-world planning, and a research agenda for further refinement.
#' *Biological Conservation*, 93: 303--325.
#'
#' Kirkpatrick, JB (1983) An iterative method for establishing priorities for
#' the selection of nature reserves: an example from Tasmania.
#' *Biological Conservation*, 25: 127--134.
#'
#' Williams P, Gibbons D, Margules C, Rebelo A, Humphries C, and Pressey RL
#' (1996) A comparison of richness hotspots, rarity hotspots and complementary
#' areas for conserving diversity using British birds.
#' *Conservation Biology*, 10: 155--174.
#'
#' @seealso \code{\link{problem}}.
#'
#' @examples
#' \donttest{
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
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#'
#' # calculate irreplaceability scores using replacement cost scores
#' ir1 <- replacement_cost(p1, s1)
#'
#' # calculate irreplaceability scores using Ferrier et al 2000 method,
#' # and extract the total irreplaceability scores
#' ir2 <- ferrier_score(p1, s1)[["total"]]
#'
#' # calculate irreplaceability scores using rarity weighted richness scores
#' ir3 <- rarity_weighted_richness(p1, s1)
#'
#' # plot irreplaceability scores
#' plot(stack(ir1, ir2, ir3), axes = FALSE, box = FALSE,
#'      main = c("replacement cost", "Ferrier score",
#'               "rarity weighted richness"))
#' }
#' @name irreplaceability
NULL
