#' @include internal.R ConservationProblem-proto.R
NULL

#' Evaluate solution importance
#'
#' Importance scores (also known as irreplaceability scores) can be used to
#' assess the relative importance of planning units selected in a solution to a
#' conservation planning [problem()].
#'
#' @details
#' The following methods are available for calculating importance scores:
#'
#' \describe{
#'
#' \item{[eval_replacement_importance()]}{
#'   Calculate importance scores using replacement costs (based
#'   on Cabeza and Moilanen 2006).
#'   These scores quantify the change in the objective
#'   function (e.g. additional costs required to meet feature targets) of the
#'   optimal solution if a given planning unit in a solution cannot be acquired.
#'   They can (i) account for the cost of different planning units, (ii) account
#'   for multiple management zones, (iii) apply to any objective function, and
#'   (iv) identify truly irreplaceable planning units (denoted with infinite
#'   values).}
#'
#' \item{[eval_ferrier_importance()]}{
#'  Calculate importance scores following Ferrier *et al.* (2000).
#'  These scores measure importance based on how critical
#'  planning units are for meeting targets. They can only be applied to
#'  conservation problems with a minimum set objective and a single zone (i.e.
#'  the classic *Marxan*-type problem). Furthermore---unlike the
#'  replacement cost scores---these scores provide a
#'  score for each feature within each planning unit, providing insight into
#'  why certain planning units are more important than other planning units.}
#'
#' \item{[eval_rare_richness_importance()]}{
#'  Calculate importance scores using the rarity weighted richness metric
#' (based on Williams *et al.* 1996).
#' These score are simply a measure of biodiversity.
#' They do not account for planning costs, multiple management zones, objective
#' functions, or feature targets (or weightings).
#' They merely describe the spatial patterns of
#' biodiversity, and do not account for many of the factors needed to quantify
#' the importance of a planning unit for achieving conservation goals.}
#'
#' }
#'
#' Generally speaking, we recommend using replacement cost scores for small and
#' moderate sized problems (e.g. less than 30,000 planning units) when it is
#' feasible to do so. It can take a very long time to compute replacement
#' cost scores, and so it is simply not feasible to compute these scores for
#' particularly large problems. For large sized problems (e.g.
#' more than 100,000 planning units),
#' we recommend using the rarity weighted richness scores simply because there
#' is no other choice available. It has been known for decades that such
#' measures of biodiversity lead to poor conservation plans
#' (Kirkpatrick 1983). We do not currently recommend using the Ferrier
#' scores because the code requires further testing to verify correctness.
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
#' @seealso [problem()].
#'
#' @examples
#' \dontrun{
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
#' # calculate importance scores using replacement cost scores
#' ir1 <- eval_replacement_importance(p1, s1)
#'
#' # calculate importance scores using Ferrier et al 2000 method,
#' # and extract the total importance scores
#' ir2 <- eval_ferrier_importance(p1, s1)[["total"]]
#'
#' # calculate importance scores using rarity weighted richness scores
#' ir3 <- eval_rare_richness_importance(p1, s1)
#'
#' # plot importance scores
#' plot(stack(ir1, ir2, ir3), axes = FALSE, box = FALSE,
#'      main = c("replacement cost", "Ferrier score",
#'               "rarity weighted richness"))
#' }
#' @name importance
#'
#' @aliases irreplaceability
NULL
