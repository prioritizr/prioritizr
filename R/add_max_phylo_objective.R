#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add Maximum Phylogenetic Representation Objective
#'
#' Set an objective to find to find the solution that fulfills as much
#' of a representative sample a phylogenetic tree as possible given a budget.
#' This objective is similar to \code{add_max_features_objective} except that
#' emphasis is placed on phylogenetic representation rather than target
#' representation. This objective requires the "ape" R package to be installed.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param budget \code{numeric} value specifying the maximum expenditure of
#' the prioritization.
#'
#' @param tree \code{\link[ape]{phylo}} object specifying a phylogenetic tree
#' for the conservation features.
#'
#' @details
#' A problem objective is used to specify the overall goal of the
#' conservation planning problem. Please note that \strong{all conservation planning 
#' problems formulated in the prioritizr package require the addition of both 
#' objectives and targets}. Failing to do so will return a default error message 
#' when solving.
#' 
#' The maximum phylogenetic representation problem is similar to the maximum features 
#' problem in that it allows for both a budget and targets to be set. This problem finds the 
#' set of planning units that meets representation targets for a phylogenetic tree while 
#' staying within a fixed budget. If multiple solutions can meet all targets while 
#' staying within budget, the cheapest solution is chosen. 
#' 
#' The maximum target coverage problem (and by extension, the maximum phylogenetic problem)
#' can be stated mathematically, for \emph{n} planning units and \emph{m} conservation 
#' features, as:
#' 
#' \deqn{\text{Maximize} \space -a\sum_{i=1}^{n} x_i c_i + \sum_{j=1}^{m}y_j 
#' \text{ subject to } \sum_{i=1}^{n}x_i c_i \leq B \space \text{and} \space 
#' \sum_{j=1}^{n} x_j r_{ij}\geq y_iT_i \space \forall \space i}{Maximize 
#' -a\sum^n (xi*ci) + \sum^m (yi) subject to \sum^n (xi*ci) \le B and \sum^n 
#' (xj*rij) \ge yi*Ti for all i}
#' 
#' where \eqn{x_i}{xi} is a binary decision variable specifying whether planning unit 
#' \emph{i} has been selected (1) or not (0), \eqn{y_i}{yi} is a binary decision variable 
#' specifying whether the target for species \emph{i} should be met, \eqn{c_i}{ci} is 
#' the cost of planning unit \emph{i}, \eqn{r_ij}{rij} is the representation level of 
#' feature \emph{i} in planning unit \emph{j}, \emph{B} is the budget, and \eqn{T_i}{Ti} 
#' is the target for feature \emph{i}. Finally, the factor \emph{a} is chosen so that the 
#' first term of the objective function is much smaller than the second. This ensures that 
#' the reserve cost only plays a role in distinguishing between solutions that meet the 
#' same number of targets.
#'
#'
#' @seealso \code{\link{objectives}}, \code{\link{constraints}}, \code{\link{problem}},
#'   \code{\link{targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features, sim_phylogeny)
#'
#' # create base problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'  add_relative_targets(0.1)
#'
#  # create problem with added minimum set objective
#' p1 <- p %>% add_min_set_objective()
#' # create problem with added maximum coverage objective
#' # note that this objective does not use targets
#' p2 <- p %>% add_max_cover_objective(5000)
#'
#' # create problem with added maximum feature representation objective
#' p3 <- p %>% add_max_features_objective(5000)
#'
#' # create problem with added maximum phylogenetic representation objective
#' p4 <- p %>% add_max_phylo_objective(5000, sim_phylogeny)
#'
#' \donttest{
#' # solve problems
#' s <- stack(solve(p1), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' plot(s, main=c("minimum set", "maximum coverage", "maximum representation",
#'                "phylogenetic representation"))
#' }
#'
#' @name add_max_phylo_objective
NULL

#' @rdname add_max_phylo_objective
#' @export
add_max_phylo_objective <- function(x, budget, tree) {
  # check that dependencies are installed
  if (!requireNamespace("ape"))
    stop("the \"ape\" package needs to be installed to use phylogenetic data")
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(budget))), assertthat::is.scalar(budget),
                          isTRUE(budget > 0.0), inherits(tree, "phylo"),
                          length(tree$tip.label) == x$number_of_features(),
                          setequal(tree$tip.label, x$feature_names()))
  # make parameter
  p <- numeric_parameter("budget", budget, lower_limit = 0,
                         upper_limit = sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    "PhylogeneticRepresentationObjective",
    Objective,
    name = "Phylogenetic representation objective",
    parameters = parameters(p),
    data = list(tree = tree),
    calculate = function(self, x) {
      assertthat::assert_that(inherits(x, "ConservationProblem"))
      # get tree
      tr <- self$get_data("tree")
      # order rows to match order of features in problem
      pos <- match(tr$tip.label, x$feature_names())
      # convert tree into matrix showing which species in which
      # branches and store the result
      self$set_data("branch_matrix", branch_matrix(tr)[pos, ])
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_phylo_objective(x$ptr,
                                               y$feature_targets(), y$planning_unit_costs(),
                                               self$parameters$get("budget"), self$get_data("branch_matrix"),
                                               self$get_data("tree")$edge.length
      ))
    }))
}
