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
#' conservation planning problem. All conservation planning problems require
#' adding objectives and targets, or solving will return an error.
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
