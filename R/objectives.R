#' @include internal.R ConservationModifier-proto.R
NULL

#' Problem objective
#'
#' The problem objective specifies how the problem \code{\link{targets}} should
#' be used to generate a solution. All conservation planning problems require
#' an objective in order to be solved. See below for details on the problem
#' objectives that are available.
#' 
#' @param budget \code{numeric} value specifying the maximum expenditure of 
#'  the prioritization.
#'
#' @param tree \code{\link[ape]{phylo}} object specifying a phylogenetic tree
#   for the conservation features.
#'
#' @details Here is a list of the available objectives and a description
#'  of each of them.
#'  \describe{
#'    \item{minimum_set_objective}{The objective here is to find the 
#'     the solution that fulfills all the targets and constraints for 
#'     the smallest cost. This objective is similar to that used in Marxan.}
#'    \item{maximum_coverage_objective}{The objective here is to find the
#'     the solution that fulfills as many targets as possible while ensuring
#'     that the cost of the solution does not exceed budget and that all
#'     constraints are met. This objective is similar to that used in
#'     Zonation.}
#'    \item{phylogenetic_coverage_objective}{This objective is similar to 
#'     \code{maximum_coverage_objective} except that emphasis is placed
#'     on preserving as much of a representative sample a phylogenetic
#'     tree as possible given a budget. This objective requires the
#'     "ape" R package to be installed.}
#'  }
#'
#' @seealso \code{link{constraints}}, \code{link{problem}},
#'  \code{\link{targets}}.
#'
#' @examples
#' # minimum set objective
#' problem(sim_pu_raster, sim_features) + minimum_set_objective()
#'
#' # maximum coverage objective
#' problem(sim_pu_raster, sim_features) + maximum_coverage_objective(cost=20)
#'
#' # phylogenetic coverage objective
#' problem(sim_pu_raster, sim_features) + 
#'   phylogenetic_coverage_objective(cost=20, tree=sim_phylogeny)
#'
#' @name objectives
NULL

#' @rdname objectives
#' @export
minimum_set_objective <- function() {
  pproto(
    'Objective',
    Objective
    name='Minimum set objective',
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(x, 'ConservationProblem'))
      invisible(rcpp_apply_minimum_set_objective(x, x$get_targets(),
        x$get_costs()))
    })
}

#' @rdname objectives
#' @export
maximum_coverage_objective <- function(budget) {
  assertthat::assert_that(assertthat::is.scalar(budget), isTRUE(budget > 0.0))
  pproto(
    'Objective',
    Objective,
    name='Maximum coverage objective',
    parameters=parameters(truncated_numeric_parameter('budget', budget)),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      assertthat::see_if(self$get_parameter('budget') <= sum(x$get_costs()))
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(x, 'ConservationProblem'))
      invisible(rcpp_apply_maximum_coverage_objective(x, x$get_targets(),
        x$get_costs()))
    })
}

#' @rdname objectives
#' @export
phylogenetic_coverage_objective <- function(budget, tree) {
  if (!'ape' %in% rownames(installed.packages()))
    stop('the "ape" package needs to be installed to use phylogenetic data')
  assertthat::assert_that(assertthat::is.scalar(budget), isTRUE(budget > 0.0),
    inherits(tree, 'phylo'))
  pproto(
    'Objective'
    Objective,
    name='Phylogenetic coverage objective',
    parameters=parameters(truncated_numeric_parameter('budget', budget)),
    data=list(tree=tree),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      assertthat::see_if(self$get_parameter('budget') <= sum(x$get_costs()),
        length(self$data$tree$tip.label) == x$get_number_of_features())
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(x, 'ConservationProblem'))    
      stop('TODO: apply function for phylogenetic_coverage_objective')
    })
}


