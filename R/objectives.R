#' @include internal.R pproto.R Objective-proto.R
NULL

#' Problem objective
#'
#' The problem objective specifies how the problem \code{\link{targets}} should
#' be used to generate a solution. All conservation planning problems require
#' an objective in order to be solved. See below for a list of the problem
#' objectives that are available.
#'
#' \describe{
#'
#'   \item{\code{add_minimum_set_objective}}{The objective here is to find the 
#'     the solution that fulfills all the targets and constraints for 
#'     the smallest cost. This objective is similar to that used in Marxan.}
#'
#'   \item{\code{add_maximum_coverage_objective}}{The objective here is to find
#'     the solution that fulfills as many targets as possible while ensuring
#'     that the cost of the solution does not exceed budget and that all
#'     constraints are met. This objective is similar to that used in
#'     Cabeza et al. XXXX.}
#'
#'   \item{\code{add_phylogenetic_coverage_objective}}{This objective is 
#'     similar to \code{maximum_coverage_objective} except that emphasis is 
#'     placed on preserving as much of a representative sample a phylogenetic
#'     tree as possible given a budget. This objective requires the
#'     "ape" R package to be installed.}
#'
#'   \item{\code{default_objective}}{This objective is used when no objective
#'     has been explicitly specified. This will result in an error because
#'     \emph{"Let the computer find out" is a poor strategy and usually
#'     reflects the fact that the researcher did not bother to think
#'     clearly about the problem of interest and its scientific setting
#'     (Burnham and Anderson, 2002).}}
#'
#'  }
#'
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param budget \code{numeric} value specifying the maximum expenditure of 
#'   the prioritization.
#'
#' @param tree \code{\link[ape]{phylo}} object specifying a phylogenetic tree
#    for the conservation features.
#'
#' @seealso \code{link{constraints}}, \code{link{problem}},
#'   \code{\link{targets}}.
#'
#' @examples
#' # create problem
#' p <- problem(sim_pu_raster, sim_features)
#' 
#  # add minimum set objective
#' p %>% minimum_set_objective()
#'
#' # maximum coverage objective
#' p %>% maximum_coverage_objective(20)
#'
#' # phylogenetic coverage objective
#' p %>% phylogenetic_coverage_objective(20, sim_phylogeny)
#'
#' @name objectives
NULL

#' @rdname objectives
#' @export
add_minimum_set_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  # add objective to problem
  x$add_objective(pproto(
    'MinimumSetObjective',
    Objective,
    name='Minimum set objective',
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      invisible(rcpp_apply_minimum_set_objective(x$ptr, y$feature_targets(),
        y$planning_unit_costs()))
    }))
  # return probem
  return(x)
}

#' @rdname objectives
#' @export
add_maximum_coverage_objective <- function(x, budget) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'), 
    isTRUE(all(is.finite(budget))), assertthat::is.scalar(budget), 
    isTRUE(budget > 0.0))
  # make parameter
  p <- numeric_parameter('budget', budget, lower_bound=0, 
    upper_limit=sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    'MaximumCoverageObjective',
    Objective,
    name='Maximum coverage objective',
    parameters=parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      invisible(rcpp_apply_maximum_coverage_objective(x$ptr, y$feature_targets(),
        x$planning_unit_costs()))
    }))
  # return problem
  return(x)
}

#' @rdname objectives
#' @export
add_phylogenetic_coverage_objective <- function(x, budget, tree) {
  # check that dependencies are installed
  if (!requireNamespace('ape'))
    stop('the "ape" package needs to be installed to use phylogenetic data')
  # assert arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'), 
    isTRUE(all(is.finite(budget))), assertthat::is.scalar(budget), 
    isTRUE(budget > 0.0), inherits(tree, 'phylo'), 
    length(self$data$tree$tip.label) == x$number_of_features())
  # make parameter
  p <- numeric_parameter('budget', budget, lower_limit=0,
    upper_limit=sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    'PhylogeneticCoverageObjective',
    Objective,
    name='Phylogenetic coverage objective',
    parameters=parameters(p),
    data=list(tree=tree),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(x, 'ConservationProblem'))    
      stop('TODO: apply function for phylogenetic_coverage_objective')
    }))
  # return problem
  return(x)
}

#' @rdname objectives
#' @export
add_default_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  # throw error because objectives must be explicitly defined
  stop('problem is missing an objective and this must be explicitly defined')
}

