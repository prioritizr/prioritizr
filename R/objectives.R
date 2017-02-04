#' @include internal.R
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
#' problem(sim_pu, sim_features) + minimum_set_objective()
#'
#' # maximum coverage objective
#' problem(sim_pu, sim_features) + maximum_coverage_objective(cost=20)
#'
#' # phylogenetic coverage objective
#' problem(sim_pu, sim_features) + 
#'  phylogenetic_coverage_objective(cost=20, tree=sim_phylogeny)
#'
#' @name objectives
NULL

#' @rdname objectives
#' @export
minimum_set_objective <- function() {
  Objective$new(name='minimum set objective',
                 parameters=parameters(),
                 modelsense='min',
                 data=list(),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'))
                  TRUE
                 },
                 apply = function(x) {
                  stop('TODO: constrain_problem function for minimum_set_objective')
                 })
}

#' @rdname objectives
#' @export
maximum_coverage_objective <- function(budget) {
  assertthat::assert_that(is.number(budget), 
                          budget > 0.0)
  Objective$new(name='maximum coverage objective',
                 modelsense='max',
                 parameters=parameters(
                  truncated_numeric_parameter('budget', budget)),
                 data=list(),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'))
                  # ensure budget is valid given cost raster
                  assertthat::assert_that()
                 },
                 apply = function(x) {
                  stop('TODO: apply function for maximum_coverage_objective')
                 })
}

#' @rdname objectives
#' @export
phylogenetic_coverage_objective <- function(budget, tree) {
  if (!'ape' %in% rownames(installed.packages()))
    stop('the "ape" package needs to be installed to use phylogenetic data')
  assertthat::assert_that(is.number(budget), 
                          budget > 0.0,
                          inherits(tree, 'phylo'))
  assertthat::assert_that(is.number(budget), 
                          budget > 0.0)
  Objective$new(name='phylogenetic coverage objective',
                 modelsense='max',
                 parameters=parameters(
                  truncated_numeric_parameter('budget', budget)),
                 data=list(tree=tree),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'))
                  # verify that names of features in problem are associated
                  # with leaves in tree
                  
                  # ensure budget is valid given cost raster
                  
                  stop('TODO: validate function for phylogenetic_coverage_objective')
                 },
                 apply = function(x) {
                  stop('TODO: apply function for phylogenetic_coverage_objective')
                 })
}


