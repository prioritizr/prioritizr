#' @include internal.R Constraint-proto.R
NULL

#' Add boundary constraints
#'
#' Add constraints to a conservation problem that favor solutions that clump
#' selected planning units together into contiguous reserves.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param penalty \code{numeric} penalty for exposed edges. This
#'  is equivalent to the
#'  \href{http://marxan.net/downloads/uq_marxan_web_2/module2.html}{boundary 
#'  length modifier (BLM)} parameter in \href{marxan.net}{Marxan}.
#'
#' @param edge_factor \code{numeric} proportion to scale edges that do
#'   not have any neighboring planning units. For example, an edge factor
#'   of \code{0.5} is commonly used for planning units along the coast line.
#'
#' @return \code{\link{ConservationProblem}} object.
#' 
#' @seealso \code{\link{constraints}} for a list of all available constraints.
#'
#' @examples
#' # create basic problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.2)
#'
#' # create problem with low boundary penalties
#' p2 <- p %>% add_boundary_constraint(5, 1)
#'
#' # create problem with high boundary penalties
#' # receive half the penalty as inner edges
#' p3 <- p %>% add_boundary_constraint(100, 1)
#'
#' # solve problems
#' solve(p)
#' s <- stack(solve(p), solve(p2), solve(p3))
#'
#' # plot solutions
#' plot(s, main = c('basic solution', 'small boundary constraints', 
#'                  'high boundary constraints'))
#'
#' @export
add_boundary_constraint <- function(x, penalty, edge_factor) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, 'ConservationProblem'),
    isTRUE(all(is.finite(penalty))), assertthat::is.scalar(penalty), 
    isTRUE(penalty >= 0),
    isTRUE(all(is.finite(edge_factor))), assertthat::is.scalar(edge_factor), 
    isTRUE(edge_factor >= 0), isTRUE(edge_factor <= 1))
  # create parameters
  p <- parameters(
    binary_parameter('apply constraint?', 1L),
    numeric_parameter('penalty', penalty, lower_limit=0),
    proportion_parameter('edge factor', edge_factor))
  # create new constraint object
  x$add_constraint(pproto(
    'BoundaryConstraint',
    Constraint,
    name='Boundary constraint',
    parameters=p,
    calculate=function(self, x) {
        assertthat::assert_that(inherits(x, 'ConservationProblem'))
        if (is.Waiver(x$get_data('boundary_matrix'))) {
          # create boundary matrix
          m <- boundary_matrix(x$get_data('cost'))
          # manually coerce boundary matrix to 'dgCMatrix' class so that
          # elements in the lower diagonal are not filled in
          class(m) <- 'dgCMatrix'
          # store data
          x$set_data('boundary_matrix', m)
        }
        # return invisible
        invisible()
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      if (isTRUE(self$parameters$get('apply constraint?')==1) &
          isTRUE(self$parameters$get('penalty') > 1e-10)) {
        # apply constraint
        rcpp_apply_boundary_constraint(x$ptr, y$get_data('boundary_matrix'), 
          self$parameters$get('penalty'), self$parameters$get('edge factor'))
      }
      invisible(TRUE)
    }))
}

