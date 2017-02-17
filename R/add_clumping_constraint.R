#' @include internal.R Constraint-proto.R
NULL

#' Add clumping constraints
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
#' # create problem with added clumping constraints with outer edges
#' # recieving the same penalty as inner edges
#' p2 <- p %>% add_clumping_constraint(5, 1)
#'
#' # create problem with added clumping constraints with outer edges
#' # recieving half the penalty as inner edges
#' p3 <- p %>% add_clumping_constraint(5, 0.5)
#'
#' # solve problems
#' s <- stack(solve(p), solve(p2), solve(p3))
#' names(s) <- c('basic solution', 'clumped solution (edge_factor=1)',
#'               'clumped solution (edge_factor=0.5)',
#'
#' # plot solutions
#' plot(s)
#'
#' @export
add_clumping_constraint <- function(x, penalty, edge_factor) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, 'ConservationProblem'),
    isTRUE(all(is.finite(penalty))), assertthat::is.scalar(penalty), 
    isTRUE(penalty >= 0),
    isTRUE(all(is.finite(edge_factor))), assertthat::is.scalar(edge_factor), 
    isTRUE(edge_factor >= 0), isTRUE(edge_factor <= 1))
  # create parameters
  p <- parameters(
    binary_parameter('Apply constraint?', 1L),
    numeric_parameter('Penalty', penalty, lower_limit=0),
    proportion_parameter('Edge factor', edge_factor))
  # create new constraint object
  x$add_constraint(pproto(
    'ClumpingConstraint',
    Constraint,
    name='Clumping constraint',
    parameters=p,
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(x, 'ConservationProblem'))
      if (self$parameters$get('Apply constraint?')==1)
        rcpp_apply_clumping_constraint(x$ptr,
          as_triplet_data_frame(x$boundary_matrix()),
          self$parameters$get('Penalty'),
          self$parameters$get('Edge factor'))
      invisible(TRUE)
    }))
  # return problem
  return(x)
}

