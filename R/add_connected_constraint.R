#' @include internal.R Constraint-proto.R
NULL

#' Add connected constraint
#'
#' Add constraints to a conservation problem to ensure that all selected
#' planning units are connected.
#'
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @return \code{\link{ConservationProblem}} object with the constraint added
#'   to it.
#' 
#' @seealso \code{\link{constraints}} for all the available constraints.
#'
#' @examples
#' # create basic problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.2)
#'
#' # create problem with added connected constraints
#' p2 <- p %>% add_connected_constraint()
#'
#' # solve problems
#' s <- stack(solve(p), solve(p2))
#' names(s) <- c('basic solution', 'connected solution')
#'
#' # plot solutions
#' plot(s)
#'
#' @export
add_connected_constraint <- function(x) {
  # assert argumnt is valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  # check that at least two planning units are touching each other
  stop('TODO: validity checks')
  # add the constraint
  x$add_constraint(pproto(
    'ConnectedConstraint',
    Constraint, 
    name='Connected constraint',
    parameters=parameters(binary_parameter('Apply constraint?', 1L)),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits('OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      if (self$parameters$get('Apply constraint?')==1)
        rcpp_apply_clumping_constraint(x$ptr, 
          as_triplet_dataframe(x$connected_matrix()))
      invisible(TRUE)
    }))
  # return problem
  return(x)
}
