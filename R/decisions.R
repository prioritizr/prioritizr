#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add problem decision type
#'
#' Conservation planning problems involve making decisions on planning units.
#' These decisions are then associated with actions (eg. turning a planning 
#' unit into a protected area). Below is a list of decisions that can be
#' added to a conservation planning problem.
#'
#' \describe{
#'
#'   \item{\code{add_binary_decision}}{This is the classic decision of either 
#'     prioritizing or not prioritizing a planning unit. Typically,
#'     this decision has the assumed action of buying the planning 
#'     unit to include in a protected area network.}
#'
#'   \item{\code{add_proportion_decision}}{This is a relaxed decision where
#'     a part of a planning unit can be prioritized. Typically, this
#'     this decsion has the assumed action of buying a fraction of 
#'     a planning unit to include in a protected area network.}
#'
#'   \item{\code{add_semicontinuous_decision}}{This decision is similar to the 
#'     \code{proportion_decision} except that it has an upper bound 
#'     parameter. By default, the decision can range from prioritizing 
#'     none (0 \%) to all (100 \%) of a planning unit. However, a upper 
#'     bound can be specified to ensure that at most only a fraction 
#'     (eg. 80 \%) of a planning unit can be preserved. This type of
#'     decision may be useful when it is not practical to conserve the 
#'     entire planning unit.}
#'
#'   \item{\code{add_default_decision}}{This decsion represents the default 
#'     decision if no decision is specified when constructing a conservation
#'     planning problem. It defaults to using a \code{binary_decision}.}
#'
#'  }
#'
#' @param x \code{ConservationProblem} object.
#'
#' @param upper_limit \code{numeric} value specifying the maximum proportion
#'   of a planning unit that can be reserved (eg. set to 0.8 for 80 \%).
#'
#' @details Only a single decision should be added to a 
#' \code{ConservationProblem} object. \strong{If multiple decisions are added 
#' to a problem object, then the last one to be added will be used.}
#'
#' @return \code{\link{Decision}} object.
#'
#' @examples
#' # create basic problem and using the default decision (binary)
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_minimum_set_objective() %>%
#'      add_relative_targets(0.1)
#'
#' # manually specify a binary decision type 
#' p2 <- p %>% add_binary_decision()
#'
#' # specify a proportion decision type
#' p3 <- p %>% add_proportion_decision()
#' 
#' # specify a semicontinuous decision type
#' p4 <- p %>% add_semicontinuous_decision(upper_limit=0.5)
#'
#' # solve problem
#' s <- stack(solve(p), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' plot(s, main = c('default (binary)', 'binary', 'proportion', 
#'                  'semicontinuous (upper=0.5)'))
#'
#' @name decisions
NULL

#' @rdname decisions
#' @export 
add_binary_decision <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  # add decision
  x$add_decision(pproto('BinaryDecision', Decision, 
    name='Binary decision',
    apply=function(self, x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      invisible(rcpp_apply_binary_decision(x$ptr))
    }
  ))
}

#' @rdname decisions
#' @export
add_proportion_decision <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  # add decision  
  x$add_decision(pproto('ProportionDecision', Decision, 
    name='Proportion decision',
    apply=function(self, x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      invisible(rcpp_apply_proportion_decision(x$ptr))
    }
  ))
}

#' @rdname decisions
#' @export
add_semicontinuous_decision <- function(x, upper_limit) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'), 
    isTRUE(all(is.finite(upper_limit))), assertthat::is.scalar(upper_limit), 
    isTRUE(upper_limit <= 1), isTRUE(upper_limit >= 0))
  # add decision to problem
  x$add_decision(pproto('SemiContinuousDecision', Decision, 
    name='Semicontinuous decision',
    parameters=parameters(proportion_parameter('upper limit', upper_limit)),
    apply=function(self, x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      invisible(rcpp_apply_semicontinuous_decision(x$ptr,
        self$parameters$get('upper limit')))
    }))
}


#' @rdname decisions
add_default_decision <- function(x) {
  add_binary_decision(x)
}
