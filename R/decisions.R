#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Problem decision type
#'
#' The type of decision made on a planning unit in a conservation planning 
#' problem. Below is a list of decisions that can be made when solving
#' conservation problems. \strong{Only a single decision type should be added 
#' to a \code{ConservationProblem} object}.
#'
#' \describe{
#'
#'   \item{\code{binary_decision}}{This is the classic decision of either 
#'     prioritizing or not prioritizing a planning unit. Typically,
#'     this decision has the assumed action of buying the planning 
#'     unit to include in a protected area network.}
#'
#'   \item{\code{proportion_decision}}{This is a relaxed decision where
#'     a part of a planning unit can be prioritized. Typically, this
#'     this decsion has the assumed action of buying a fraction of 
#'     a planning unit to include in a protected area network.}
#'
#'   \item{\code{semicontinuous_decision}}{This decision is similar to the 
#'     \code{proportion_decision} except that it has an upper bound 
#'     parameter. By default, the decision can range from prioritizing 
#'     none (0 \%) to all (100 \%) of a planning unit. However, a upper 
#'     bound can be specified to ensure that at most only a fraction 
#'     (eg. 80 \%) of a planning unit can be preserved. This type of
#'     decision may be useful when it is not practical to conserve the 
#'     entire planning unit.}
#'
#'   \item{\code{default_decision}}{This decsion represents the default 
#'     decision if no decision is specified when constructing a conservation
#'     planning problem. It defaults to using a \code{binary_decision}.}
#'
#'  }
#'
#' @param upper \code{numeric} value specifying the maximum proportion
#'   of a planning unit that can be reserved (eg. set to 0.8 for 80 \%).
#'
#' @return \code{\link{Decision}} object.
#'
#' @examples
#' # create basic problem and using the default decision (binary)
#' p <- problem(sim_pu_raster, sim_features) + 
#'      minimium_set_objective() +
#'      relative_targets(0.1)
#'
#' # manually specify a binary decision type 
#' p + binary_decision()
#'
#' # specify a proportion decision type
#' p + proportion_decision()
#' 
#' # specify a semicontinuous decision type
#' p + semicontinuous_decision(bound=0.5)
#'
#' @name decisions
NULL

#' @rdname decisions
#' @export 
binary_decision <- function() {
  pproto('BinaryDecision', Decision, 
    name='binary decision',
    apply=function(self, x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      invisible(rcpp_apply_binary_decision(x))
    }
  )
}

#' @rdname decisions
#' @export
proportion_decision <- function() {
  pproto('ProportionDecision', Decision, 
    name='proportion decision',
    apply=function(self, x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      invisible(rcpp_apply_proportion_decision(x))
    }
  )
}

#' @rdname decisions
#' @export
semicontinuous_decision <- function(upper=1) {
  assertthat::assert_that(assertthat::is.scalar(upper), isTRUE(upper <= 1),
    isTRUE(upper >= 0))
  pproto('SemiContinuousDecision', Decision, 
    name='semicontinuous decision',
    parameters=parameters(proportion_parameter('Upper',upper)),
    apply=function(self, x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      invisible(rcpp_apply_semicontinuous_decision(x,
        self$parameters$get('Upper')))
    }
  )
}

#' @rdname decisions
default_decision <- function() {
  pproto('DefaultDecision', binary_decision())
}

