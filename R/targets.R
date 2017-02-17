#' @include internal.R Target-proto.R
NULL

#' Targets 
#'
#' Targets are used to specify the minimum amount or proportion of a feature's 
#' distribution that needs to be protected. Below is a list of different 
#' targets that can be added to a conservation planning \code{\link{problem}}.
#' 
#' \describe{
#'   \item{\code{relative_targets}}{Targets are expressed as a proportion 
#'     (between 0
#'     and 1) of the maximum level of representation in the study area.
#'     The argument to \code{x} should have a single value if all features
#'     have the same target. Otherwise, the vector should have a value for
#'     each feature. In this case, targets are assigned to features based
#'     on the their position in the argument to \code{x} and the
#'     \code{feature} when specifying the problem.}
#'
#'   \item{\code{absolute_targets}}{Targets are expressed as the actual value 
#'     that needs to be represented in the prioritisation. The argument to 
#'     \code{x} is treated the same as for \code{relative_targets}.}
#'
#'   \item{\code{loglinear_targets}}{Targets are expressed as a proportion 
#'     (between 0
#'     and 1) which is calculated using a log-linear equation and four
#'     tuning parameters (as used in XXX \emph{et al.} XXX). The first
#'     tuning parameter specifies the first cut-off range size,
#'     and the second the second cut-off range size, the third argument
#'     specifies the target required for species with a range size equal
#'     to or less than the first cut-off range size, and the fourth
#'     argument specifies the target required for species with a range
#'     size equal to or greater than the required range size.}
#'
#'   \item{\code{default_targets}}{The default targets are used when targets
#'     have not explicitly been set using the above functions. The creators
#'     of this package do not believe that there can be any sensible default
#'     targets. Thus relying on the default target will yield an error. }
#'
#'  }
#'
#' @param x \code{\link{ConservationProblem}} object.
#' 
#' @param targets \code{numeric} targets for features. If all features should
#'   have the same target, \code{targets} can be a single number. Otherwise, 
#'   \code{targets} should be a \code{numeric} \code{vector} specifying a 
#'   target for each feature.
#'
#' @param lower_bound_amount \code{numeric} lower bound for the total amount
#'   of the features.
#'
#' @param lower_bound_target \code{numeric} relative target that should be 
#'   applied to features with a total amount that is less 
#'  than or equal to \code{lower_bound_amount}.
#'
#' @param upper_bound_target \code{numeric} upper bound for the total amount
#'   of features.
#'
#' @param upper_bound_target \code{numeric} relative target that should be 
#'   applied to features with a total amount that is greater
#'   than or equal to \code{upper_bound_amount}.
#'
#' @return \code{\link{ConservationProblem}} object with the target added
#'   to it.
#'
#' @seealso \code{\link{constraints}}, \code{\link{objectives}},
#'   \code{\link{problem}}, \code{\link{target_weights}}.
#'
#' @examples
#' # create basic problem
#' p <- problem(cost=sim_pu_raster, features=sim_features) %>%
#'   minimium_set_objective()
#'
#' # add relative targets
#' p %>% relative_targets(0.1)
#'
#' # add absolute targets
#' p %>% absolute_targets(3)
#'
#' # add log-linear target
#' p %>% loglinear_targets(c(1, 10, 0.9, 0.2))
#'
#' @name targets
NULL

#' @rdname targets
#' @export
add_relative_targets <- function(x, targets) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, 'ConservationProblem'),
    inherits(targets, 'numeric'), isTRUE(all(is.finite(targets))), 
    isTRUE(all(targets >= 0.0)),  isTRUE(all(targets <= 1.0)), 
    isTRUE(length(targets) > 0))
  # assert that targets are compatible with problem
  if (length(targets) != 1)
    assertthat::assert_that(length(targets) == x$number_of_features())
  # create target parameters
  if (length(targets)==1) {
    targets <- rep(targets, x$number_of_features())
  }
  targets <- proportion_parameter_array('targets', targets, x$feature_names())
  # add targets to problem
  x$add_targets(pproto(
    'RelativeTargets',
    Target,
    name='Relative targets',
    data = list(abundances=x$feature_abundances_in_planning_units()),
    parameters=parameters(targets),
    output = function(self) {
      self$parameters$get('targets')[[1]] * self$data$abundances
    }))
  # return problem
  return(x)
}

#' @rdname targets
#' @export
add_absolute_targets <- function(x, targets) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, 'ConservationProblem'),
    inherits(targets, 'numeric'), isTRUE(all(is.finite(targets))), 
    isTRUE(all(targets >= 0.0)), isTRUE(length(targets) > 0))
  # assert that targets are compatible with problem
  if (length(targets)==1) {
    targets <- rep(targets, x$number_of_features())
  }
  assertthat::assert_that(length(targets)==x$number_of_features(),
    isTRUE(all(targets <= x$feature_abundances_in_planning_units())))
  # create target parameters
  targets <- numeric_parameter_array('targets', targets, x$feature_names(),
    lower_limit=rep(0, x$number_of_features()), 
    upper_limit=x$feature_abundances_in_planning_units())
  # add targets to problem  
  x$add_targets(pproto(
    'AbsoluteTargets',
    Target,
    name='Absolute targets',
    parameters=parameters(targets),
    output = function(self) {
      self$parameters$get('targets')[[1]]
    }))
  # return problem
  return(x)
}

#' @rdname targets
#' @export
add_loglinear_targets <- function(x, lower_bound_amount, 
                                  lower_bound_target,
                                  upper_bound_amount, 
                                  upper_bound_target) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'),
    isTRUE(all(is.finite(lower_bound_amount))), 
    assertthat::is.scalar(lower_bound_amount), isTRUE(lower_bound_amount>=0),
    isTRUE(all(is.finite(upper_bound_amount))),
    assertthat::is.scalar(upper_bound_amount), isTRUE(upper_bound_amount>=0),
    isTRUE(all(is.finite(lower_bound_target))), 
    assertthat::is.scalar(lower_bound_target), 
    isTRUE(lower_bound_target>=0), isTRUE(lower_bound_target<=1),
    isTRUE(all(is.finite(upper_bound_target))), 
    assertthat::is.scalar(upper_bound_target), 
    isTRUE(upper_bound_target>=0), isTRUE(upper_bound_target<=1),
    isTRUE(upper_bound_amount > lower_bound_amount))
  # create parameters
  p <- parameters(
    numeric_parameter('Amount at lower bound', value=lower_bound_amount,
      lower_limit=0),
    numeric_parameter('Amount at upper bound', value=upper_bound_amount,
      lower_limit=0),
    proportion_parameter('Target at lower bound', value=lower_bound_target),
    proportion_parameter('Target at upper bound', value=upper_bound_target))
  # add targets to problem
  x$add_targets(pproto(
    'LogLinearTargets',
    Target,
    name='Log-linear targets',
    parameter=p,
    data=list(abundances = x$feature_abundances_in_planning_units()),
    output = function(self) {
      loglinear_interpolate(self$data$abundances, 
        self$parameters$get('Amount at lower bound'),
        self$parameters$get('Target at lower bound'),
        self$parameters$get('Amount at upper bound'),
        self$parameters$get('Target at upper bound')) * self$data$abundances
    }))
  # return problem
  return(x)
}

#' @rdname targets
#' @export
add_default_targets <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, 'ConservationProblem'))
  # throw error because targets must be chosen by the user
  stop('problem is missing targets and they must be explicitly defined')
}

#' Target weights
#' 
#' Targets are used to express the minimum amount or poportion of a 
#' feature's distribution that is required for it to be adequately conserved
#' in a protected area network. However, when using a maximum coverage problem 
#' it may not be possible to preserve all features. Target weights can
#' be used to favour the representation of some features over other features
#' when making decisions about how the budget should be allocated.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param weights \code{numeric} target weights. Higher values for a feature 
#'   indicate that it is more desireable to represent the feature in a network. 
#'   Weights cannot have negative values.
#'
#' @details Target weights can only be applied to a maximum coverage
#'   type of planning problem (eg. \code{\link{maximum_coverage_objective}},
#'   \code{\link{phylogenetic_coverage_objective}}).
#'
#' @return \code{\link{ConservationProblem}} object with the target weights
#'   added to it.
#'
#' @seealso \code{\link{targets}}.
#'
#' @examples
#' # create problem
#' p <- problem(cost=sim_pu_raster, features=sim_features) %>%
#'   maximum_coverage_objective() %>%
#'   relative_targets(0.1)
#' 
#' # create weights based on rarity (1/number occurrences)
#' w <- 1/raster::cellStats(sim_features, 'sum') 
#'
#' # add weight the targets in a problem
#' p %>% target_weights(w)
#'
#' @export
target_weights <- function(x, weights) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, 'ConservationProblem'),
    inherits(weights, 'numeric'), isTRUE(all(is.finite(weights))), 
    isTRUE(all(weights >= 0.0)), isTRUE(length(weights) > 0))
  # assert that weights are compatible with problem
  assertthat::assert_that(length(weights) == x$number_of_features())
  # create weight parameters
  weights <- numeric_parameter_array('weights', weights, x$feature_names(),
    lower_limit = rep(0, x$number_of_features()))
  # add targets to problem
  x$add_constraint(pproto(
    'TargetWeights',
    Constraint,
    name='Target weights',
    parameters=parameters(weights),
    apply = function(self, x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      invisible(rcpp_apply_target_weights(x,
        self$parameters$get('Target weights')[[1]]))
    }))
  # return problem
  return(x)
}
