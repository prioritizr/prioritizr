#' @include internal.R Target-class.R
NULL

#' Targets 
#'
#' After constructing a basic conservation \code{\link{problem}} that specifies
#' the cost and feature data, it needs to have targets added to it. Targets 
#' are used to specify the minimum amount or proportion of a feature's distribution
#' that needs to be protected. Thus targets ensure that each species is 
#' adequately represented in the reserve network.
#' 
#' @param x \code{numeric} vector. 
#' 
#' @details Targets can be specified using one of the functions below.
#'   \describe{
#'     \item{relative_targets}{Targets are expressed as a proportion (between 0
#'       and 1) of the maximum level of representation in the study area.
#'       The argument to \code{x} should have a single value if all features
#'       have the same target. Otherwise, the vector should have a value for
#'       each feature. In this case, targets are assigned to features based
#'       on the their position in the argument to \code{x} and the
#'       \code{feature} when specifying the problem.}
#'     \item{absolute_targets}{Targets are expressed as the actual value that
#'       needs to be represented in the prioritisation. The argument to 
#'       \code{x} is treated the same as for \code{relative_targets}.}
#'     \item{loglinear_targets}{Targets are expressed as a proportion (between 0
#'       and 1) which is calculated using a log-linear equation and four
#'       tuning parameters (as used in XXX \emph{et al.} XXX). The first
#'       tuning parameter specifies the first cut-off range size,
#'       and the second the second cut-off range size, the third argument
#'       specifies the target required for species with a range size equal
#'       to or less than the first cut-off range size, and the fourth
#'       argument specifies the target required for species with a range
#'       size equal to or greater than the required range size.}
#'  }
#'
#' @return \code{\link{Constraint}} object containing the target data.
#'
#' @seealso \code{\link{objectives}}, \code{\link{problem}}, 
#'  \code{\link{target_weights}}.
#'
#' @examples
#' # create basic problem
#' p <- problem(cost=sim_pu, features=sim_features) + 
#'   minimium_set_objective()
#'
#' # add relative targets
#' p + relative_targets(0.1)
#'
#' # add absolute targets
#' p + absolute_targets(0.1)
#'
#' # add log-linear target
#' p + loglinear_targets(c(1, 10, 0.9, 0.2))
#'
#' @name targets
NULL

#' @rdname targets
#' @export
relative_targets <- function(x) {
  assertthat::assert_that(inherits(x, 'numeric'),
                          all(x >= 0.0), all(x <= 1.0),
                          all(is.finite(x)))
  Target$new(name='Relative targets',
                 parameters=parameters(
                  proportion_parameter_array(
                    name='targets',
                    as.numeric(x),
                    seq_along(x))),
                 data=list(),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'))
                  if (nrow(self$parameters$get('targets') > 1))
                      assertthat::assert_that(
                        nrow(self$parameters$get('targets') == 
                        raster::nlayers(x$features)))
                  invisible(TRUE)
                 },
                 apply = function(x) {
                  stop('TODO: implement apply methed for relative_targets')
                 })
}

#' @rdname targets
#' @export
absolute_targets <- function(x) {
  assertthat::assert_that(inherits(x, 'numeric'),
                          all(x >= 0),
                          all(is.finite(x)))
  Target$new(name='Absolute targets',
                 parameters=parameters(
                  truncated_numeric_parameter_array(
                    name='targets',
                    as.numeric(x),
                    seq_along(x))),
                 data=list(),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'))
                  if (nrow(self$parameters$get('targets') > 1))
                      assertthat::assert_that(
                        nrow(self$parameters$get('targets') == 
                        raster::nlayers(x$features)))
                  invisible(TRUE)
                 },
                 apply = function(x) {
                  stop('TODO: implement apply method for absolute_targets')
                 })
}

#' @rdname targets
#' @export
loglinear_targets <- function(x) {
  assertthat::assert_that(inherits(x, 'numeric'),
                          all(x >= 0),
                          all(x[1:2]) >= 0.0,
                          all(x[1:2]) <= 1.0,
                          all(x[3:4]) >= 0.0,
                          length(x==4),
                          all(is.finite(x)))
  Target$new(name='Log-linear targets',
                 parameters=parameters(
                  truncated_numeric_parameter(
                    name='Minimum distribution threshold',
                    x[1]),
                  truncated_numeric_parameter(
                    name='Maximum distribution threshold',
                    x[2]),
                  proportion_parameter(
                    name='Target at minimum threshold', 
                    x[3]),
                  proportion_parameter(
                    name='Target at maximum threshold', 
                    x[3]),
                ),
                data=list(),
                validate = function(x) {
                  assertthat::assert_that(
                    inherits(x, 'ConservationProblem'),
                    self$parameters$get('Minimum distribution threshold') < 
                      self$parameters$get('Maximum distribution threshold'),
                    self$parameters$get('Target at minimum threshold') < 
                      self$parameters$get('Target at maximum threshold'))
                },
                apply = function(x) {
                  assertthat::assert_that(inherits(x, 'OptimizationProblem'))
                  stop('TODO: implement apply methed for loglinear_targets')
                })
}

#' Target weights
#' 
#' Targets are used to express the minimum amount or poportion of a 
#' feature's distribution required to preserve it in a protected area
#' network. However, when using a maximum coverage problem with a pre-specified
#' budget, it may not be possible to preserve all features. Target weights can
#' be used to favour the representation of some features over other features
#' when making decisions about how the budget should be allocated.
#' 
#' @param x \code{numeric} target weights. Higher values for a feature indicate
#'   that it is more desireable to represent the feature in a network. Weights
#'   cannot have negative values.
#'
#' @details Note that target weights can only be applied to a maximum coverage
#'   type of planning problem (eg. \code{\link{maximum_coverage_objective}},
#'   \code{\link{phylogenetic_coverage_objective}}).
#'
#' @return \code{\link{Constraint}} object containing the weight data.
#'
#' @seealso \code{\link{targets}}.
#'
#' @examples
#' # create problem
#' p <- problem(cost=sim_pu, features=sim_features) + 
#'   maximum_coverage_objective() +
#'   relative_targets(0.1)
#' 
#' # create weights based on rarity (1/number occurrences)
#' w <- 1/raster::cellStats(sim_features, 'sum') 
#'
#' # add weight the targets in a problem
#' p <- p + target_weights(w)
#'
#' @export
target_weights <- function(x) {
  assertthat::assert_that(inherits(x, 'numeric'),
                          all(x >= 0),
                          all(is.finite(x)))
  Constraint$new(name='Target weights',
                 parameters=parameters(
                  truncated_numeric_parameter_array(
                    name='Target weights',
                    as.numeric(x),
                    seq_along(x))),
                 data=list(),
                 validate = function(x) {
                  assertthat::assert_that(inherits(x, 'ConservationProblem'))
                  assertthat::assert_that(
                    name(x$objective) == 'maximum_coverage_problem',
                    nrow(self$parameters$get('Target weights')) ==
                      raster::nlayers(x$features))
                 },
                 apply = function(x) {
                  stop('TODO: function to apply target weights')
                 })
}
