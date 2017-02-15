#' @include internal.R Target-proto.R
NULL

#' Targets 
#'
#' After constructing a basic conservation \code{\link{problem}} that specifies
#' the cost and feature data, it needs to have targets added to it. Targets 
#' are used to specify the minimum amount or proportion of a feature's 
#' distribution that needs to be protected. Thus targets ensure that each 
#' species is adequately represented in the reserve network.
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
#' p <- problem(cost=sim_pu_raster, features=sim_features) + 
#'   minimium_set_objective()
#'
#' # add relative targets
#' p + relative_targets(0.1)
#'
#' # add absolute targets
#' p + absolute_targets(3)
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
  pproto(
    'Target',
    Target,
    name='Relative targets',
    parameters=parameters(
      proportion_parameter_array(
        'targets',
        as.double(x),
        as.character(seq_along(x)))),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      n_targets <- nrow(self$parameters$get('targets'))
      if (n_targets!=1)
        invisible(assertthat::see_if(n_targets == x$get_number_of_features()))
      invisible(TRUE)
    },
    synchronize = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      self$data$abundances <- x$feature_abundances()
      p <- self$parameters$get('targets')
      if (nrow(p)==1) {
        targets <- rep(p[[1]], length(x$get_number_of_features()))
      } else {
        targets <- p[[1]]
      }
      self$parameters$parameters[[1]] <- proportion_parameter_array(
        'targets', targets, x$get_feature_names())
      invisible(TRUE)
    },
    postvalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      invisible(assertthat::see_if(
        isTRUE(nrow(self$parameters$get('targets')) ==
          x$get_number_of_features()),
        isTRUE(all(rownames(self$parameters$get('targets')) ==
          x$get_feature_names()))))
    },            
    output = function(self) {
      self$parameters$get('targets')[[1]] * self$data$abundances
    })
}

#' @rdname targets
#' @export
absolute_targets <- function(x) {
  assertthat::assert_that(inherits(x, 'numeric'),
                          all(x >= 0),
                          all(is.finite(x)))
  pproto(
    'Target',
    Target,
    name='Absolute targets',
    parameters=parameters(
      truncated_numeric_parameter_array(
        x='targets', value=as.numeric(x), label=as.character(seq_along(x)))),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      n_targets <- nrow(self$parameters$get('targets'))
      if (n_targets!=1)
        invisible(assertthat::see_if(n_targets == x$get_number_of_features()))
      invisible(TRUE)
    },
    synchronize = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      self$data$abundances <- x$feature_abundances()
      p <- self$parameters$get('targets')
      if (nrow(p)==1) {
        targets <- rep(p[[1]], length(abundances))
      } else {
        targets <- p[[1]]
      }
      self$parameters$parameters[[1]] <- truncated_numeric_parameter_array(
          x='targets', value=targets, label=x$get_feature_names(),
          upper_limit=self$data$abundances)
      invisible(TRUE)
    },
    postvalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      invisible(assertthat::see_if(
        isTRUE(nrow(self$parameters$get('targets')) ==
          x$get_number_of_features()),
        isTRUE(all(rownames(self$parameters$get('targets')) ==
          x$get_feature_names()))))
    },
    output = function(self) {
      self$parameters$get('targets')[[1]]
    })
}

#' @rdname targets
#' @export
loglinear_targets <- function(x) {
  assertthat::assert_that(inherits(x, 'numeric'),
                          isTRUE(all(x >= 0)),
                          isTRUE(all(x[1:2] >= 0.0)),
                          isTRUE(all(x[1:2] <= 1.0)),
                          isTRUE(all(x[3:4] >= 0.0)),
                          isTRUE(length(x)==4),
                          assertthat::noNA(x))
  pproto(
    'Target',
    Target,
    name='Log-linear targets',
    parameters=parameters(
      truncated_numeric_parameter(
        x='Minimum distribution threshold', value=x[1]),
      truncated_numeric_parameter(
        x='Maximum distribution threshold', value=x[2]),
      proportion_parameter(
        x='Target at minimum threshold', value=x[3]),
      proportion_parameter(
        x='Target at maximum threshold', value=x[4]),
    ),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      assertthat::see_if(
        self$parameters$get('Minimum distribution threshold') < 
          self$parameters$get('Maximum distribution threshold'),
        self$parameters$get('Target at minimum threshold') > 
          self$parameters$get('Target at maximum threshold'))
    },
    synchronize = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      self$data$abundances <- x$feature_abundances()
      invisible(TRUE)       
    },
    output = function(self) {
      targets <- loglinear_interpolate(
        self$data$abundances, 
        self$parameters$get('Minimum distribution threshold'),
        self$parameters$get('Target at minimum threshold'),
        self$parameters$get('Maximum distribution threshold'),
        self$parameters$get('Target at maximum threshold'))
      targets * cs
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
#' p <- problem(cost=sim_pu_raster, features=sim_features) + 
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
  assertthat::assert_that(
    inherits(x, 'numeric'),
    isTRUE(all(x >= 0)),
    assertthat::noNA(x))
  pproto(
    'Constraint'
    Constraint,
    name='Target weights',
    parameters=parameters(
      truncated_numeric_parameter_array(
        name='Target weights',
        as.numeric(x),
        as.character(seq_along(x)))),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      invisible(assertthat::see_if(
        name(x$objective) == 'maximum_coverage_problem',
        nrow(self$parameters$get('Target weights')) ==
          x$get_number_of_features()))
    },
    synchronize = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      self$parameters$parameters$labels <- x$get_feature_names()
      invisible(TRUE)
    },
    postvalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      invisible(assertthat::see_if(
        name(x$objective) == 'maximum_coverage_problem',
        nrow(self$parameters$get('Target weights')) ==
          x$get_number_of_features(),
        rownames(self$parameters$get('Target weights')) ==
          x$get_feature_names()))
    },
    apply = function(self, x) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      invisible(rcpp_apply_target_weights(x,
        self$parameters$get('Target weights')[[1]]))
    })
}
