#' @include internal.R pproto.R ConservationProblem-proto.R
NULL

#' Targets
#'
#' Targets are used to specify the minimum amount or proportion of a feature's
#' distribution that needs to be protected.
#'
#' Please note that failing to specify targets will return a default
#' error message when solving.The exception is the maximum cover problem 
#' (see \code{\link{add_max_cover_objective}}), which maximizes all features 
#' in the solution and therefore does not require targets. 
#'
#' @details
#' The following list contains the targets can be added to a conservation planning \code{\link{problem}}.
#'
#' \describe{
#'
#'   \item{\code{\link{add_relative_targets}}}{Set targets as a proportion (between 0 and 1)
#'   of the maximum level of representation of features in the study area. The argument
#'   to \code{x} should have a single value if all features have the same target. Otherwise,
#'   the vector should have a value for each feature. In this case, targets are assigned to
#'   features based on their position in the argument to \code{x} and the \code{feature}
#'   when specifying the problem.}
#'
#'   \item{\code{\link{add_absolute_targets}}}{Set targets expressed as the actual value
#'   of features in the study area that need to be represented in the prioritization. The
#'   argument to \code{x} is treated the same as for \code{add_relative_targets}.}
#'
#'   \item{\code{\link{add_loglinear_targets}}}{Set targets as a proportion (between 0 and 1)
#'   and calculated using a log-linear equation and four tuning parameters (as used in
#'   Rodrigues \emph{et al.} 2004). The first tuning parameter specifies the first cut-off
#'   range size, and the second specifies the second cut-off range size, the third argument
#'   specifies the target required for species with a range size equal to or less than the
#'   first cut-off range size, and the fourth argument specifies the target required for
#'   species with a range size equal to or greater than the required range size.}
#'
#'  }
#'
#' @seealso \code{\link{constraints}}, \code{\link{objectives}},
#'   \code{\link{problem}}, \code{\link{add_feature_weights}}.
#'
#' @name targets
NULL

add_default_targets <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # throw error because targets must be chosen by the user
  stop("problem is missing targets and they must be explicitly defined")
}

