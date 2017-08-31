#' @include internal.R pproto.R ConservationProblem-proto.R
NULL

#' Add Absolute Targets
#'
#' Set targets expressed as the actual value of features in the study area 
#' that need to be represented in the prioritization. The argument to 
#' \code{x} is treated the same as for \code{\link{add_relative_targets}}.
#' 
#' Note that with the exception of the maximum cover problem, targets must 
#' be added to a \code{\link{problem}} or solving will return an error.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param targets \code{numeric} targets for features. If all features should
#'   have the same target, \code{targets} can be a single number. Otherwise,
#'   \code{targets} should be a \code{numeric} \code{vector} specifying a
#'   target for each feature. Alternatively, if the features in
#'   \code{x} were specified using a \code{data.frame} object, then
#'   argument to \code{targets} may refer to a column name.
#'
#' @param ... not used.
#' 
#' @details
#' Targets are used to specify the minimum amount or proportion of a feature's
#' distribution that needs to be protected. All conservation planning problems require 
#' adding targets with the exception of the maximum cover problem 
#' (see \code{\link{add_max_cover_objective}}), which maximizes all features 
#' in the solution and therefore does not require targets. 
#'
#' @return \code{\link{ConservationProblem-class}} object with the target added
#'   to it.
#'
#' @seealso \code{\link{targets}}, \code{\link{constraints}}, \code{\link{objectives}},
#'   \code{\link{problem}},  \code{\link{add_relative_targets}}, \code{\link{add_loglinear_targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create basic problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective()
#'
#' # create problem with added relative targets
#' p1 <- p %>% add_relative_targets(0.1)
#'
#' # create problem with added absolute targets
#' p2 <- p %>% add_absolute_targets(3)
#'
#' # create problem with added log-linear target
#' p3 <- p %>% add_loglinear_targets(10, 0.9, 100, 0.2)
#'
#' \donttest{
#' # solve solutions
#' s <- stack(solve(p1), solve(p2), solve(p3))
#'
#' # plot solutions
#' plot(s, main=c("relative targets", "absolute targets",
#'                "log-linear targets"))
#' }
#'
#' @aliases add_absolute_targets-method add_absolute_targets,ConservationProblem,numeric-method add_absolute_targets,ConservationProblem,character-method
#' 
#' @name add_absolute_targets
#' @docType methods
NULL

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @exportMethod add_absolute_targets
#' @export
methods::setGeneric(
  "add_absolute_targets",
  signature = methods::signature("x", "targets"),
  function(x, targets, ...) standardGeneric("add_absolute_targets"))

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage add_absolute_targets(x, targets, ...) # x=ConservationProblem, targets=numeric
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "numeric"),
  function(x, targets, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      inherits(targets, "numeric"), isTRUE(all(is.finite(targets))),
      isTRUE(all(targets >= 0.0)), isTRUE(length(targets) > 0))
    # assert that targets are compatible with problem
    if (length(targets) == 1) {
      targets <- rep(targets, x$number_of_features())
    }
    assertthat::assert_that(length(targets) == x$number_of_features(),
                            isTRUE(all(targets <= x$feature_abundances_in_planning_units())))
    # create target parameters
    targets <- numeric_parameter_array("targets", targets, x$feature_names(),
                                       lower_limit = rep(0, x$number_of_features()),
                                       upper_limit = x$feature_abundances_in_planning_units())
    # add targets to problem
    x$add_targets(pproto(
      "AbsoluteTargets",
      Target,
      name = "Absolute targets",
      parameters = parameters(targets),
      output = function(self) {
        self$parameters$get("targets")[[1]]
      }))
  })

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage add_absolute_targets(x, targets, ...) # x=ConservationProblem, targets=character
methods::setMethod("add_absolute_targets",
                   methods::signature("ConservationProblem", "character"),
                   function(x, targets, ...) {
                     # assert that arguments are valid
                     assertthat::assert_that(inherits(x, "ConservationProblem"),
                                             inherits(x$data$features, "data.frame"),
                                             assertthat::has_name(x$data$features, targets))
                     # add targets
                     add_absolute_targets(x, x$data$features[[targets]])
                   })
