#' @include internal.R pproto.R ConservationProblem-proto.R zones.R
NULL

#' Add Absolute Targets
#'
#' Set targets expressed as the actual value of features in the study area
#' that need to be represented in the prioritization. For instance,
#' setting a target of 10 requires that the solution secure a set of
#' planning units for which their summed feature values are equal to or greater
#' than 10. In problems associated with multiple zones, this function can be
#' used to set targets that each pertain to a single feature and a single zone.
#' To set targets which can be met through allocating different planning units
#' to multiple zones, see the \code{\link{add_manual_targets}} function.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param targets \code{numeric} \code{vector}, \code{matrix}, or
#'   \code{character} \code{vector} object with targets for
#'   features. The correct argument for \code{targets} depends on multiple
#'   factors:
#'   \describe{
#'     \item{\code{numeric}}{This type of argument can be a
#'        \code{numeric} \code{vector} containing multiple values for each
#'        feature. Additionally, for convenience,
#'        this type of argument can be a single value to assign the
#'        same target to each feature. Note that this type of argument
#'        cannot be used to specify targets for problems with multiple zones.}
#'
#'    \item{\code{matrix}}{This type of argument for \code{targets} can be
#'      used to set targets for each feature in each zone. Here, each
#'      row corresponds to a different feature in argument to \code{x},
#'      each column corresponds to a different zone in argument to \code{x},
#'      and each cell contains the minimum amount of a given feature that the
#'      solution needs to secure in a given zone.}
#'
#'    \item{\code{character}}{This type of argument for \code{targets} can be
#'       used to set the target for each feature using the names of fields
#'       (columns) in the feature data associated with the argument to
#'       \code{x}. This type of argument can only be used when the
#'       feature data associated with \code{x} is a \code{data.frame}.
#'       This argument must contain a field (column) name for each zone.}
#'
#'  }
#'
#' @param ... not used.
#'
#' @inherit add_manual_targets details return seealso
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create base problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_binary_decisions()
#'
#' # create problem with targets to secure 3 amounts for each feature
#' p1 <- p %>% add_absolute_targets(3)
#'
#' # create problem with varying targets for each feature
#' targets <- c(1, 2, 3, 2, 1)
#' p2 <- p %>% add_absolute_targets(targets)
#' \donttest{
#' # solve problem
#' s <- stack(solve(p1), solve(p2))
#'
#' # plot solution
#' plot(s, main = c("targets for 3", "varying targets"), axes = FALSE,
#'      box = FALSE)
#' }
#'
#' @aliases add_absolute_targets-method add_absolute_targets,ConservationProblem,numeric-method add_absolute_targets,ConservationProblem,matrix-method add_absolute_targets,ConservationProblem,character-method
#'
#' @name add_absolute_targets
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
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            x$number_of_zones() == 1,
                            length(targets) %in% c(1, x$number_of_features()))
    add_absolute_targets(x, matrix(targets, nrow = x$number_of_features(),
                                   ncol = 1))
})

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage add_absolute_targets(x, targets, ...) # x=ConservationProblem, targets=matrix
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "matrix"),
  function(x, targets, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      inherits(targets, "matrix"), is.numeric(targets),
      isTRUE(all(is.finite(targets))),
      isTRUE(all(targets >= 0.0)),
      isTRUE(length(targets) > 0),
      nrow(targets) == x$number_of_features(),
      ncol(targets) == x$number_of_zones(),
      all(targets >= 0),
      isTRUE(all(targets <= x$feature_abundances_in_planning_units())))
    # create targets as data.frame
    if (x$number_of_zones() > 1) {
      target_data <- expand.grid(feature = x$feature_names(),
                                 zone = x$zone_names(),
                                 type = "absolute")
    } else {
      target_data <- expand.grid(feature = x$feature_names(),
                                 type = "absolute")
    }
    target_data$target <- as.numeric(targets)
    # add targets to problem
    add_manual_targets(x, target_data)
  })

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage add_absolute_targets(x, targets, ...) # x=ConservationProblem, targets=character
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "character"),
  function(x, targets, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      is.character(targets),
      inherits(x$data$features, "data.frame"),
      !anyNA(targets),
      all(assertthat::has_name(x$data$features, targets)),
      length(targets) == x$number_of_zones(),
      all(vapply(x$data$features[, targets, drop = FALSE], is.numeric,
                logical(1))))
    # add targets to problem
    add_absolute_targets(x, as.matrix(x$data$features[, targets, drop = FALSE]))
})
