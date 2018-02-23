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
#' @param targets \code{numeric} \code{vector}, \code{character},
#'   \code{matrix}, or \code{\link{ZonesCharacter}} object with targets for
#'   features. The correct argument for \code{targets} depends on multiple
#'   factors:
#'   \describe{
#'     \item{\code{numeric}}{This type of argument can be a
#'        \code{numeric} \code{vector} containing multiple values for each
#'        feature. Additionally, for convenience,
#'        this type of argument can be a single value to assign each feature
#'        the same target. If the argument to \code{x} contains multiple zones,
#'        each feature is assigned the same target in each zone. Thus
#'        \code{numeric} arguments cannot be used to specify different targets
#'        for different features in different zones.}
#'
#'    \code{\code{matrix}}{This type of argument for \code{targets} can be
#'      used to set targets for each feature in each zone. Here, each
#'      row corresponds to a different feature in argument to \code{x},
#'      each column corresponds to a different zone in argument to \code{x},
#'      and each cell contains the minimum amount of a given feature that the
#'      solution needs to secure in a given zone.}
#'
#'    \item{\code{character}}{This type of argument for \code{targets} can be
#'       used to set the target for each feature using the names of fields
#'       (columns) in the feature data associated with the argument to
#'       \code{x}. If the argument to \code{x} contains multiple zones,
#'        each feature is assigned the same target in each zone. Thus
#'        as with \code{numeric} arguments to \code{target}, \code{character}
#'        arguments cannot be used to specify different targets for different
#'        features in different zones.}
#'
#'    \code{\code{\link{ZonesCharacter}}}{This type of argument for
#'      \code{targets} can be to set targets for each feature in each zone
#'      using the names of fields (columns) in the feature data associated
#'      with the argument to \code{x}.}
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
#' @aliases add_absolute_targets-method add_absolute_targets,ConservationProblem,numeric-method add_absolute_targets,ConservationProblem,matrix-method add_absolute_targets,ConservationProblem,character-method add_absolute_targets,ConservationProblem,ZonesCharacter-method
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
    add_absolute_targets(x, matrix(targets, ncol = 1))
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
      isTRUE(length(targets) > 0))
    # assert that targets are compatible with problem
    if (nrow(targets) == 1 && ncol(targets) == 1) {
      targets <- matrix(targets, nrow = x$number_of_features(),
                        ncol = x$number_of_zones())
    }
    assertthat::assert_that(
      nrow(targets) == x$number_of_features(),
      ncol(targets) == x$number_of_zones(),
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
    target_data$target <- c(targets)
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
      inherits(x$data$features, c("data.frame", "Spatial")),
      assertthat::has_name(x$data$features, targets),
      length(targets) == x$data$features())
    # add targets to problem
    add_absolute_targets(x, zones(targets))
 })

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage add_absolute_targets(x, targets, ...) # x=ConservationProblem, targets=ZonesCharacter
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "ZonesCharacter"),
  function(x, targets, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "ConservationProblem"),
     inherits(x$data$features, c("data.frame", "Spatial")),
     n_zone(targets) == x$number_of_zones(),
     n_feature(targets) == x$number_of_features(),
     assertthat::has_name(x$data$features, unlist(as.list(targets))),
     all(vapply(x$data$features[, unlist(as.list(targets)), drop = FALSE],
                is.numeric, logical(1))))
    # extract target data
    targets <- x$data$features[, unlist(as.list(targets)), drop = FALSE]
    targets <- as.matrix(targets)
    # add targets to problem
    add_absolute_targets(x, targets)
})
