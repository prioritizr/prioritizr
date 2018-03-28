#' @include internal.R pproto.R ConservationProblem-proto.R zones.R
NULL

#' Add absolute targets
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
#' @param targets Object that specifies the targets for each feature. See the
#'   Details section for more information.
#'
#' @inherit add_manual_targets return seealso
#'
#' @details Targets are used to specify the minimum amount or proportion of a
#'   feature's distribution that needs to be protected. Most conservation
#'   planning problems require targets with the exception of the maximum cover
#'   (see \code{\link{add_max_cover_objective}}) and maximum utility
#'   (see \code{\link{add_max_utility_objective}}) problems. Attempting to solve
#'   problems with objectives that require targets without specifying targets
#'   will throw an error.
#'
#'   The targets for a problem can be specified in several different ways:
#'
#'   \describe{
#'
#'   \item{\code{numeric}}{\code{vector} of target values for each feature.
#'     Additionally, for convenience, this type of argument can be a single
#'     value to assign the same target to each feature. Note that this type of
#'     argument cannot be used to specify targets for problems with multiple
#'     zones.}
#'
#'   \item{\code{matrix}}{containing a target for each feature in each zone.
#'     Here, each row corresponds to a different feature in argument to
#'     \code{x}, each column corresponds to a different zone in argument to
#'     \code{x}, and each cell contains the target value for a given feature
#'     that the solution needs to secure in a given zone.}
#'
#'   \item{\code{character}}{containing the names of fields (columns) in the
#'     feature data associated with the argument to \code{x} that contain
#'     targets. This type of argument can only be used when the
#'     feature data associated with \code{x} is a \code{data.frame}.
#'     This argument must contain a field (column) name for each zone.}
#'
#'   }
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create simple problem
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
#' plot(s, main = c("equal targets", "varying targets"), axes = FALSE,
#'      box = FALSE)
#' }
#'
#' # create a problem with multiple management zones
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_binary_decisions()
#'
#' # create a problem with targets that specify an equal amount of each feature
#' # to be represented in each zone
#' p4_targets <- matrix(2, nrow = number_of_features(sim_features_zones),
#'                      ncol = number_of_zones(sim_features_zones),
#'                      dimnames = list(feature_names(sim_features_zones),
#'                                      zone_names(sim_features_zones)))
#' print(p4_targets)
#'
#' p4 <- p3 %>% add_absolute_targets(p4_targets)
#'
#' # solve problem
#' \donttest{
#' # solve problem
#' s4 <- solve(p4)
#'
#' # plot solution (pixel values correspond to zone identifiers)
#' plot(category_layer(s4), main = c("equal targets"))
#' }
#' # create a problem with targets that require a varying amount of each
#' # feature to be represented in each zone
#' p5_targets <- matrix(rpois(15, 1),
#'                      nrow = number_of_features(sim_features_zones),
#'                      ncol = number_of_zones(sim_features_zones),
#'                      dimnames = list(feature_names(sim_features_zones),
#'                                      zone_names(sim_features_zones)))
#' print(p5_targets)
#'
#' p5 <- p3 %>% add_absolute_targets(p4_targets)
#' # solve problem
#' \donttest{
#' # solve problem
#' s5 <- solve(p5)
#'
#' # plot solution (pixel values correspond to zone identifiers)
#' plot(category_layer(s5), main = c("varying targets"))
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
  function(x, targets) standardGeneric("add_absolute_targets"))

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage \S4method{add_absolute_targets}{ConservationProblem,numeric}(x, targets)
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "numeric"),
  function(x, targets) {
    assertthat::assert_that(inherits(x, "ConservationProblem"))
    assertthat::assert_that(x$number_of_zones() == 1,
                            msg = paste("argument to x has multiple zones,",
                                        "and so targets must be provided as",
                                        "a matrix"))
    assertthat::assert_that(length(targets) %in% c(1, x$number_of_features()))
    add_absolute_targets(x, matrix(targets, nrow = x$number_of_features(),
                                   ncol = 1))
})

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage \S4method{add_absolute_targets}{ConservationProblem,matrix}(x, targets)
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "matrix"),
  function(x, targets) {
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
#' @usage \S4method{add_absolute_targets}{ConservationProblem,character}(x, targets)
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "character"),
  function(x, targets) {
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
