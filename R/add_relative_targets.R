#' @include internal.R pproto.R ConservationProblem-proto.R zones.R
NULL

#' Add Relative Targets
#'
#' Set targets as a proportion (between 0 and 1) of the maximum level of
#' representation of features in the study area. Note
#' that the \code{\link{add_manual_targets}} function must be used to
#' specify targets that can be met through allocating planning units
#' to multiple zones. In other words, this function can be used to specify
#' targets that each pertain to a single feature and a single zone.
#'
#' @inheritParams add_absolute_targets
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
#' # create problem with 10 % targets
#' p1 <- p %>% add_relative_targets(0.1)
#'
#' # create problem with varying targets for each feature
#' targets <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' p2 <- p %>% add_relative_targets(targets)
#' \donttest{
#' # solve problem
#' s <- stack(solve(p1), solve(p2))
#'
#' # plot solution
#' plot(s, main = c("10 % targets", "varying targets"), axes = FALSE,
#'      box = FALSE)
#' }
#'
#' @aliases add_relative_targets-method add_relative_targets,ConservationProblem,numeric-method add_relative_targets,ConservationProblem,matrix-method add_relative_targets,ConservationProblem,character-method add_relative_targets,ConservationProblem,ZonesCharacter-method
#'
#' @name add_relative_targets
#'
#' @docType methods
NULL

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @exportMethod add_relative_targets
#' @export
methods::setGeneric(
  "add_relative_targets",
  signature = methods::signature("x", "targets"),
  function(x, targets, ...) standardGeneric("add_relative_targets"))

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,numeric}(x, targets, ...)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "numeric"),
  function(x, targets, ...) {
    add_relative_targets(x, matrix(targets, ncol = 1))
})

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,matrix}(x, targets, ...)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "numeric"),
  function(x, targets, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      inherits(targets, "matrix"), is.numeric(targets),
      isTRUE(all(is.finite(targets))),
      isTRUE(all(targets >= 0.0)),
      isTRUE(all(targets <= 1.0)),
      isTRUE(length(targets) > 0))
    # assert that targets are compatible with problem
    if (nrow(targets) == 1 && ncol(targets) == 1) {
      targets <- matrix(targets, nrow = x$number_of_features(),
                        ncol = x$number_of_zones())
    }
    assertthat::assert_that(
      nrow(targets) == x$number_of_features(),
      ncol(targets) == x$number_of_zones())
    # create targets as data.frame
    if (x$number_of_zones() > 1) {
      target_data <- expand.grid(feature = x$feature_names(),
                                 zone = x$zone_names(),
                                 type = "relative")
    } else {
      target_data <- expand.grid(feature = x$feature_names(),
                                 type = "relative")
    }
    target_data$target <- c(targets)
    # add targets to problem
    add_manual_targets(x, target_data)
})

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,character}(x, targets, ...)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "character"),
  function(x, targets, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(x$data$features, c("data.frame", "Spatial")),
      assertthat::has_name(x$data$features, targets),
      length(targets) == x$data$features())
    # add targets to problem
    add_relative_targets(x, zones(targets))
})

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,ZonesCharacter}(x, targets, ...)
methods::setMethod(
  "add_relative_targets",
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
    add_relative_targets(x, targets)
})
