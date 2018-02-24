#' @include internal.R pproto.R ConservationProblem-proto.R zones.R add_absolute_targets.R
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
#' @aliases add_relative_targets-method add_relative_targets,ConservationProblem,numeric-method add_relative_targets,ConservationProblem,matrix-method add_relative_targets,ConservationProblem,character-method
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
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            x$number_of_zones() == 1,
                            length(targets) %in% c(1, x$number_of_features()))
    add_relative_targets(x, matrix(targets, nrow = x$number_of_features(),
                                   ncol = 1))
})

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,matrix}(x, targets, ...)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "matrix"),
  function(x, targets, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      inherits(targets, "matrix"), is.numeric(targets),
      isTRUE(all(is.finite(targets))),
      isTRUE(all(targets >= 0)),
      isTRUE(all(targets <= 1)),
      isTRUE(length(targets) > 0),
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
    target_data$target <- as.numeric(targets)
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
      is.character(targets),
      inherits(x$data$features, "data.frame"),
      !anyNA(targets),
      all(assertthat::has_name(x$data$features, targets)),
      length(targets) == x$number_of_zones(),
      all(vapply(x$data$features[, targets, drop = FALSE], is.numeric,
                logical(1))))
    # add targets to problem
    add_relative_targets(x, as.matrix(x$data$features[, targets, drop = FALSE]))
})
