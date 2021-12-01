#' @include internal.R pproto.R ConservationProblem-proto.R zones.R add_absolute_targets.R
NULL

#' Add relative targets
#'
#' Set targets as a proportion (between 0 and 1) of the maximum level of
#' representation of features in the study area. Please note that proportions
#' are scaled according to the features' total abundances in the study area
#' (including any locked out planning units, or planning units with `NA`
#' cost data) using the [feature_abundances()] function.
#'
#' @param x [problem()] (i.e., [`ConservationProblem-class`]) object.
#'
#' @param targets Object that specifies the targets for each feature.
#'   See the Targets format section for more information.
#'
#' @inherit add_absolute_targets details return
#' @inheritSection add_absolute_targets Targets format
#'
#' @seealso
#' See [targets] for an overview of all functions for adding targets.
#'
#' @family targets
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create base problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_binary_decisions() %>%
#'      add_default_solver(verbose = FALSE)
#'
#' # create problem with 10% targets
#' p1 <- p %>% add_relative_targets(0.1)
#'
#' # create problem with varying targets for each feature
#' targets <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' p2 <- p %>% add_relative_targets(targets)
#' \dontrun{
#' # solve problem
#' s <- stack(solve(p1), solve(p2))
#'
#' # plot solution
#' plot(s, main = c("10% targets", "varying targets"), axes = FALSE,
#'      box = FALSE)
#' }
#' # create a problem with multiple management zones
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # create a problem with targets that specify an equal amount of each feature
#' # to be represented in each zone
#' p4_targets <- matrix(0.1, nrow = 5, ncol = 3,
#'                      dimnames = list(feature_names(sim_features_zones),
#'                                      zone_names(sim_features_zones)))
#' print(p4_targets)
#'
#' p4 <- p3 %>% add_relative_targets(p4_targets)
#'
#' # solve problem
#' \dontrun{
#' # solve problem
#' s4 <- solve(p4)
#'
#' # plot solution (pixel values correspond to zone identifiers)
#' plot(category_layer(s4), main = c("equal targets"))
#' }
#' # create a problem with targets that require a varying amount of each
#' # feature to be represented in each zone
#' p5_targets <- matrix(runif(15, 0.01, 0.2), nrow = 5, ncol = 3,
#'                      dimnames = list(feature_names(sim_features_zones),
#'                                      zone_names(sim_features_zones)))
#' print(p5_targets)
#'
#' p5 <- p3 %>% add_relative_targets(p4_targets)
#' # solve problem
#' \dontrun{
#' # solve problem
#' s5 <- solve(p5)
#'
#' # plot solution (pixel values correspond to zone identifiers)
#' plot(category_layer(s5), main = c("varying targets"))
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
  function(x, targets) standardGeneric("add_relative_targets"))

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,numeric}(x, targets)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "numeric"),
  function(x, targets) {
    assertthat::assert_that(inherits(x, "ConservationProblem"))
    assertthat::assert_that(number_of_zones(x) == 1,
                            msg = paste("argument to x has multiple zones,",
                                        "and so targets must be provided as",
                                        "a matrix"))
    assertthat::assert_that(length(targets) %in% c(1, number_of_features(x)))
    add_relative_targets(x, matrix(targets, nrow = x$number_of_features(),
                                   ncol = 1))
})

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,matrix}(x, targets)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "matrix"),
  function(x, targets) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      inherits(targets, "matrix"), is.numeric(targets),
      isTRUE(all(is.finite(targets))),
      isTRUE(all(targets >= 0)),
      isTRUE(all(targets <= 1)),
      isTRUE(length(targets) > 0),
      nrow(targets) == number_of_features(x),
      ncol(targets) == number_of_zones(x))
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
#' @usage \S4method{add_relative_targets}{ConservationProblem,character}(x, targets)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "character"),
  function(x, targets) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      is.character(targets),
      inherits(x$data$features, "data.frame"),
      !anyNA(targets),
      all(assertthat::has_name(x$data$features, targets)),
      length(targets) == number_of_zones(x),
      all(vapply(x$data$features[, targets, drop = FALSE], is.numeric,
                logical(1))))
    # add targets to problem
    add_relative_targets(x, as.matrix(x$data$features[, targets, drop = FALSE]))
})
