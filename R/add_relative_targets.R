#' @include internal.R ConservationProblem-class.R zones.R add_absolute_targets.R
NULL

#' Add relative targets
#'
#' Add targets to a conservation planning problem expressed as a proportion
#' (between 0 and 1) of the maximum level of representation of each feature in
#' the study area.
#' Please note that proportions
#' are scaled according to the features' total abundances in the study area
#' (including any locked out planning units, or planning units with `NA`
#' cost values) using the [feature_abundances()] function.
#'
#' @param x [problem()] object.
#'
#' @param targets Object that specifies the targets for each feature.
#'   See the Targets format section for more information.
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_absolute_targets Targets format
#'
#' @inherit add_absolute_targets details return seealso references
#'
#' @family targets
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create base problem
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with 10% targets
#' p1 <- p %>% add_relative_targets(0.1)
#'
#' # create problem with varying targets for each feature
#' targets <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' p2 <- p %>% add_relative_targets(targets)
#'
#' # solve problem
#' s3 <- c(solve(p1), solve(p2))
#' names(s3) <- c("10% targets", "varying targets")
#'
#' # plot solution
#' plot(s3, main = , axes = FALSE)
#'
#' # create a problem with multiple management zones
#' p4 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create a problem with targets that specify an equal amount of each feature
#' # to be represented in each zone
#' p4_targets <- matrix(
#'   0.1, nrow = 5, ncol = 3, dimnames = list(
#'     feature_names(sim_zones_features), zone_names(sim_zones_features)
#'   )
#' )
#' print(p4_targets)
#'
#' p5 <- p4 %>% add_relative_targets(p4_targets)
#'
#' # solve problem
#' s5 <- solve(p5)
#'
#' # plot solution (cell values correspond to zone identifiers)
#' plot(category_layer(s5), main = "equal targets")
#'
#' # create a problem with targets that require a varying amount of each
#' # feature to be represented in each zone
#' p6_targets <- matrix(
#'   runif(15, 0.01, 0.2), nrow = 5, ncol = 3, dimnames = list(
#'     feature_names(sim_zones_features), zone_names(sim_zones_features)
#'   )
#' )
#' print(p6_targets)
#'
#' p6 <- p4 %>% add_relative_targets(p6_targets)
#'
#' # solve problem
#' s6 <- solve(p6)
#'
#' # plot solution (cell values correspond to zone identifiers)
#' plot(category_layer(s6), main = "varying targets")
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
  function(x, targets) {
    assert_required(x)
    assert_required(targets)
    assert(
      is_conservation_problem(x),
      is_inherits(targets, c("numeric", "matrix", "character"))
    )
    standardGeneric("add_relative_targets")
  }
)

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,numeric}(x, targets)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "numeric"),
  function(x, targets) {
    assert(is_conservation_problem(x))
    assert(
      number_of_zones(x) == 1,
      msg = c(
        "{.arg targets} must be a matrix.",
        "i" = "This is because {.arg x} has multiple zones."
      )
    )
    assert(
      is_match_of(length(targets), c(1, number_of_features(x)))
    )
    add_relative_targets(
      x, matrix(targets, nrow = x$number_of_features(), ncol = 1)
    )
  }
)

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,matrix}(x, targets)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "matrix"),
  function(x, targets) {
    # assert that arguments are valid
    assert(
      is_conservation_problem(x),
      is.matrix(targets),
      is.numeric(targets),
      all_finite(targets),
      all_proportion(targets),
      nrow(targets) == number_of_features(x),
      ncol(targets) == number_of_zones(x)
    )
    # create targets as data.frame
    if (x$number_of_zones() > 1) {
      target_data <- expand.grid(
        feature = x$feature_names(),
        zone = x$zone_names(),
        type = "relative"
      )
    } else {
      target_data <- data.frame(feature = x$feature_names(), type = "relative")
    }
    target_data$target <- as.numeric(targets)
    # add targets to problem
    add_manual_targets(x, target_data)
  }
)

#' @name add_relative_targets
#' @rdname add_relative_targets
#' @usage \S4method{add_relative_targets}{ConservationProblem,character}(x, targets)
methods::setMethod(
  "add_relative_targets",
  methods::signature("ConservationProblem", "character"),
  function(x, targets) {
    # assert that arguments are valid
    assert(
      is_conservation_problem(x),
      is.character(targets),
      assertthat::noNA(targets),
      length(targets) == number_of_zones(x)
    )
    assert(
      is.data.frame(x$data$features),
      msg = c(
        "{.arg targets} must not be a character vector.",
        "i" = paste(
          "This is because the feature data for {.arg x}",
          "are not a data frame."
        )
      )
    )
    assert(
      all(assertthat::has_name(x$data$features, targets)),
      msg = paste(
        "{.arg targets} must contain character values that refer",
        "to column names of the feature data for {.arg x}."
      )
    )
    assert(
      all_columns_inherit(
        x$data$features[, targets, drop = FALSE],
        c("numeric", "integer")
      ),
      msg = paste(
        "{.arg targets} must refer to {.cls numeric} columns of the",
        "feature data for {.arg x}."
      )
    )
    assert(
      all_proportion(x$data$features[, targets, drop = FALSE]),
      msg = paste(
        "{.arg targets} must refer to columns of the feature data",
        "for {.arg x} that contain values between {.val {0}} and {.val {1}}."
      )
    )
    # add targets to problem
    add_relative_targets(x, as.matrix(x$data$features[, targets, drop = FALSE]))
  }
)
