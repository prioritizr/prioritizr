#' @include internal.R ConservationProblem-class.R zones.R
NULL

#' Add absolute targets
#'
#' Set targets expressed as the actual value of features in the study area
#' that need to be represented in the prioritization. For instance,
#' setting a target of 10 requires that the solution secure a set of
#' planning units for which their summed feature values are equal to or greater
#' than 10.
#'
#' @param x [problem()] object.
#'
#' @param targets object that specifies the targets for each feature.
#'   See the Targets format section for more information.
#'
#' @details
#' Targets are used to specify the minimum amount or proportion of a
#' feature's distribution that needs to be protected. Most conservation
#' planning problems require targets with the exception of the maximum cover
#' (see [add_max_cover_objective()]) and maximum utility
#' (see [add_max_utility_objective()]) problems. Attempting to solve
#' problems with objectives that require targets without specifying targets
#' will throw an error.
#'
#' For problems associated with multiple management zones,
#' [add_absolute_targets()] can
#' be used to set targets that each pertain to a single feature and a single
#' zone. To set targets that can be met through allocating different
#' planning units to multiple zones, see the [add_manual_targets()]
#' function. An example of a target that could be met through allocations
#' to multiple zones might be where each management zone is expected to
#' result in a different amount of a feature and the target requires that
#' the total amount of the feature in all zones must exceed a certain
#' threshold. In other words, the target does not require that any single
#' zone secure a specific amount of the feature, but the total amount held
#' in all zones must secure a specific amount. Thus the target could,
#' potentially, be met through allocating all planning units to any specific
#' management zone, or through allocating the planning units to different
#' combinations of management zones.
#'
#' @section Targets format:
#' The `targets` for a problem can be specified using the following formats.
#'
#' \describe{
#'
#' \item{`targets` as a `numeric` vector}{containing target values for each
#'   feature.
#'   Additionally, for convenience, this format can be a single
#'   value to assign the same target to each feature. Note that this format
#'   cannot be used to specify targets for problems with multiple zones.}
#'
#' \item{`targets` as a `matrix` object}{containing a target for each feature
#'   in each zone.
#'   Here, each row corresponds to a different feature in argument to
#'   `x`, each column corresponds to a different zone in argument to
#'   `x`, and each cell contains the target value for a given feature
#'   that the solution needs to secure in a given zone.}
#'
#' \item{`targets` as a `character` vector}{containing the column name(s) in the
#'   feature data associated with the argument to `x` that
#'   contain targets. This format can only be used when the
#'   feature data associated with `x` is a [sf::st_sf()] or `data.frame`.
#'   For problems that contain a single zone, the argument to `targets` must
#'   contain a single column name. Otherwise, for problems that
#'   contain multiple zones, the argument to `targets` must
#'   contain a column name for each zone.}
#'
#' }
#'
#' @inherit add_manual_targets return
#'
#' @seealso
#' See [targets] for an overview of all functions for adding targets.
#'
#' @family targets
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem with no targets
#' p0 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with targets to secure 3 amounts for each feature
#' p1 <- p0 %>% add_absolute_targets(3)
#'
#' # create problem with varying targets for each feature
#' targets <- c(1, 2, 3, 2, 1)
#' p2 <- p0 %>% add_absolute_targets(targets)
#'
#' # solve problem
#' s1 <- c(solve(p1), solve(p2))
#' names(s1) <- c("equal targets", "varying targets")
#'
#' # plot solution
#' plot(s1, axes = FALSE)
#'
#' # create a problem with multiple management zones
#' p3 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create a problem with targets that specify an equal amount of each feature
#' # to be represented in each zone
#' p4_targets <- matrix(
#'   2,
#'   nrow = number_of_features(sim_zones_features),
#'   ncol = number_of_zones(sim_zones_features),
#'   dimnames = list(
#'     feature_names(sim_zones_features), zone_names(sim_zones_features)
#'   )
#' )
#' print(p4_targets)
#'
#' p4 <- p3 %>% add_absolute_targets(p4_targets)
#'
#' # solve problem
#' s4 <- solve(p4)
#'
#' # plot solution (cell values correspond to zone identifiers)
#' plot(category_layer(s4), main = "equal targets", axes = FALSE)
#'
#' # create a problem with targets that require a varying amount of each
#' # feature to be represented in each zone
#' p5_targets <- matrix(
#'   rpois(15, 1),
#'   nrow = number_of_features(sim_zones_features),
#'   ncol = number_of_zones(sim_zones_features),
#'   dimnames = list(
#'     feature_names(sim_zones_features),
#'     zone_names(sim_zones_features)
#'   )
#' )
#' print(p5_targets)
#'
#' p5 <- p3 %>% add_absolute_targets(p5_targets)
#'
#' # solve problem
#' s5 <- solve(p5)
#'
#' # plot solution (cell values correspond to zone identifiers)
#' plot(category_layer(s5), main = "varying targets", axes = FALSE)
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
  function(x, targets) {
    assert_required(x)
    assert_required(targets)
    assert(
      is_conservation_problem(x),
      is_inherits(targets, c("character", "numeric", "matrix"))
    )
    standardGeneric("add_absolute_targets")
  }
)

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage \S4method{add_absolute_targets}{ConservationProblem,numeric}(x, targets)
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "numeric"),
  function(x, targets) {
    assert(is_conservation_problem(x))
    assert(
      x$number_of_zones() == 1,
      msg = paste(
        "{.arg targets} must be a character vector or matrix, because",
        "{.arg x} has multiple zones"
      )
    )
    assert(
      length(targets) %in% c(1, x$number_of_features()),
      msg  = paste(
        "{.arg targets} must be a single numeric value,",
        "or a numeric vector containing ", x$number_of_features(),
        "values (one for each feature)."
      )
    )
    add_absolute_targets(
      x, matrix(targets, nrow = x$number_of_features(), ncol = 1)
    )
  }
)

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage \S4method{add_absolute_targets}{ConservationProblem,matrix}(x, targets)
methods::setMethod(
  "add_absolute_targets",
  methods::signature("ConservationProblem", "matrix"),
  function(x, targets) {
    # assert that arguments are valid
    assert(
      is_conservation_problem(x),
      is.matrix(targets),
      is.numeric(targets),
      all_finite(targets),
      length(targets) > 0,
      nrow(targets) == x$number_of_features(),
      ncol(targets) == x$number_of_zones()
    )
    verify(all_positive(targets))
    inf_idx <- which(
      targets > x$feature_positive_abundances_in_planning_units()
    )
    verify(
      length(inf_idx) == 0,
      msg = c(
        paste(
          "{.arg targets} contains infeasible values that cannot be met even",
          "if all planning units selected."
        ),
        "i" = paste(
          "Infeasible values found at {.val {length(inf_idx)}} locations:",
          "{.val {inf_idx}}."
        )
      )
    )
    # create targets as data.frame
    if (x$number_of_zones() > 1) {
      target_data <- expand.grid(
        feature = x$feature_names(),
        zone = x$zone_names(),
        type = "absolute"
      )
    } else {
      target_data <- data.frame(feature = x$feature_names(), type = "absolute")
    }
    target_data$target <- as.numeric(targets)
    # add targets to problem
    add_manual_targets(x, target_data)
  }
)

#' @name add_absolute_targets
#' @rdname add_absolute_targets
#' @usage \S4method{add_absolute_targets}{ConservationProblem,character}(x, targets)
methods::setMethod(
  "add_absolute_targets",
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
      msg = paste(
        "{.arg targets} cannot be a character vector, because the feature data",
        "for {.arg x} are not a data frame."
      )
    )
    assert(
      all(assertthat::has_name(x$data$features, targets)),
      msg = paste0(
        "{.arg targets} must contain character values that are",
        "column names of the feature data for {.arg x}."
      )
    )
    assert(
      all_columns_inherit(
        x$data$features[, targets, drop = FALSE],
        "numeric"
      ),
      msg = paste(
        "{.arg targets} must contain character values that",
        "refer to numeric columns of the feature data for {.arg x}."
      )
    )
    inf_idx <- which(
      as.matrix(x$data$features[, targets, drop = FALSE]) >
        x$feature_positive_abundances_in_planning_units()
    )
    verify(
      length(inf_idx) == 0,
      msg = c(
        paste(
          "{.arg targets} contains infeasible values that cannot be met even",
          "if all planning units selected."
        ),
        "i" = paste(
          "Infeasible values found at {.val {length(inf_idx)}} locations:",
          "{.val {inf_idx}}."
        )
      )
    )
    # add targets to problem
    add_absolute_targets(
      x, as.matrix(x$data$features[, targets, drop = FALSE]
      )
    )
  }
)
