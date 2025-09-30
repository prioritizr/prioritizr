#' @include internal.R ConservationProblem-class.R
NULL

#' Specify absolute targets
#'
#' Specify targets expressed as the
#' same values as the underlying feature data (ignoring any specified
#' feature units).
#' For example, setting an absolute target of 10 for a feature means that the
#' solution should ideally select a set of planning units that have a total
#' (summed) value of 10 for the feature.
#' This function is designed to be used with [add_auto_targets()].
#'
#' @param targets `numeric` vector that specifies targets for each
#' of the features. If a single value is specified, then all features
#' are assigned the same target threshold.
#'
#' @param ... not used.
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on a pre-specified
#' value.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{a} the absolute target for a feature
#' (per `targets`).
#' Given this terminology, the target threshold (\eqn{t}) for the feature
#' is calculated as follows.
#' \deqn{t = a}
#'
#' @inherit spec_jung_targets seealso return
#' @seealso
#' To add relative targets directly to a [problem()], see
#' [add_absolute_targets()].
#'
#' @family methods
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create base problem
#' p0 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # this function sets targets based on the total abundance of the features
#' # (i.e., sum of planning unit values for the feature) and does not
#' # consider the spatial area covered by the planning units
#'
#' # create problem with absolute targets of 5 for each feature
#' p1 <-
#'   p0 %>%
#'   add_auto_targets(method = spec_absolute_targets(targets = 5))
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution based on constant targets", axes = FALSE)
#'
#' # targets can also be specified for each feature separately.
#' # to demonstrate this, we will set a target value for each
#' # feature based on a random number between 1 and 5
#' target_values <- runif(terra::nlyr(sim_features), 1, 5)
#'
#' # create problem with targets defined separately for each feature
#' p2 <-
#'   p0 %>%
#'   add_auto_targets(method = spec_absolute_targets(targets = target_values))
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution based on varying targets", axes = FALSE)
#' }
#' @export
spec_absolute_targets <- function(targets, ...) {
  assert_valid_method_arg(targets)
  rlang::check_dots_empty()
  internal_absolute_targets(targets = targets)
}

internal_absolute_targets <- function(targets, call = fn_caller_env()) {
  # assert arguments are valid
  assert_required(targets, call = call)

  # return new method
  new_method(
    name = "Absolute targets",
    type = "absolute",
    fun = calc_absolute_targets,
    args = list(targets = targets),
    call = call
  )
}

calc_absolute_targets <- function(x, features, targets,
                                  call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call)
  assert_required(features, call = call)
  assert_required(targets, call = call)
  assert(
    # x
    is_conservation_problem(x),
    has_single_zone(x),
    # features
    is_integer(features),
    all(features >= 1),
    all(features <= x$number_of_features()),
    # targets
    is.numeric(targets),
    is_match_of(length(targets), c(1, number_of_features(x))),
    all_finite(targets),
    call = call
  )

  # if features have user-defined area units, then throw warning indicating
  # that these targets do not consider the spatial units
  verify(
    inherits(x$data$cost, c("SpatRaster", "Raster")) ||
      all(is.na(x$feature_units())),
    msg = c(
      "!" = "{.arg x} has spatial units defined for the features.",
      "i" = paste(
        "{.fun spec_absolute_targets} does not account",
        "for spatial units."
      ),
      "i" = paste(
        "See {.fun spec_area_targets} to",
        "specify targets that account for spatial units."
      )
    ),
    call = NULL
  )

  # if needed, duplicate target values for each feature
  if (identical(length(targets), 1L)) {
    targets <- rep(targets, x$number_of_features())
  }

  # return targets
  targets[features]
}
