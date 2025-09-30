#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets based on area units
#'
#' Specify targets expressed as area-based units.
#' For example, this function can be used to express targets as
#' hectares, acres, or \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' To ensure feasibility, area-based targets are clamped based on the
#' total abundance of features.
#'
#' @param targets `numeric` vector denoting the
#' area-based targets for the features.
#' These values must be expressed in the same units as `area_units`.
#' If a single `numeric` value is specified, then all
#' features are assigned targets based on the area-based target.
#'
#' @param area_units `character` vector denoting the unit of measurement
#' for each `targets` value (e.g., `"km^2"`, `"ha"`, and `"acres"`).
#' If a single `character` value is specified, then all
#' features are assigned targets assuming that `targets`
#' are in the same units.
#'
#' @inheritSection spec_jung_targets Data calculations
#'
#' @section Mathematical formulation:
#' This method provides an approach for setting target thresholds based
#' on an area-based threshold.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total spatial extent of a feature (e.g., geographic
#' range size expressed as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}), and
#' \eqn{a} the specified area-based target
#' (expressed as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}},
#' per `targets` and `area_units`).
#' Given this terminology, the target threshold (\eqn{t}) for the feature
#' is calculated as follows.
#' \deqn{
#' t = min(f, a)
#' }{
#' t = min(f, a)
#' }
#'
#' @inherit spec_jung_targets return seealso
#'
#' @family methods
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_complex_pu_raster <- get_sim_complex_pu_raster()
#' sim_complex_features <- get_sim_complex_features()
#'
#' # create base problem
#' p0 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with targets of 50 km^2 for each feature
#' p1 <-
#'   p0 %>%
#'   add_auto_targets(
#'     method = spec_area_targets(targets = 50, area_units = "km^2")
#'   )
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution based on constant targets", axes = FALSE)
#'
#' # targets can also be specified for each feature separately.
#' # to demonstrate this, we will set a target value for each
#' # feature based on a random number between 5000 and 30000 hectares
#' target_values <- runif(terra::nlyr(sim_complex_features), 5000, 30000)
#'
#' # create problem with targets defined separately for each feature
#' p2 <-
#'   p0 %>%
#'   add_auto_targets(
#'     method = spec_area_targets(targets = target_values, area_units = "ha")
#'   )
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution based on varying targets", axes = FALSE)
#' }
#' @export
spec_area_targets <- function(targets, area_units) {
  # assert arguments are valid
  assert_valid_method_arg(targets)
  assert_required(area_units)
  # return new method
  new_method(
    name = "area targets",
    type = "relative",
    fun = calc_area_targets,
    args = list(
      targets = targets,
      area_units = area_units
    )
  )
}

calc_area_targets <- function(x, features, targets, area_units,
                              call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(targets, call = call, .internal = TRUE)
  assert_required(area_units, call = call, .internal = TRUE)
  assert(
    # x
    is_conservation_problem(x),
    has_single_zone(x),
    # features
    is_integer(features),
    all(features >= 1),
    all(features <= x$number_of_features()),
    call = call,
    .internal = TRUE
  )
  assert(
    # targets
    is.numeric(targets),
    all_finite(targets),
    all_positive(targets),
    is_match_of(length(targets), c(1, number_of_features(x))),
    # area_units
    all_area_units(area_units),
    is_match_of(length(area_units), c(1, number_of_features(x))),
    call = call
  )
  assert_can_calculate_area_based_targets(x, features, call = call)

  # if needed, duplicate target values for each feature
  if (identical(length(targets), 1L)) {
    targets <- rep(targets, x$number_of_features())
  }
  if (identical(length(area_units), 1L)) {
    area_units <- rep(area_units, x$number_of_features())
  }

  # extract abundances
  fa <- x$feature_abundances_km2_in_total_units()[features, 1]

  # calculate targets as km^2
  targets <- as_km2(targets[features], area_units)

  # clamp targets to feature abundances
  targets <- pmin(targets, fa)

  # return target
  targets / fa
}
