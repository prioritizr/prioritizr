#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following a set of rules
#'
#' Specify targets based on a set of rules for ecological and ecosystem
#' criteria. This is a customizable version of the
#' approach in Harris and Holness (2023).
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed to help set targets for a broad range of features
#' (e.g., species, ecosystems, ecosystem services, ecological processes)
#' at local and national scales.
#' Note that this function is designed to be used with [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param baseline_relative_target `numeric` value indicating the baseline
#' target values as a proportion (ranging between 0 and 1). Depending on
#' context of the prioritization exercise, a value of 0.3 (i.e., 30%)
#' may be appropriate (Harris *et al.* 2023).
#"
#' @param rules_relative_target named `numeric` vector with name-value key pairs
#' denoting the values that should be added together when calculating the
#' relative target for a given feature. Note that values must range
#' between -1 and 1, and the names must correspond to column names of `data`.
#'
#' @param data `data.frame` indicating if each feature meets the criteria
#' for applying each rule (or not) during the target calculations.
#' Here, each row must correspond to a different feature, and each column
#' contains information about the feature (per `feature_names(x)`).
#' In particular, `data`
#' must have a `"feature"` column that contains `character` values with the
#' feature names. Additionally, `data` must also contain columns
#' with `logical` (`TRUE`/`FALSE`) values that indicate if the feature
#' meets a particular criterion for calculating targets.
#' For example,
#' `data` could contain columns that indicate if each feature corresponds
#' to a threatened species, ecosystem service, or culturally important site.
#' Note that if different rules should be applied to species based on their
#' particular threat status (e.g., Critically Endangered and Vulnerable species
#' should receive different target thresholds), then `data` should contain a
#' column for each threat status (separately).
#'
#' @inheritParams spec_rodrigues_targets
#'
#' @details
#' This method has been applied to set target thresholds at national
#' scales (e.g., Harris *et al.* 2023).
#' It is based on the rationale that it is appropriate to set the target
#' for a feature based on a linear combination of values.
#' When using this method in a planning exercise, it is important to ensure
#' that the criteria and values used for target setting reflect the stakeholder
#' objectives.
#' Additionally, the baseline relative target (per `baseline_relative_target`)
#' and cap threshold (per `cap_area_target` and `area_units`) may need to
#' set based on the features and spatial extent of the planning region.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection spec_jung_targets Data calculations
#'
#' @section Mathematical formulation:
#' This method provides a flexible approach for setting target thresholds based
#' on a set of rules.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature (e.g., geographic
#' range), \eqn{b} denote the baseline relative target threshold
#' (per `baseline_relative_target`), and let \eqn{c} denote the cap threshold
#' (per `cap_area_target` and `area_units`).
#' To describe the rules, let \eqn{U} denote the set of rules
#' (indexed by \eqn{u}), \eqn{d_u}{du} indicate if the target for
#' the feature should be calculated based on each rule \eqn{u \in U}{u in U}
#' (using binary values, per `data`), and \eqn{a_u} denote the value
#' should be added to the target given each rule \eqn{u \in U}{u in U}
#' (per `rules_relative_target`).
#' Given this terminology, the target threshold (\eqn{t}) for the feature
#' is calculated as follows.
#' \deqn{
#' t = min(f \times max(min(b + \sum_{u \in U} d_u \times a_u, 1), 0), c)
#' }{
#' t = min(f * max(min(b + sum_u^U d_u * a_u, 1), 0), c)
#' }
#'
#' @inherit spec_jung_targets return seealso
#'
#' @references
#' Butchart SHM, Clarke M, Smith RJ, Sykes RE, Scharlemann JPW, Harfoot M,
#' Buchanan GM, Angulo A, Balmford A, Bertzky B, Brooks TM, Carpenter KE,
#' Comeros‐Raynal MT, Cornell J, Ficetola GF, Fishpool LDC, Fuller RA,
#' Geldmann J, Harwell H, Hilton‐Taylor C, Hoffmann M, Joolia A, Joppa L,
#' Kingston N, May I, Milam A, Polidoro B, Ralph G, Richman N, Rondinini C,
#' Segan DB, Skolnik B, Spalding MD, Stuart SN, Symes A, Taylor J, Visconti P,
#' Watson JEM, Wood L, Burgess ND (2015) Shortfalls and solutions for meeting
#' national and global conservation area targets. *Conservation Letters*,
#' 8: 329--337.
#'
#' Harris LR, Holness SD (2023) A practical approach to setting heuristic
#' marine biodiversity targets for systematic conservation planning.
#' *Biological Conservation*, 285: 110218.
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
#' # calculate total distribution size for features in km^2
#' feature_size <- as.numeric(units::set_units(
#'   units::set_units(
#'     terra::global(sim_complex_features, "sum", na.rm = TRUE)[[1]] *
#'       prod(terra::res(sim_complex_features)),
#'     "m^2"
#'   ),
#'   "km^2"
#' ))
#'
#' # simulate data that provide additional information for each feature
#' rule_data <- tibble::tibble(feature = names(sim_complex_features))
#'
#' # add a column indicating if each feature has a small distribution,
#  # based on a threshold of 1000 km^2
#' rule_data$small_distribution <- feature_size <= 1000
#'
#' # add a column indicating if each feature has a large distribution,
#  # based on a threshold of 5000 km^2
#' rule_data$large_distribution <- feature_size >= 5000
#'
#' # add a column indicating if each feature has low quality data
#' # associated with it, based on random simulated values
#' rule_data$low_quality <- sample(
#'   c(TRUE, FALSE), terra::nlyr(sim_complex_features),
#'   replace = TRUE, prob = c(0.2, 0.8)
#' )
#'
#' # next, we will add simulate dat for columns indicating the threat status of
#' # the features. since all columns must contain logical (TRUE/FALSE) values,
#' # we will add a column for each threat status separately. note that these
#' # values will be simulated such that a feature will only have a value of
#' # TRUE for, at most, a single threat status
#'
#' # add a column indicating if each feature has a Vulnerable threat status
#' rule_data$vulnerable <- sample(
#'   c(TRUE, FALSE), terra::nlyr(sim_complex_features),
#'   replace = TRUE, prob = c(0.3, 0.7)
#' )
#'
#' # add a column indicating if each feature has an Endangered threat status,
#' # based on random simulated values
#' rule_data$endangered <- sample(
#'   c(TRUE, FALSE), terra::nlyr(sim_complex_features),
#'   replace = TRUE, prob = c(0.3, 0.7)
#' ) & !rule_data$vulnerable
#'
#' # add a column indicating if each feature has a Critically Endangered threat
#' # status, based on random simulated values
#' rule_data$critically_endangered <- sample(
#'   c(TRUE, FALSE), terra::nlyr(sim_complex_features),
#'   replace = TRUE, prob = c(0.3, 0.7)
#' ) & !rule_data$endangered & !rule_data$vulnerable
#'
#' # preview rule data
#' print(rule_data)
#'
#' # create problem with rule based targets, wherein targets are calculated
#' # with a baseline of 30%, features with a small distribution are assigned
#' # targets of an additional 10%, features with a large distribution are
#' # assigned targets reduced by 10%, features with low quality data are
#' # assigned targets reduced by 10%, features with a Vulnerable threat status
#' # are assigned targets of an additional 5%, features with an Endangered
#' # threat status are assigned targets of an additional 10%, and features with
#' # a Critically Endangered threat status are assigned targets of an
#' # additional 20%
#' p1 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_auto_targets(
#'     method = spec_rule_targets(
#'       baseline_relative_target = 0.3,
#'       rules_relative_target = c(
#'         "small_distribution" = 0.1,
#'         "large_distribution" = -0.1,
#'         "low_quality" = -0.1,
#'         "vulnerable" = 0.05,
#'         "endangered" = 0.1,
#'         "critically_endangered" = 0.2
#'       ),
#'       data = rule_data
#'     )
#'   ) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#' }
#' @export
spec_rule_targets <- function(baseline_relative_target,
                              rules_relative_target,
                              data,
                              cap_area_target = 1000000,
                              area_units = "km^2") {
  # assert arguments are valid
  assert_valid_method_arg(baseline_relative_target)
  assert_required(baseline_relative_target)
  assert_required(rules_relative_target)
  assert_required(data)
  assert_required(area_units)
  # return new method
  new_method(
    name = "rule targets",
    type = "relative",
    fun = calc_rule_targets,
    args = list(
      baseline_relative_target = baseline_relative_target,
      rules_relative_target = rules_relative_target,
      data = data,
      cap_area_target = cap_area_target,
      area_units = area_units
    )
  )
}

calc_rule_targets <- function(x,
                              features,
                              baseline_relative_target,
                              rules_relative_target,
                              data,
                              cap_area_target,
                              area_units,
                              call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(baseline_relative_target, call = call, .internal = TRUE)
  assert_required(rules_relative_target, call = call, .internal = TRUE)
  assert_required(data, call = call, .internal = TRUE)
  assert_required(cap_area_target, call = call, .internal = TRUE)
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
    # baseline_relative_target
    assertthat::is.number(baseline_relative_target),
    all_finite(baseline_relative_target),
    baseline_relative_target >= 0,
    baseline_relative_target <= 1,
    # rules_relative_target
    is.numeric(rules_relative_target),
    length(rules_relative_target) > 0,
    all_finite(rules_relative_target),
    length(rules_relative_target) >= 1,
    # data
    is.data.frame(data),
    nrow(data) >= 1,
    assertthat::has_name(data, "feature"),
    is_inherits(data$feature, c("character", "factor")),
    no_duplicates(data$feature),
    assertthat::noNA(data$feature),
    # cap_area_target
    assertthat::is.scalar(cap_area_target),
    # area_units
    is_area_units(area_units),
    call = call
  )
  assert(
    all(rules_relative_target >= -1),
    all(rules_relative_target <= 1),
    msg = paste(
      "{.arg rules_relative_target} must contain values",
      "between {.val {-1}} and {.val {1}}."
    ),
    call = call
  )
  assert_can_calculate_area_based_targets(x, features, call = call)
  if (!is.null(cap_area_target)) {
    assert(
      assertthat::is.number(cap_area_target),
      all_finite(cap_area_target),
      cap_area_target >= 0,
      call = call
    )
  } else {
    ## this is needed to account for different NA classes
    cap_area_target <- NA_real_ # nocov
  }

  # convert feature to character
  data$feature <- as.character(data$feature)

  # convert cap_area_target to km^2
  cap_area_target <- as_km2(cap_area_target, area_units)

  # additional checks
  miss_ft <- setdiff(x$feature_names(), data$feature)
  extra_ft <- setdiff(data$feature, x$feature_names())
  assert(
    all_match_of(data$feature, x$feature_names()),
    msg = c(
      "!" =
        "{.arg data$feature} must contain names of features in {.arg x}.",
      "x" =
        "The following values are not feature names: {repr.character(extra_ft)}"
    ),
    call = call
  )
  assert(
    all_match_of(x$feature_names(), data$feature),
    msg = c(
      "!" =
        "{.arg data$feature} is missing some of the features in {.arg x}.",
      "i" = "The following features are missing: {repr.character(miss_ft)}"
    ),
    call = call
  )
  assert(
    !is.null(names(rules_relative_target)),
    all(nzchar(names(rules_relative_target))),
    msg = "{.arg rules_relative_target} must be named vector.",
    call = call
  )
  assert(
    all(names(rules_relative_target) %in% names(data)),
    msg = paste(
      "{.arg rules_relative_target} must have names that",
      "correspond to columns of {.arg data}."
    ),
    call = call
  )
  assert(
    all_columns_inherit(
      data[, names(rules_relative_target), drop = FALSE],
      "logical"
    ),
    msg = paste(
      "{.arg rules_relative_target} must have names that",
      "correspond to columns of {.arg data} with {.cls logical} values."
    ),
    call = call
  )
  assert(
    all(vapply(data[, names(rules_relative_target)], all_finite, logical(1))),
    msg = paste(
      "{.arg rules_relative_target} must have names that",
      "correspond to columns of {.arg data} that do not have missing",
      "{.val {NA}} values."
    ),
    call = call
  )
  assert(
    all(vapply(data[, names(rules_relative_target)], all_finite, logical(1))),
    msg = paste(
      "{.arg rules_relative_target} must have names that",
      "correspond to columns of {.arg data} that do not have missing",
      "{.val {NA}} values."
    ),
    call = call
  )
  if (assertthat::noNA(cap_area_target)) {
    assert(
      assertthat::is.number(cap_area_target),
      cap_area_target >= 0,
      call = call
    )
  }

  # re-order data
  data <- data[match(data$feature, x$feature_names()), , drop = FALSE]

  # extract abundances
  fa <- x$feature_abundances_km2_in_total_units()[features, 1]

  # calculate targets for relevant features
  targets <-
    baseline_relative_target +
    rowSums(
      as.matrix(data[features, names(rules_relative_target)]) *
      matrix(
        unname(rules_relative_target),
        byrow = TRUE, nrow = nrow(data), ncol = length(rules_relative_target)
      )
    )

  ## clamp values to between 0 and 1
  targets <- pmax(targets, 0)
  targets <- pmin(targets, 1)

  # convert the targets to absolute values
  targets <- targets * fa

  # apply target cap
  if (all_finite(cap_area_target)) {
    targets <- pmin(targets, cap_area_target)
  }

  # clamp to feature
  targets <- pmin(targets, fa)

  # return targets
  targets / fa
}
