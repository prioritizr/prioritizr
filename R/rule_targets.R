#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following a set of rules
#'
#' Specify targets following a rule-based procedure based on a set of
#' ecological and ecosystem criteria. This is a customizable version of the
#' approach in Harris and Holness (2023).
#' This function is designed to be used within `add_auto_targets()` and
#' `add_group_targets()`.
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
#' contains information about the feature. In particular, `data`
#' must have a `"feature"` column that contains `character` values with the
#' feature names. Additionally, `data` must also contain columns
#' with `logical` (`TRUE`/`FALSE`) values that indicate if the feature
#' meets a particular criterion for calculating targets.
#' For example,
#' `data` could contain columns that indicate if each feature corresponds
#' to a threatened species, ecosystem service, or culturally important site.
#' Note that if different rules should be applied to species based on their
#' particular threat status (e.g., Critically Endangered species should receive
#' different target then Vulnerable species), then `data` should contain a
#' column for each threat status (separately).
#'
#' @inheritParams rodrigues_targets
#'
#' @section Mathematical formulation:
#' This method provides a flexible approach for setting target thresholds based
#' on a set of criteria.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature (e.g., geographic
#' range).
#' Also, let \eqn{U} denote the set of rules (indexed by \eqn{u}).
#' Next, let \eqn{b} denote the baseline relative target threshold
#' (per `baseline_relative_target`).
#' To parameterize the information about the features that will be used
#' for target calculations (per `data`),
#' let \eqn{d_u}{du} indicate if the target for
#' the feature should be calculated based on each rule \eqn{u \in U}{u in U}
#' (using binary values).
#' Furthermore, let \eqn{a_u} denote the value that will be used to calculate
#' the target based on rule \eqn{u} (per `rules_relative_target`).
#' Finally, let \eqn{c} denote the cap threshold (per `cap_area_target`).
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' \deqn{
#' t = min(f \times max(min(b + \sum_{u \in U} d_u \times a_u, 1), 0), c)
#' }{
#' t = min(f * max(min(b + sum_u^U d_u * a_u, 1), 0), c)
#' }
#'
#' @details
#' This method has been applied to set target thresholds at national
#' scales (e.g., Harris *et al.* 2023).
#' It is based on the rationale that it is appropriate to set the target
#' for a feature based on a linear combination of values.
#' When using this method in a planning exercise, it is important to ensure
#' that the criteria and values used for target setting reflect the stakeholder
#' objectives.
#' Additionally, the cap threshold may need to calibrated based on
#' the features and spatial extent of the planning region.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit jung_targets return seealso
#'
#' @inherit add_manual_targets seealso
#'
#' @family methods
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
#' Carwardine J, Klein CJ, Wilson KA, Pressey RL, Possingham HP (2009) Hitting
#' the target and missing the point: target‐based conservation planning in
#' context. *Conservation Letters*, 2: 4--11.
#'
#' Harris LR, Holness SD (2023) A practical approach to setting heuristic
#' marine biodiversity targets for systematic conservation planning.
#' *Biological Conservation*, 285: 110218.
#'
#' @family methods
#'
#' @examples
#' #TODO
#'
#' @export
rule_targets <- function(baseline_relative_target,
                         rules_relative_target,
                         data,
                         cap_area_target = 1000000,
                         area_units = "km^2") {
  assert_valid_method_arg(baseline_relative_target, "add_rule_targets")
  internal_rule_targets(
    baseline_relative_target = baseline_relative_target,
    rules_relative_target= rules_relative_target,
    data = data,
    cap_area_target = cap_area_target,
    area_units = area_units
  )
}

internal_rule_targets <- function(baseline_relative_target,
                                  rules_relative_target,
                                  data,
                                  cap_area_target,
                                  area_units,
                                  call = fn_caller_env()) {
  # assert arguments are valid
  assert_required(baseline_relative_target, call = call)
  assert_required(rules_relative_target, call = call)
  assert_required(data, call = call)
  assert_required(area_units, call = call)
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
    ),
    call = call
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
  assert_required(x, call = call)
  assert_required(features, call = call)
  assert_required(baseline_relative_target, call = call)
  assert_required(rules_relative_target, call = call)
  assert_required(data, call = call)
  assert_required(cap_area_target, call = call)
  assert_required(area_units, call = call)
  assert(
    # x
    is_conservation_problem(x),
    has_single_zone(x),
    # features,
    is.numeric(features),
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
