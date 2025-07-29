#' @include internal.R ConservationProblem-class.R
NULL

#' Specify targets based on the IUCN Red List of Ecosystems
#'
#' Specify targets based on criteria from the
#' International Union for the Conservation of Nature (IUCN) Red List of
#' Ecosystems (IUCN 2024).
#' This function is designed to be used within `add_auto_targets()` and
#' `add_group_targets()`.
#'
#' @inheritParams add_rl_species_targets
#'
#' @param criterion_a `character` value indicating which subcriterion
#' should be considered based on geographic distribution reduction.
#' Available options include subcriterion based on
#' `"A1"` (reductions over the past 50 years),
#' `"A2a"` (reductions over the next 50 years),
#' `"A2b"` (reductions over any 50 year period), or
#' `"A3"` (reductions during historical time periods).
#' See Mathematical formulation below for details.
#'
#' @param criterion_b `character` value indicating which subcriterion
#' should be considered based on geographic distribution size.
#' Available options include subcriterion based on
#' `"B1"` (extent of occupancy), and
#' `"B2"` (area of occupancy).
#' See Mathematical formulation below for details.
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on assessment
#' criteria from the International Union for the Conservation of Nature (IUCN)
#' Red List of Ecosystems (IUCN 2024).
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature (e.g., geographic
#' range size), \eqn{a} the threshold value from Criterion A based on the
#' specified threat status (per `status`, see below for details),
#' \eqn{b} the threshold value from Criterion B
#' based on the specified threat status (per `status`, see below for details),
#' \eqn{p} the percentage uplift as a proportion (per `prop_uplift`),
#' and \eqn{c} the target cap (per `cap_threshold`).
#' Additionally, let
#' \eqn{\box()}{box()} denote either \eqn{max()} or \eqn{min()} (per `method`).
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' \deqn{
#' t = min(min(\box(b, f \times ((1 + p) \times (1 - a))), c), f)
#' }{
#' t = min(min(box(b, f * ((1 + p) * (1 - a))) , c), f)
#' }
#'
#' Additionally, \eqn{a} is equal to one of the following values depending on
#' `status` and `criterion_a`.
#' Note that if `criterion_a` has a value of `"A2a"` or `"A2b", then
#' \eqn{a} is assigned the same value as if had a value of `"A1"`.
#' * If `status = "CR"` and `criterion_a = "A1"`, then \eqn{a =} 80%.
#' * If `status = "EN"` and `criterion_a = "A1"`, then \eqn{a =} 50%.
#' * If `status = "VU"` and `criterion_a = "A1"`, then \eqn{a =} 30%.
#' * If `status = "CR"` and `criterion_a = "A3"`, then \eqn{a =} 90%.
#' * If `status = "EN"` and `criterion_a = "A3"`, then \eqn{a =} 70%.
#' * If `status = "VU"` and `criterion_a = "A3"`, then \eqn{a =} 30%.
#'
#' Additionally, \eqn{b} is equal to one of the following values
#' depending on `status` and `criterion_b`.
#' * If `status = "CR"` and `criterion_a = "B1"`, then \eqn{b =} 2,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "EN"` and `criterion_a = "B1"`, then \eqn{b =} 20,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "VU"` and `criterion_a = "B1"`, then \eqn{b =} 50,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "CR"` and `criterion_a = "B2"`, then \eqn{b =} 200 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "EN"` and `criterion_a = "B2"`, then \eqn{b =} 2,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "VU"` and `criterion_a = "B2"`, then \eqn{b =} 5,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#'
#' @details
#' Targets based on criteria from the IUCN Red List of Ecosystems
#' may be appropriate for global and national scale prioritizations.
#' Despite this, prioritizations based on these criteria may fail to identify
#' meaningful priorities for prioritizations conducted at smaller geographic
#' scales (e.g, local or county scales).
#' For example, if this method is applied to
#' smaller geographic scales, then the resulting prioritizations
#' may select an overly large percentage of the study area,
#' or be biased towards over-representing common and widespread species.
#' This is because the target thresholds were developed based on
#' criteria for promoting the long-term persistence of entire species.
#' As such, if you are working at smaller scales, it is recommended to set
#' thresholds based on that criteria are appropriate to the spatial extent
#' of the planning region.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection add_manual_targets Target setting
#' @inheritSection add_rodrigues_targets Data calculations
#'
#' @inherit jung_targets return seealso
#'
#' @family method
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
#' IUCN (2024). Guidelines for the application of IUCN Red List of Ecosystems
#' Categories and Criteria, Version 2.0. Keith DA, Ferrer-Paris JR,
#' Ghoraba SMM, Henriksen S, Monyeki M, Murray NJ, Nicholson E, Rowland J,
#' Skowno A, Slingsby JA, Storeng AB, Valderrábano M, Zager I
#' (Eds.). Gland, Switzerland: IUCN.
#'
#' @examples
#' #TODO
#'
#' @name rl_ecosystem_targets
NULL

#' @rdname rl_ecosystem_targets
#' @export
rl_ecosystem_targets <- function(x, status, criterion_a, criterion_b,
                                 prop_uplift = 0, method = "max",
                                 cap_threshold = 1000000) {
  # return method
  new_method(
    name = "IUCN Red List of Ecosystem targets",
    type = "absolute",
    fun = internal_rl_ecosystem_targets,
    args = list(
      status = status,
      criterion_a = criterion_a,
      criterion_b = criterion_b,
      prop_uplift = prop_uplift,
      method = method,
      cap_threshold = cap_threshold
    )
  )
}

internal_rl_ecosystem_targets <- function(x, status, criterion_a,
                                          criterion_b,
                                          prop_uplift, method,
                                          cap_threshold,
                                          call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call)
  assert_required(status, call = call)
  assert_required(criterion_a, call = call)
  assert_required(criterion_b, call = call)
  assert_required(prop_uplift, call = call)
  assert_required(method, call = call)
  assert(
    # x
    is_conservation_problem(x),
    has_single_zone(x),
    # status
    assertthat::is.string(status),
    assertthat::noNA(status),
    is_match_of(status, c("CR", "EN", "VU")),
    # criterion a
    assertthat::is.string(criterion_a),
    assertthat::noNA(criterion_a),
    is_match_of(criterion_a, c("A1", "A2a", "A2b", "A3")),
    # criterion b
    assertthat::is.string(criterion_b),
    assertthat::noNA(criterion_b),
    is_match_of(criterion_b, c("B1", "B2")),
    # prop_uplift
    assertthat::is.number(prop_uplift),
    all_finite(prop_uplift),
    prop_uplift >= 0,
    # method
    assertthat::is.string(method),
    assertthat::noNA(method),
    is_match_of(method, c("min", "max")),
    # cap_threshold
    assertthat::is.scalar(cap_threshold),
    call = call
  )
  if (!is.null(cap_threshold)) {
    assert(
      assertthat::is.number(cap_threshold),
      all_finite(cap_threshold),
      cap_threshold >= 0,
      call = call
    )
  }

  # define thresholds
  criterion_data <- tibble::tribble(
    ~status, ~criterion, ~threshold,
    "CR", "A1", 0.8,
    "EN", "A1", 0.5,
    "VU", "A1", 0.3,
    "CR", "A2a", 0.8,
    "EN", "A2a", 0.5,
    "VU", "A2a", 0.3,
    "CR", "A2b", 0.8,
    "EN", "A2b", 0.5,
    "VU", "A2b", 0.3,
    "CR", "A3", 0.9,
    "EN", "A3", 0.7,
    "VU", "A3", 0.3,
    "CR", "B1", 2000,
    "EN", "B1", 20000,
    "VU", "B1", 50000,
    "CR", "B2", 200,
    "EN", "B2", 2000,
    "VU", "B2", 5000
  )

  # find thresholds
  size_threshold <- criterion_data$threshold[
    which(
      criterion_data$status == status & criterion_data$criterion == criterion_a
    )
  ]
  reduction_threshold <- criterion_data$threshold[
    which(
      criterion_data$status == status & criterion_data$criterion == criterion_b
    )
  ]
  assert(
    assertthat::is.number(size_threshold),
    msg = "Failed to find Criterion A threshold.",
    .internal = TRUE,
    call = call
  )
  assert(
    assertthat::is.number(reduction_threshold),
    msg = "Failed to find Criterion B threshold.",
    .internal = TRUE,
    call = call
  )

  # calculate targets
  internal_rl_targets(
    x = x,
    size_threshold = size_threshold,
    reduction_threshold = reduction_threshold,
    prop_uplift = prop_uplift,
    method = method,
    cap_threshold = cap_threshold,
    call = call
  )
}
