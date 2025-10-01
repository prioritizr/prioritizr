#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets based on the IUCN Red List of Threatened Species
#'
#' Specify targets based on criteria from the IUCN Red
#' List of Threatened Species (IUCN 2025).
#' Briefly, this method can be used to set targets based on
#' criteria pertaining to geographic range size (Criterion B) and
#' population size reduction criteria (Criterion A).
#' To help prevent widespread features from obscuring priorities for
#' rare features, targets are capped following Butchart *et al.* (2015).
#' This method may be suitable for species protection at global and
#' and national scales (e.g., Jung *et al.* 2021, Ward *et al.* 2025).
#' Note that this function is designed to be used with [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param status `character` value denoting the IUCN Red List threat
#' status used for target setting.
#' Available options include `"CR"` (Critically Endangered),
#' `"EN"` (Endangered), and `"VU"` (Vulnerable).
#'
#' @param criterion_a `character` value indicating which subcriterion
#' should be considered based on population size reduction.
#' Available options include subcriterion
#' `"A1"` (reduction that occurred in the past and the causes are reversible,
#' understood, and have ceased),
#' `"A2"` (reduction that occurred in the past and the causes may not be
#' reversible, understood, or have ceased);
#' `"A3"` (reduction is is inferred or suspected in the future), and
#' `"A4"` (reduction includes a time period in the past and the future and
#' causes may not be reversible, understood, or have ceased).
#' For convenience, these options can also be specified with lower case letters.
#' See Mathematical formulation below for details.
#'
#' @param criterion_b `character` value indicating which subcriterion
#' should be considered based on geographic range.
#' Available options include subcriterion
#' `"B1"` (extent of occupancy) and
#' `"B2"` (area of occupancy).
#' For convenience, these option can also be specified with lower case letters.
#' See Mathematical formulation below for details.
#'
#' @param method `character` indicating how the target thresholds
#' should be calculated based on values derived from Criterion A and and
#' Criterion B.
#' Available options include `"min"` and `"max"`.
#' For example, `method = "max"` means that the target threshold
#' should be calculated as a maximum of the values from
#' Criterion A and Criterion B.
#' Defaults to `"max"` as a precaution.
#'
#' @param prop_uplift `numeric` value denoting the percentage
#' uplift as a proportion. Defaults to 0 (i.e., 0%).
#'
#' @inheritParams spec_rodrigues_targets
#'
#' @details
#' Targets based on criteria from the IUCN Red List of Threatened Species
#' have been applied to global and national scale prioritizations
#' (e.g., Jung *et al.* 2021; Fastré *et al.* 2021; Ward *et al.* 2025).
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
#' @inheritSection spec_jung_targets Data calculations
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on assessment
#' criteria from the International Union for the Conservation of Nature (IUCN)
#' Red List of Threatened Species (IUCN 2025).
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature (e.g., geographic
#' range size), \eqn{a} the threshold value from Criterion A based on the
#' specified threat status (per `status`, see below for details),
#' \eqn{b} the threshold value from Criterion B
#' based on the specified threat status (per `status`, see below for details),
#' \eqn{p} the percentage uplift as a proportion (per `prop_uplift`),
#' \eqn{c} the target cap (per `cap_area_target` and `area_units`), and
#' \eqn{m()} denote either \eqn{max()} or \eqn{min()} (per `method`).
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' \deqn{
#' t = min(m(b \times (1 + p), f \times ((1 + p) \times (1 - a))), c, f)
#' }{
#' t = min(m(b * (1 + p), f * ((1 + p) * (1 - a))), c, f)
#' }
#'
#' Here \eqn{a} and \eqn{b} are equal to one of the following values
#' depending on `status`, `criterion_a`, and `criterion_b`.
#' Note that if `criterion_a` has a value of `"A3"` or `"A4"`, then \eqn{a}
#' is assigned the same value as if it were `"A2"`.
#' * If `status = "CR"` and `criterion_a = "A1"`, then \eqn{a =} 90%.
#' * If `status = "EN"` and `criterion_a = "A1"`, then \eqn{a =} 70%.
#' * If `status = "VU"` and `criterion_a = "A1"`, then \eqn{a =} 50%.
#' * If `status = "CR"` and `criterion_a = "A2"`, then \eqn{a =} 80%.
#' * If `status = "EN"` and `criterion_a = "A2"`, then \eqn{a =} 50%.
#' * If `status = "VU"` and `criterion_a = "A2"`, then \eqn{a =} 30%.
#' * If `status = "CR"` and `criterion_b = "B1"`, then \eqn{b =} 100 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "EN"` and `criterion_b = "B1"`, then \eqn{b =} 5,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "VU"` and `criterion_b = "B1"`, then \eqn{b =} 20,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "CR"` and `criterion_b = "B2"`, then \eqn{b =} 10 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "EN"` and `criterion_b = "B2"`, then \eqn{b =} 50 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' * If `status = "VU"` and `criterion_b = "B2"`, then \eqn{b =} 2,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
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
#' Fastré C, van Zeist W-J, Watson JEM, Visconti P (2021) Integrated spatial
#' planning for biodiversity conservation and food production. *One Earth*,
#' 4:1635--1644.
#'
#' IUCN (2025) The IUCN Red List of Threatened Species. Version 2025-1.
#' Available at <https://www.iucnredlist.org>. Accessed on 23 July 2025.
#'
#' Jung M, Arnell A, de Lamo X, García-Rangel S, Lewis M, Mark J, Merow C,
#' Miles L, Ondo I, Pironon S, Ravilious C, Rivers M, Schepaschenko D,
#' Tallowin O, van Soesbergen A, Govaerts R, Boyle BL, Enquist BJ, Feng X,
#' Gallagher R, Maitner B, Meiri S, Mulligan M, Ofer G, Roll U, Hanson JO,
#' Jetz W, Di Marco M, McGowan J, Rinnan DS, Sachs JD, Lesiv M, Adams VM,
#' Andrew SC, Burger JR, Hannah L, Marquet PA, McCarthy JK, Morueta-Holme N,
#' Newman EA, Park DS, Roehrdanz PR, Svenning J-C, Violle C, Wieringa JJ,
#' Wynne G, Fritz S, Strassburg BBN, Obersteiner M, Kapos V, Burgess N, Schmidt-
#' Traub G, Visconti P (2021) Areas of global importance for
#' conserving terrestrial biodiversity, carbon and water.
#' *Nature Ecology and Evolution*, 5:1499--1509.
#'
#' Mogg S, Fastre C, Jung M, Visconti P (2019) Targeted expansion of Protected
#' Areas to maximise the persistence of terrestrial mammals.
#' *Preprint at bioxriv*, \doi{10.1101/608992}.
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
#' # note that the following targets will be specified based on subcriterion
#' # A2 under the assumption that protected areas will be effectively managed,
#' # and B2 because the feature data (per sim_complex_features) characterize
#' # area of occupancy
#'
#' # create problem with targets based on criteria from the IUCN Red List of
#' # Threatened Species for the Endangered threat status with a 0% uplift
#' p1 <-
#'   p0 %>%
#'   add_auto_targets(
#'     method = spec_rl_species_targets(
#'       status = "EN",
#'       criterion_a = "A2",
#'       criterion_b = "B2",
#'       prop_uplift = 0
#'     )
#'   )
#'
#' # create problem with targets based on criteria from the IUCN Red List of
#' # Threatened Species for the Endangered threat status with a 20% uplift
#' p2 <-
#'   p0 %>%
#'   add_auto_targets(
#'     method = spec_rl_species_targets(
#'       status = "EN",
#'       criterion_a = "A2",
#'       criterion_b = "B2",
#'       prop_uplift = 0.2
#'     )
#'   )
#'
#' # create problem with targets based on criteria from the IUCN Red List of
#' # Threatened Species for the Vulnerable threat status with a 20% uplift
#' p3 <-
#'   p0 %>%
#'   add_auto_targets(
#'     method = spec_rl_species_targets(
#'       status = "VU",
#'       criterion_a = "A2",
#'       criterion_b = "B2",
#'       prop_uplift = 0.2
#'     )
#'   )
#'
#' # solve problems
#' s <- c(solve(p1), solve(p2), solve(p3))
#' names(s) <- c("EN (0%)", "EN (20%)", "VU (20%)")
#'
#' # plot solutions
#' plot(s, axes = FALSE)
#' }
#' @export
spec_rl_species_targets <- function(status, criterion_a, criterion_b,
                                    prop_uplift = 0, method = "max",
                                    cap_area_target = 1000000,
                                    area_units = "km^2") {
  # assert arguments are valid
  assert_valid_method_arg(status)
  assert_required(status)
  assert_required(criterion_a)
  assert_required(criterion_b)
  assert_required(prop_uplift)
  assert_required(method)
  assert_required(cap_area_target)
  assert_required(area_units)
  # return new method
  new_target_method(
    name = "IUCN Red List of Threatened Species targets",
    type = "relative",
    fun = calc_rl_species_targets,
    args = list(
      status = status,
      criterion_a = criterion_a,
      criterion_b = criterion_b,
      prop_uplift = prop_uplift,
      method = method,
      cap_area_target = cap_area_target,
      area_units = area_units
    )
  )
}

calc_rl_species_targets <- function(x, features, status,
                                    criterion_a,
                                    criterion_b,
                                    prop_uplift, method,
                                    cap_area_target,
                                    area_units,
                                    call = fn_caller_env()) {
  # assert that arguments are present
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(status, call = call, .internal = TRUE)
  assert_required(criterion_a, call = call, .internal = TRUE)
  assert_required(criterion_b, call = call, .internal = TRUE)
  assert_required(prop_uplift, call = call, .internal = TRUE)
  assert_required(method, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  # default argument handling
  if (is.character(status)) {
    status <- toupper(status)
  }
  if (is.character(criterion_a)) {
    criterion_a <- toupper(criterion_a)
  }
  if (is.character(criterion_b)) {
    criterion_b <- toupper(criterion_b)
  }
  # assert that arguments are valid
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
    # status
    assertthat::is.string(status),
    assertthat::noNA(status),
    is_match_of(status, c("CR", "EN", "VU")),
    # criterion a
    assertthat::is.string(criterion_a),
    assertthat::noNA(criterion_a),
    is_match_of(criterion_a, c("A1", "A2", "A3", "A4")),
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
    # cap_area_target
    assertthat::is.scalar(cap_area_target),
    # area_units
    is_area_units(area_units),
    call = call
  )
  if (assertthat::noNA(cap_area_target)) {
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

  # define thresholds
  ## note that B2 thresholds are in km^2 units
  criterion_data <- tibble::tribble(
    ~status, ~criterion, ~threshold,
    "CR", "A1", 0.9,
    "EN", "A1", 0.7,
    "VU", "A1", 0.5,
    "CR", "A2", 0.8,
    "EN", "A2", 0.5,
    "VU", "A2", 0.3,
    "CR", "A3", 0.8,
    "EN", "A3", 0.5,
    "VU", "A3", 0.3,
    "CR", "A4", 0.8,
    "EN", "A4", 0.5,
    "VU", "A4", 0.3,
    "CR", "B1", 100,
    "EN", "B1", 5000,
    "VU", "B1", 20000,
    "CR", "B2", 10,
    "EN", "B2", 500,
    "VU", "B2", 2000
  )

  # find thresholds
  reduction_threshold <- criterion_data$threshold[
    which(
      criterion_data$status == status & criterion_data$criterion == criterion_a
    )
  ]
  size_threshold <- criterion_data$threshold[
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
    features = features,
    size_threshold = size_threshold,
    reduction_threshold = reduction_threshold,
    prop_uplift = prop_uplift,
    method = method,
    cap_threshold = as_km2(cap_area_target, area_units),
    call = call
  )
}

internal_rl_targets <- function(x, features, size_threshold,
                                reduction_threshold,
                                prop_uplift, method, cap_threshold,
                                call = fn_caller_env()) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    is.numeric(features),
    assertthat::is.number(size_threshold),
    assertthat::is.number(reduction_threshold),
    assertthat::is.number(prop_uplift),
    assertthat::is.string(method),
    assertthat::is.number(cap_threshold),
    call = call,
    .internal = TRUE
  )
  assert_can_calculate_area_based_targets(x, features, call = call)

  # extract abundances
  fa <- x$feature_abundances_km2_in_total_units()[features, 1]

  # prepare values for target setting
  size_threshold <- size_threshold * (1 + prop_uplift)
  reduction_threshold <- (1 - reduction_threshold) * (1 + prop_uplift)

  # preliminary target calculations
  if (identical(method, "min")) {
    targets <- pmin(size_threshold, fa * reduction_threshold)
  } else {
    targets <- pmax(size_threshold, fa * reduction_threshold)
  }

  # apply cap thresholds
  targets <- pmin(targets, cap_threshold, na.rm = TRUE)

  # clamp targets to abundances
  targets <- pmin(fa, targets)

  # return targets
  targets / fa
}
