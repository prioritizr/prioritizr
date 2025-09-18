#' @include internal.R ConservationProblem-class.R
NULL

#' Specify targets following Wilson *et al.* (2010)
#'
#' Specify targets based on the methodology outlined by
#' Wilson *et al.* (2010).
#' Briefly, this method involves using population growth rate data to set
#' target thresholds based on the amount
#' of habitat required to sustain populations for 100,000 years.
#' Note that this function is designed to be used within [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param mean_growth_rates `numeric` vector that specifies the average
#' population growth rate that would be expected for each
#' feature within priority areas
#' (i.e., \eqn{r} per Wilson *et al.* 2010).
#' If a single `numeric` value is specified, then all
#' features are assigned targets based on the same average population growth
#' rate.
#' See Population data section for more details.
#'
#' @param var_growth_rates `numeric` vector that specifies the
#' variance in population growth rate that would be expected for each
#' feature within priority areas
#' (i.e., \eqn{\sigma^2} per Wilson *et al.* 2010).
#' If a single `numeric` value is specified, then all
#' features are assigned targets assuming the same variance in population
#' growth rate.
#' See Population data section for more details.
#'
#' @inheritParams spec_pop_size_targets
#'
#' @section Population data:
#' Although the package does not provide population growth rate data,
#' such data can be obtained from published databases or approximated
#' from physiological trait databases.
#' For example, Wilson *et al.* (2010) detail calculations to approximate
#' average population growth rate and variance in population growth rate
#' for mammal species based on body mass, and Santini *et al.* (2022) provide
#' body mass data for mammal species globally.
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on the amount of habitat
#' required to sustain populations for 100,000 years.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature (i.e., geographic
#' range size expressed as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}),
#' \eqn{k} the carrying capacity required for a population to persist for
#' 100,000 years,
#' \eqn{d} the population density of the feature
#' (i.e.,
#' number of individuals per \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}},
#' per `pop_density` and `density_units`),
#' \eqn{r} the mean population growth rate of the feature,
#' \eqn{sigma^2} the variance in population growth rate of the feature,
#' \eqn{b} is a constant calculated from \eqn{r} and \eqn{sigma^2},
#' , and \eqn{j} the target cap (expressed as
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}, per `cap_area_target`).
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' \deqn{
#' t = min(f, min(j, d \times k)) \\
#' k = (\frac{100000 \times \sigma^2 \times b^2}{2})^\frac{1}{b} \\
#' b = (2 \times \frac{r}{\sigma^2}) - 1
#' }{
#' t = min(f, min(j, d \times k)),
#' k = ((100000 * sigma^2 * b^2) / 2)^(1/b),
#' b = (2 * r/sigma^2) - 1
#' }
#'
#' @details
#' This target setting method was developed to identify the minimum amount
#' of habitat to protect an entire species (Wilson *et al.* 2010).
#' Although this method was originally applied to the sub-national scale
#' (i.e., the East Kalimantan province of Indonesia), Wilson *et al.* (2010)
#' linearly re-scaled targets derived from this method according to the
#' proportion of each species' distribution located within the study area
#' (i.e., based on the species' total distribution size in Borneo).
#' As such, this method may especially well-suited for national or global-scale
#' conservation planning exercises, and may also be useful for local-scale
#' planning exercises as long as the targets are re-scaled appropriately.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection spec_pop_size_targets Population density
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#' @inherit spec_jung_targets seealso return
#'
#' @references
#' IUCN (2025) The IUCN Red List of Threatened Species. Version 2025-1.
#' Available at <https://www.iucnredlist.org>. Accessed on 23 July 2025.
#'
#' Santini L, Benítez‐López A, Dormann CF, Huijbregts MAJ (2022) Population
#' density estimates for terrestrial mammal species.
#' *Global Ecology and Biogeography*, 31:978--994.
#'
#' Wilson KA, Meijaard E, Drummond S, Grantham HS, Boitani L, Catullo G,
#' Christie L, Dennis R, Dutton I, Falcucci A, Maiorano L, Possingham HP,
#' Rondinini C, Turner WR, Venter O, Watts M (2010) Conserving biodiversity in
#' production landscapes. *Ecological Applications*, 20:1721--1732.
#'
#' @family methods
#'
#' @examples
#' # TODO
#'
#' @export
spec_wilson_targets <- function(mean_growth_rates, var_growth_rates,
                                pop_density, density_units,
                                cap_area_target, area_units) {
  # assert arguments are valid
  assert_valid_method_arg(mean_growth_rates)
  assert_required(mean_growth_rates)
  assert_required(var_growth_rates)
  assert_required(pop_density)
  assert_required(density_units)
  assert_required(cap_area_target)
  assert_required(area_units)
  # return new method
  new_method(
    name = "Wilson targets",
    type = "relative",
    fun = calc_wilson_targets,
    args = list(
      mean_growth_rates = mean_growth_rates,
      var_growth_rates = var_growth_rates,
      pop_density = pop_density,
      density_units = density_units,
      cap_area_target = cap_area_target,
      area_units = area_units
    )
  )
}

calc_wilson_targets <- function(x, features,
                                mean_growth_rates, var_growth_rates,
                                pop_density, density_units,
                                cap_area_target, area_units,
                                call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(mean_growth_rates, call = call, .internal = TRUE)
  assert_required(var_growth_rates, call = call, .internal = TRUE)
  assert_required(pop_density, call = call, .internal = TRUE)
  assert_required(density_units, call = call,  .internal = TRUE)
  assert_required(cap_area_target, call = call,  .internal = TRUE)
  assert_required(area_units, call = call,  .internal = TRUE)
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
    # mean_growth_rates
    is.numeric(mean_growth_rates),
    is_match_of(length(mean_growth_rates), c(1, number_of_features(x))),
    all_finite(mean_growth_rates),
    all_positive(mean_growth_rates),
    # var_growth_rates
    is.numeric(var_growth_rates),
    is_match_of(length(var_growth_rates), c(1, number_of_features(x))),
    all_finite(var_growth_rates),
    all_positive(var_growth_rates),
    # pop_density
    is.numeric(pop_density),
    is_match_of(length(pop_density), c(1, number_of_features(x))),
    all_finite(pop_density),
    all_positive(pop_density),
    # density_units
    all_area_units(density_units),
    is_match_of(length(density_units), c(1, number_of_features(x))),
    # area_units
    is_area_units(area_units),
    call = call
  )
  assert_can_calculate_area_based_targets(x, features, call = call)

  # if needed, duplicate values for each feature
  if (identical(length(mean_growth_rates), 1L)) {
    mean_growth_rates <- rep(mean_growth_rates, x$number_of_features())
  }
  if (identical(length(var_growth_rates), 1L)) {
    var_growth_rates <- rep(var_growth_rates, x$number_of_features())
  }
  if (identical(length(pop_density), 1L)) {
    pop_density <- rep(pop_density, x$number_of_features())
  }
  if (identical(length(density_units), 1L)) {
    density_units <- rep(density_units, x$number_of_features())
  }

  # assert valid bounds for non-missing values
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

  # calculate population size targets based on required carrying capacity
  b <- (2 * mean_growth_rates / var_growth_rates) - 1
  pop_size_targets <- ((100000 * var_growth_rates * (b^2)) / 2)^(1 / b)

  # return targets
  calc_n_individuals_targets(
    x = c(x$feature_abundances_km2_in_total_units()[features, 1]),
    pop_size_targets = pop_size_targets,
    pop_density_km2 = as_per_km2(pop_density, density_units),
    cap_absolute_target = as_km2(cap_area_target, area_units),
    call = call
  )
}
