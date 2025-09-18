#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following Duran *et al.* 2020
#'
#' Specify targets based on the methodology outlined by
#' Duran *et al.* (2020).
#' Briefly, this target setting method involves using historical distribution
#' data to infer the minimum amount of habitat required for a species to meet
#' a pre-specified probability of persisting indefinitely.
#' Note that this function is designed to be used within [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param probability_threshold `numeric` vector denoting the minimum
#' probability of persistence for each feature.
#' For example, a value of 0.1 corresponds to a 10% chance of
#' persistence, and a value of 1 corresponds to a 100% chance of persistence.
#' Values must range between 0 and 1.
#' If a single `numeric` value is specified, then all
#' features are assigned targets assuming the same probability threshold.
#'
#' @param historical_area `numeric` vector denoting the total area
#' encompassed by the historical distribution for each feature
#' (e.g., in \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#' If a single `numeric` value is specified, then all
#' features are assigned targets assuming the same historical distribution size.
#' See Details section for information on obtaining these data.
#'
#' @param area_units `character` vector denoting the unit of measurement
#' for the argument to `historical_area`.
#' For example, to specify that `historical_area` contains historical
#' distribution sizes
#' expressed as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}, then
#' `area_units = "km^2"` should be used.
#' If a single `character` value is specified, then all
#' features are assigned targets assuming the same area units.
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on the area
#' required to ensure that each feature meets a pre-specified
#' probability of persistence.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature (e.g., current geographic
#' range size expressed as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}),
#' \eqn{h} the historical total abundance of the feature (e.g., historical
#' range size expressed as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}},
#' per `historical_area` and `area_units`),
#' and \eqn{p} the desired threshold probability of persistence for the
#' feature.
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' \deqn{
#' t = min(f, h \times p^(1/0.25))
#' }{
#' t = min(f, h * p^(1/0.25))
#' }
#'
#' @details
#' This target setting method is derived from a framework for estimating
#' the impacts of anthropogenic activities at national and global scales
#' (Duran *et al.* 2020).
#' It involves setting targets based on the minimum amount of
#' habitat required for a species to have a particular probability of
#' persistence.
#' Although the gold standard approach for estimating such an amount would
#' involve performing a population viability analysis
#' (e.g., Taylor *et al.* 2017),
#' population viability analyses require a considerable amount of species-
#' specific data that are often not available for conservation planning
#' exercises (reviewed by Akçakaya and Sjögren-Gulve 2000).
#' As such, this method provides a less data intensive
#' alternative for setting targets based on desired probabilities of
#' persistence.
#'
#' This target setting method is heavily dependent on particular assumptions.
#' In particular, it is based on the assumption that -- for a given species --
#' there is an
#' idealized distribution size (e.g., geographic range size) that would allow
#' for the species to have a 100% chance of persisting indefinitely, and
#' decreases in this distribution size would be associated with
#' reductions in the probability of persistence
#' (Payne and Finnegan 2007; Purvis *et al.* 2000).
#' Building on this assumption, it further assumes that (i) the historical
#' distribution of a species can reliably approximate its idealized
#' distribution size with
#' a 100% chance of persisting indefinitely, and (ii)
#' proportionate decreases in the species' distribution size (relative to
#' its idealized distribution size) are associated
#' with increasingly greater reductions in probability of persistence
#' (i.e., following a power-law function with an exponent of 0.25)
#' (Balmford *et al.* 2018; Brooks *et al.* 1999; Thomas *et al.* 2004).
#' Based on these assumptions, this method involves estimating the minimum
#' amount of habitat required to ensure that a species has, at least, a
#' particular probability of persistence, and then setting the species'
#' representation target accordingly.
#'
#' The validity of this target setting method depends heavily on its
#' assumptions.
#' As such, great care should be taken to ensure that the
#' historical distribution of a species used to approximate its idealized
#' distribution does indeed have a (near) 100% probability of persistence
#' (Durán et al. 2020).
#' Although the package does not provide historical distribution data,
#' such data can be derived from species distribution modeling techniques.
#' For example, one approach for characterizing the historical distribution
#' of a species is to fit an environmental niche model based on present-day
#' environmental data and then use historical environmental data to predict
#' the species' historical distribution (reviewed by Nogués‐Bravo 2009).
#' Another approach involves using the area of habitat framework
#' (reviewed by Brooks *et al.* 2019) with information on a
#' species' habitat preferences,
#' current and former (i.e., now extinct) geographic ranges,
#' and historical land cover data (Eyres *et al.* 2025).
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection add_auto_targets Data calculations
#' @inherit spec_jung_targets return seealso
#'
#' @family methods
#'
#' @references
#' Akçakaya HR, Sjögren-Gulve P (2000) Population viability analyses in
#' conservation planning: an overview. *Ecological Bulletins*, 48:9--21.

#' Balmford B, Green RE, Onial M, Phalan B, Balmford A (2018) How imperfect can
#' land sparing be before land sharing is more favourable for wild species?
#' *Journal of Applied Ecology*, 56:73--84.
#'
#' Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
#' Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
#' Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial area of habitat
#' (AOH) and its utility for the IUCN Red List.
#' *Trends in Ecology and Evolution*, 34:977--986
#'
#' Brooks TM, Pimm SL, Oyugi JO (1999) Time lag between deforestation and bird
#' extinction in tropical forest fragments. *Conservation Biology*,
#' 13:1140--1150.
#'
#' Durán AP, Green JMH, West CD, Visconti P, Burgess ND, Virah‐Sawmy M,
#' Balmford A (2020) A practical approach to measuring the biodiversity impacts
#' of land conversion. *Methods in Ecology and Evolution*, 11:910--921.
#'
#' Eyres A, Ball TS, Dales M, Swinfield T, Arnell A, Baisero D, Durán AP, Green
#' JMH, Green RE, Madhavapeddy A, Balmford A (2025) LIFE: A metric for mapping
#' the impact of land-cover change on global extinctions.
#' *Philosophical Transactions of the Royal Society B: Biological Sciences*,
#' 380: 20230327.
#'
#' Nogués‐Bravo D (2009) Predicting the past distribution of species climatic
#' niches. Predicting the past distribution of species climatic niches.
#' *Global Ecology and Biogeography*, 18:521--531.
#'
#' Payne JL, Finnegan S (2007) The effect of geographic range on extinction
#' risk during background and mass extinction.
#' *Proceedings of the National Academy of Sciences*, 104:10506--10511.
#'
#' Purvis A, Gittleman JL, Cowlishaw G, Mace GM (2000) Predicting extinction
#' risk in declining species.
#' *Proceedings of the Royal Society of London. Series B: Biological Sciences*,
#' 267:1947--1952.
#'
#' Taylor C, Cadenhead N, Lindenmayer DB, Wintle BA (2017) Improving the design
#' of a conservation reserve for a critically endangered species.
#' *PLOS ONE*, 12:e0169629.
#'
#' Thomas CD, Cameron A, Green RE, Bakkenes M, Beaumont LJ, Collingham YC,
#' Erasmus BFN, de Siqueira MF, Grainger A, Hannah L, Hughes L, Huntley B,
#' van Jaarsveld AS, Midgley GF, Miles L, Ortega-Huerta MA,
#' Townsend Peterson A, Phillips OL, Williams SE (2004)
#' Extinction risk from climate change. *Nature* 427:145--148.
#'
#' @examples
#' #TODO
#'
#' @export
spec_duran_targets <- function(probability_threshold,
                               historical_area,
                               area_units) {
  # assert arguments are valid
  assert_valid_method_arg(probability_threshold)
  assert_required(probability_threshold)
  assert_required(historical_area)
  assert_required(area_units)
  # return new method
  new_method(
    name = "Duran et al. (2020) targets",
    type = "relative",
    fun = calc_duran_targets,
    args = list(
      probability_threshold = probability_threshold,
      historical_area = historical_area,
      area_units = area_units
    )
  )
}

calc_duran_targets <- function(x, features,
                               probability_threshold,
                               historical_area,
                               area_units,
                               call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(probability_threshold, call = call, .internal = TRUE)
  assert_required(historical_area, call = call, .internal = TRUE)
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
    # probability_threshold
    is.numeric(probability_threshold),
    all_finite(probability_threshold),
    all_proportion(probability_threshold),
    is_match_of(length(probability_threshold), c(1, number_of_features(x))),
    # historical_area
    is.numeric(historical_area),
    all_finite(historical_area),
    is_match_of(length(historical_area), c(1, number_of_features(x))),
    all(historical_area >= 0),
    # area_units
    all_area_units(area_units),
    call = call
  )
  assert_can_calculate_area_based_targets(x, features, call = call)

  # if needed, duplicate values for each feature
  if (identical(length(probability_threshold), 1L)) {
    probability_threshold <- rep(probability_threshold, x$number_of_features())
  }
  if (identical(length(historical_area), 1L)) {
    historical_area <- rep(historical_area, x$number_of_features())
  }
  if (identical(length(area_units), 1L)) {
    area_units <- rep(area_units, x$number_of_features())
  }

  # calculate historical ranges as km^2
  historical_area_km2 <- vapply(
    seq_along(historical_area),
    FUN.VALUE = numeric(1),
    function(i) as_km2(historical_area[[i]],  area_units[[i]])
  )

  # calculate target
  fa <- c(x$feature_abundances_km2_in_total_units()[features, 1])
  target <- historical_area_km2 * (probability_threshold^(1 / 0.25))
  pmin(target, fa) / fa
}
