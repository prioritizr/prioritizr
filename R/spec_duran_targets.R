#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following Durán *et al.* (2020)
#'
#' Specify targets based on the methodology outlined by
#' Durán *et al.* (2020).
#' Briefly, this method involves using historical distribution
#' data to infer the minimum amount of habitat required for a species to have
#' a particular probability of persisting indefinitely.
#' Note that this function is designed to be used with [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param probability_target `numeric` vector denoting the minimum
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
#' @details
#' This target setting method is derived from a framework for estimating
#' the impacts of anthropogenic activities at national and global scales
#' (Durán *et al.* 2020).
#' It involves setting targets based on an estimate of the minimum amount of
#' habitat required for a species to have a particular probability of
#' persistence.
#' Although the gold standard approach for estimating such an amount of
#' habitat would involve population viability analysis
#' (e.g., Taylor *et al.* 2017),
#' population viability analyses require a considerable amount of
#' species-specific data that are often not available for conservation planning
#' exercises (reviewed by Akçakaya and Sjögren-Gulve 2000).
#' As such, this method provides a less data intensive
#' alternative for setting targets based on desired probabilities of
#' persistence.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' This target setting method involves calculating species representation
#' targets based on aspirational goals for species recovery and persistence.
#' For example, let's consider setting `probability_target = 0.95`.
#' If a species has a large amount of its historical distribution
#' remaining, then this probability target threshold may result in
#' setting a species representation target that is equivalent to 90% of the
#' species' current distribution. Assuming that the assumptions
#' that underpin this target setting method are correct (see below), such a
#' probability target threshold would seek to prevent future habitat loss
#' from causing this species to have a chance of persistence below 95%.
#' Additionally, if a threatened species has a relatively small amount
#' of its historical distribution remaining and not enough habitat for this
#' species remains to achieve a 95% chance of persistence, then
#' this probability target threshold may result in a setting species
#' representation target that is equivalent to 100% of the species' current
#' distribution. In this case -- given the assumptions of this target setting
#' method (see below) -- such a probability target threshold would seek
#' to enable future species recovery efforts to secure a 95%
#' chance of persistence.
#'
#' To use this method effectively, probability target thresholds
#' (i.e., `probability_targets`) will need to be set carefully.
#' One option for setting such thresholds could be based on probabilities
#' estimated for threat statuses associated with the Red List of
#' Threatened Species by the International Union for the Conservation
#' of Nature (IUCN).
#' For example, Davis et al. (2018) estimated that species recognized as
#' Least Concern on the Red List of
#' Threatened Species by the International Union for the Conservation
#' of Nature (IUCN) would have a 0.9983 probability of persistence
#' (i.e., 99.83% chance of persistence).
#' If less a less ambitious goal is more practical,
#' then probabilities of persistence for
#' other threat statuses could be more appropriate (e.g., such as the
#' probability estimated for the Vulnerable threat status).
#' Similar to Davis et al. (2018), Gumbs et al. (2023) also estimated
#' probabilities of extinction for threat statuses recognized the IUCN Red List
#' of Threatened Species. For reference, we provide the probabilities of
#' persistence estimated by Davis et al. (2018) and Gumbs et al. (2023) below.
#'
#' Davis et al. (2018) estimated the following probabilities of persistence.
#' \itemize{
#' \item Least Concern (LC) has `probability_targets = 0.9983`.
#' \item Near Threatened (NT) has `probability_targets = 0.9859`.
#' \item Vulnerable (VU) has `probability_targets = 0.9`.
#' \item Endangered (EN) has `probability_targets = 0.3277`.
#' \item Critically Endangered (CR) has `probability_targets = 0.001`.
#' }
#'
#' Gumbs et al. (2023) estimated the following probabilities of persistence.
#' \itemize{
#' \item Least Concern (LC) has `probability_targets = 0.939375`.
#' \item Near Threatened (NT) has `probability_targets = 0.87875`.
#' \item Vulnerable (VU) has `probability_targets = 0.7575`.
#' \item Endangered (EN) has `probability_targets = 0.515`.
#' \item Critically Endangered (CR) has `probability_targets = 0.03`.
#' }
#'
#' This target setting method relies heavily on assumptions.
#' In particular, it is based on the assumption that -- for a given species --
#' there is an
#' idealized distribution size (e.g., geographic range size) that would allow
#' for the species to have a 100% chance of persisting indefinitely, and
#' decreases in this distribution size would be associated with
#' reductions in the species' probability of persistence
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
#' The validity of this target setting method depends on how well its
#' assumptions are justified.
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
#'
#' @inheritSection spec_jung_targets Data calculations
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
#' Given this terminology, the target threshold (\eqn{t}) for the feature
#' is calculated as follows.
#' \deqn{
#' t = min(f, h \times p^{\frac{1}{0.25}})
#' }{
#' t = min(f, h * p^(1/0.25))
#' }
#'
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
#' Davis M, Faurby S, Svenning J-C (2018) Mammal diversity will take millions
#' of years to recover from the current biodiversity crisis.
#' *Proceedings of the National Academy of Sciences*, 115:11262--11267.
#'
#' Durán AP, Green JMH, West CD, Visconti P, Burgess ND, Virah‐Sawmy M,
#' Balmford A (2020) A practical approach to measuring the biodiversity impacts
#' of land conversion. *Methods in Ecology and Evolution*, 11:910--921.
#'
#' Eyres A, Ball TS, Dales M, Swinfield T, Arnell A, Baisero D, Durán AP, Green
#' JMH, Green RE, Madhavapeddy A, Balmford A (2025) LIFE: A metric for mapping
#' the impact of land-cover change on global extinctions.
#' *Philosophical Transactions of the Royal Society B: Biological Sciences*,
#' 380:20230327.
#'
#' Gumbs R, Gray CL, Böhm M, Burfield IJ, Couchman OR, Faith DP, Forest F,
#' Hoffmann M, Isaac NJB, Jetz W, Mace GM, Mooers AO, Safi K, Scott O, Steel M,
#' Tucker CM, Pearse WD, Owen NR, Rosindell J (2023) The EDGE2 protocol:
#' Advancing the prioritisation of Evolutionarily Distinct and Globally
#' Endangered species for practical conservation action. *PLOS Biology*,
#' 21:e3001991.
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
#' *PLoS ONE*, 12:e0169629.
#'
#' Thomas CD, Cameron A, Green RE, Bakkenes M, Beaumont LJ, Collingham YC,
#' Erasmus BFN, de Siqueira MF, Grainger A, Hannah L, Hughes L, Huntley B,
#' van Jaarsveld AS, Midgley GF, Miles L, Ortega-Huerta MA,
#' Townsend Peterson A, Phillips OL, Williams SE (2004)
#' Extinction risk from climate change. *Nature* 427:145--148.
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_complex_pu_raster <- get_sim_complex_pu_raster()
#' sim_complex_features <- get_sim_complex_features()
#' sim_complex_historical_features <- get_sim_complex_historical_features()
#'
#' # calculate the total historical distribution size for each feature.
#' # note that here we assume that the features in both sim_complex_features
#' # and sim_complex_historical_features follow the same ordering
#' historical_distribution_size <- as.numeric(units::set_units(
#'  units::set_units(
#'    terra::global(sim_complex_historical_features, "sum", na.rm = TRUE)[[1]] *
#'    prod(terra::res(sim_complex_historical_features)),
#'    "m^2"
#'  ),
#'  "km^2"
#' ))
#'
#' # create base problem
#' p0 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with targets based on the minimum amount of habitat required
#' # to ensure that each species has a 95% probability of persistence,
#' # following Duran et al. (2020)
#' # a 95% probability of persistence
#' p1 <-
#'  p0 %>%
#'  add_auto_targets(
#'     method = spec_duran_targets(
#'      probability_target = 0.95,
#'      historical_area = historical_distribution_size,
#'      area_units = "km^2"
#'    )
#'  )
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution based on 95% persistence targets", axes = FALSE)
#'
#' # create problem with targets based on the minimum amount of habitat required
#' # to ensure that each species has a particular probability of persistence,
#' # following Duran et al. (2020)
#'
#' # simulate a probability of persistence value for each feature
#' sim_probs <- runif(terra::nlyr(sim_complex_features), 0.1, 0.99)
#'
#' # now, create problem with these targets
#' p2 <-
#'  p0 %>%
#'  add_auto_targets(
#'     method = spec_duran_targets(
#'      probability_target = sim_probs,
#'      historical_area = historical_distribution_size,
#'      area_units = "km^2"
#'    )
#'  )
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution based on varying targets", axes = FALSE)
#' }
#' @export
spec_duran_targets <- function(probability_target,
                               historical_area,
                               area_units) {
  # assert arguments are valid
  assert_valid_method_arg(probability_target)
  assert_required(probability_target)
  assert_required(historical_area)
  assert_required(area_units)
  # return new method
  new_target_method(
    name = "Duran et al. (2020) targets",
    type = "relative",
    fun = calc_duran_targets,
    args = list(
      probability_target = probability_target,
      historical_area = historical_area,
      area_units = area_units
    )
  )
}

calc_duran_targets <- function(x, features,
                               probability_target,
                               historical_area,
                               area_units,
                               call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(probability_target, call = call, .internal = TRUE)
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
    # probability_target
    is.numeric(probability_target),
    all_finite(probability_target),
    all_proportion(probability_target),
    is_match_of(length(probability_target), c(1, number_of_features(x))),
    # historical_area
    is.numeric(historical_area),
    all_finite(historical_area),
    is_match_of(length(historical_area), c(1, number_of_features(x))),
    all_positive(historical_area),
    # area_units
    all_area_units(area_units),
    call = call
  )
  assert_can_calculate_area_based_targets(x, features, call = call)

  # if needed, duplicate values for each feature
  if (identical(length(probability_target), 1L)) {
    probability_target <- rep(probability_target, x$number_of_features())
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
  target <- historical_area_km2 * (probability_target^(1 / 0.25))
  pmin(target, fa) / fa
}
