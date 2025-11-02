#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following Jung *et al.* (2021)
#'
#' Specify targets based on the methodology outlined by
#' Jung *et al.* (2021).
#' Briefly, this method involves setting targets based the criteria
#' for recognizing Vulnerable species by the International Union for the
#' Conservation of Nature (IUCN) Red List of Threatened Species (IUCN 2025).
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed for global-scale prioritizations.
#' Note that this function is designed to be used with [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param status `character` value denoting the IUCN Red List threat
#' status used for target setting.
#' Available options include `"CR"` (Critically Endangered) ,
#' `"EN"` (Endangered), and `"VU"` (Vulnerable).
#' Defaults to `"VU"`.
#'
#' @param prop_uplift `numeric` value denoting the percentage
#' uplift as a proportion. Defaults to 0.1 (i.e., 10%).
#'
#' @inheritParams spec_rodrigues_targets
#'
#' @details
#' This target setting method was designed to protect species in
#' global-scale prioritizations (Jung *et al.* 2021).
#' Although it has been successfully applied to multiple global-scale
#' prioritizations (e.g., Mogg *et al.* 2019; Fastré *et al.* 2021),
#' it may fail to identify meaningful priorities for
#' prioritizations conducted at smaller geographic scales
#' (e.g., national, state-level, or county scales).
#' For example, if this target setting method is applied to
#' smaller geographic scales, then the resulting prioritizations
#' may select an overly large percentage of the study area,
#' or be biased towards over-representing common and widespread species.
#' This is because the target thresholds were developed based on
#' criteria for promoting the long-term persistence of entire species.
#' As such, if you are working at smaller scales that do not fully
#' cover the entire spatial distribution of the study species,
#' then you may need to rescale these targets
#' (e.g., based on the proportion of the
#' species' distribution found within the study area) or consider an
#' alterative target setting method.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @section Data calculations:
#' This function involves calculating targets based on the spatial extent
#' of the features in `x`.
#' Although it can be readily applied to [problem()] objects that
#' have the feature data provided as a [terra::rast()] object,
#' you will need to specify the spatial units for the features
#' when initializing the [problem()] objects if the feature data
#' are provided in a different format. In particular, if the feature
#' data are provided as a `data.frame` or `character` vector,
#' then you will need to specify an argument to `feature_units` when
#' using the [problem()] function.
#' See the Examples section of the documentation for [add_auto_targets()]
#' for a demonstration of specifying the spatial units for features.
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on assessment
#' criteria from the International Union for the Conservation of Nature (IUCN)
#' Red List of Threatened Species (IUCN 2025).
#' In particular, it considers criteria related to
#' the size of a species' spatial distribution (i.e., Criterion B2)
#' and population size reduction (i.e., Criterion A2) and applies a
#' percentage-based uplift to them.
#' By default, it considers criteria for the Vulnerable threat status
#' with a 10% uplift and involves setting the target threshold for a species
#' as 2,200
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} or 80%
#' of its spatial distribution (which ever value is larger).
#' Additionally, following Butchart *et al.* (2015), a cap of 1,000,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} is applied to target
#' thresholds.
#'
#' @return
#' An object ([`TargetMethod-class`]) for specifying targets that
#' can be used with [add_auto_targets()] and [add_group_targets()]
#' to add targets to a [problem()].
#'
#' @seealso
#' See [targets] for an overview of all functions for adding targets.
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
#' # create problem with Jung et al. (2021) targets
#' p1 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_auto_targets(method = spec_jung_targets()) %>%
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
spec_jung_targets <- function(status = "VU", prop_uplift = 0.1,
                              cap_area_target = 1000000,
                              area_units = "km^2") {
  # assert arguments are valid
  assert_valid_method_arg(status)
  assert_required(status)
  assert_required(prop_uplift)
  assert_required(cap_area_target)
  assert_required(area_units)
  # return new method
  new_target_method(
    name = "Jung et al. (2021) targets",
    type = "relative",
    fun = calc_rl_species_targets,
    args = list(
      status = status,
      criterion_a = "A2",
      criterion_b = "B2",
      prop_uplift = prop_uplift,
      method = "max",
      cap_area_target = cap_area_target,
      area_units = area_units
    )
  )
}
