#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following Polak *et al.* (2015)
#'
#' Specify targets based on the methodology outlined by
#' Polak *et al.* (2015).
#' Briefly, this method involves setting targets based on linear
#' interpolation methods.
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' Note that this function is designed to be used with [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @inheritParams spec_rodrigues_targets
#'
#' @param common_area_threshold `numeric` value indicating the threshold
#' area for identifying common features.
#' Defaults to 10000
#' (i.e., 10,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @details
#' This target setting method was designed to protect species in national-
#' scale prioritizations (Polak *et al.* 2015).
#' Although it has been successfully applied to to national-scales
#' (e.g., Polak *et al.* 2016; Clements *et al.* 2018; ),
#' it may fail to identify meaningful priorities for
#' prioritizations conducted at smaller or larger geographic scales
#' (e.g., local or global scales).
#' For example, if this method is applied to
#' smaller geographic scales, then the resulting prioritizations
#' may select an overly large percentage of the study area,
#' or be biased towards over-representing common and widespread species.
#' This is because the thresholds for defining rare and common
#' features (i.e., `rare_area_threshold` and `common_area_threshold`)
#' were originally developed based on criteria for national-scales.
#' As such, if you working at a different scale, you may need to calibrate
#' these thresholds based on the spatial extent of the planning region.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection spec_jung_targets Data calculations
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on the spatial
#' extent of the features.
#' By default, this method identifies rare features as those with a
#' spatial distribution smaller than 1,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}
#' (per `rare_area_threshold` and `area_units`)
#' and common features as those with a spatial distribution
#' larger than 10,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}
#' (per `common_area_threshold` and `area_units`).
#' Given this, rare features are assigned a target threshold
#' of 100% (per `rare_relative_target`), common features
#' are assigned a target threshold of 10% (per `common_relative_target`),
#' and features with a spatial distribution that is between
#' the area-based thresholds used to identify rare and common features are
#' assigned a target threshold through linear interpolation.
#' Additionally, following Butchart *et al.* (2015), a cap of 1,000,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} is applied to target
#' thresholds (per `cap_area_threshold` and `area_units`).
#'
#' @inherit spec_jung_targets return seealso
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
#' Clements HS, Kearney SG, Cook CN (2018) Moving from representation to
#' persistence: The capacity of Australia's National Reserve System to support
#' viable populations of mammals. *Diversity and Distributions*,
#' 24: 1231--1241.
#'
#' Polak T, Watson JEM, Fuller RA, Joseph LN, Martin TG, Possingham HP,
#' Venter O, Carwardine J (2015) Efficient expansion of global protected areas
#' requires simultaneous planning for species and ecosystems.
#' *Royal Society Open Science*, 2: 150107.
#'
#' Polak T, Watson JEM, Bennett JR, Possingham HP, Fuller RA, Carwardine J
#' (2016) Balancing ecosystem and threatened species representation in
#' protected areas and implications for nations achieving global conservation
#' goals. *Conservation Letters*, 9:438--445.
#'
#' UNEP-WCMC and IUCN (2025) Protected Planet Report 2024.
#' Cambridge, UK: UNEP-WCMC and IUCN. Available at <www.protectedplanet.net>.
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
#' # create problem with Polak et al. (2015) targets
#' p1 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_auto_targets(method = spec_polak_targets()) %>%
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
spec_polak_targets <- function(rare_area_threshold = 1000,
                               rare_relative_target = 1,
                               common_area_threshold = 10000,
                               common_relative_target = 0.1,
                               cap_area_target = 1000000,
                               area_units = "km^2") {
  # assert arguments are valid
  assert_valid_method_arg(rare_area_threshold)
  assert_required(rare_area_threshold)
  assert_required(rare_relative_target)
  assert_required(common_area_threshold,)
  assert_required(common_relative_target)
  assert_required(cap_area_target)
  assert_required(area_units)
  # return new method
  new_target_method(
    name = "Polak et al. (2015) targets",
    type = "relative",
    fun = calc_interp_area_targets,
    args = list(
      rare_area_threshold = rare_area_threshold,
      rare_relative_target = rare_relative_target,
      rare_area_target = NA_real_,
      rare_method = "max", # has no effect
      common_area_threshold = common_area_threshold,
      common_relative_target = common_relative_target,
      common_area_target = NA_real_,
      common_method = "max", # has no effect
      cap_area_target = cap_area_target,
      interp_method = "linear",
      area_units = area_units
    )
  )
}
