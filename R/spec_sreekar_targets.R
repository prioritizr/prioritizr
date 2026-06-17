#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following Sreekar and Watson (2026)
#'
#' Specify targets based on the methodology outlined by
#' Sreekar and Watson (2026).
#' Briefly, this method is based on updating the target setting method of
#' Rodrigues *et al.* (2024) to better reflect (i) Target 3 of the Kunming-
#' Montreal Global Biodiversity Framework when setting targets for common
#' species and (ii) ecological and sociopolitical constraints when setting
#' targets for rare species.
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed for global-scale prioritizations.
#' Note that this function is designed to be used with [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param rare_area_threshold `numeric` value indicating the threshold area
#' for rare identifying rare features.
#' Defaults to 200 (i.e., 200 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @param rare_relative_target `numeric` value denoting the
#' relative target for features with a spatial distribution
#' that is smaller than `rare_area_threshold`.
#' Note that this value must be a proportion between 0 and 1.
#' Defaults to 1 (i.e., 100%).
#'
#' @param common_area_threshold `numeric` value indicating the threshold area
#' for identifying common features.
#' Defaults to 250000
#' (i.e., 250,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @param common_relative_target `numeric` value denoting the
#' relative target for features with a spatial distribution
#' that is greater than `common_area_threshold`.
#' Defaults to 0.3 (i.e., 30%).
#'
#' @param cap_area_target `numeric` value denoting the area-based target cap.
#' To avoid setting a target cap, a missing (`NA`) value can be specified.
#' Defaults to 1000000
#' (i.e., 1,000,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @param area_units `character` value denoting the unit of measurement
#' for the area-based arguments.
#' Defaults to `"km^2"`
#' (i.e., \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @details
#' This target setting method was designed to protect species in global-scale
#' prioritizations (Sreekar and Watson 2026).
#' Although it may be useful for global-scales,
#' it may fail to identify meaningful priorities for
#' prioritizations conducted at smaller geographic scales
#' (e.g., national, state-level or county scales).
#' For example, if this method is applied to
#' such geographic scales, then the resulting prioritizations
#' may select an overly large percentage of the study area,
#' or be biased towards over-representing common and widespread species.
#' This is because the thresholds
#' (i.e., `rare_area_threshold`, `common_area_threshold`,
#' and `cap_area_threshold`)
#' were originally developed based on rationale for promoting the long-term
#' persistence of entire species.
#' As such, if you are working at a sub-global scale, it is recommended to set
#' thresholds based on that criteria are appropriate to the spatial extent
#' of the planning region.
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
#' spatial distribution smaller than 200
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}
#' (per `rare_area_threshold` and `area_units`)
#' and common features as those with a spatial distribution
#' larger than 250,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}
#' (per `common_area_threshold` and `area_units`).
#' Given this, rare features are assigned a target threshold
#' of 100% (per `rare_relative_target`), common features
#' are assigned a target threshold of 30% (per `common_relative_target`),
#' and features with a spatial distribution that is between
#' the area-based thresholds used to identify rare and common features are
#' assigned a target threshold through log-linear interpolation.
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
#' Rodrigues ASL, Akçakaya HR, Andelman SJ, Bakarr MI, Boitani L, Brooks TM,
#' Chanson JS, Fishpool LDC, Da Fonseca GAB, Gaston KJ, Hoffmann M, Marquet PA,
#' Pilgrim JD, Pressey RL, Schipper J, Sechrest W, Stuart SN, Underhill LG,
#' Waller RW, Watts MEJ, Yan X (2004)
#' Global gap analysis: priority regions for expanding the global
#' protected-area network. *BioScience*, 54: 1092--1100.
#'
#' Sreekar R and Watson JEM (2026) Updating species representation targets for
#' protected and conserved area planning. *Nature Reviews Biodiversity*,
#' \doi{10.1038/s44358-026-00169-7}.
#'
#' @examplesIf prioritizr::do_run_example()
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_complex_pu_raster <- get_sim_complex_pu_raster()
#' sim_complex_features <- get_sim_complex_features()
#'
#' # create problem with Sreekar and Watson (2026) targets
#' p1 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_auto_targets(method = spec_sreekar_targets()) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' @export
spec_sreekar_targets <- function(rare_area_threshold = 200,
                                 rare_relative_target = 1,
                                 common_area_threshold = 250000,
                                 common_relative_target = 0.3,
                                 cap_area_target = 1000000,
                                 area_units = "km^2") {
  # assert arguments are valid
  assert_valid_method_arg(rare_area_threshold)
  assert_required(rare_area_threshold)
  assert_required(rare_relative_target)
  assert_required(common_area_threshold)
  assert_required(common_relative_target)
  assert_required(cap_area_target)
  assert_required(area_units)
  # return new method
  new_target_method(
    name = "Sreekar and Watson (2026) targets",
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
      interp_method = "log10",
      area_units = area_units
    )
  )
}
