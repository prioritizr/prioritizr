#' @include internal.R
NULL

#' @export
if (!methods::isClass("ZonesCharacter"))
  methods::setOldClass("ZonesCharacter")
NULL

#' @export
if (!methods::isClass("ZonesRaster"))
  methods::setOldClass("ZonesRaster")
NULL

#' @export
if (!methods::isClass("ZonesSpatRaster"))
  methods::setOldClass("ZonesSpatRaster")
NULL

#' Management zones
#'
#' Organize data for multiple features for multiple management zones.
#' Specifically, the data should describe the expected amount of each
#' feature within each planning unit given each management zone.
#' For example, the data could describe the occupancy
#' (e.g., presence/absence), probability of occurrence, or
#' abundance expected for each feature when each planning unit
#' is allocated to a different zone.
#'
#' @param ... [terra::rast()] or `character` objects that
#'   pertain to the biodiversity data. See Details for more information.
#'
#' @param zone_names `character` names of the management zones. Defaults
#'   to `NULL` which results in sequential integers.
#'
#' @param feature_names `character` names of the features zones. Defaults
#'   to `NULL` which results in sequential integers.
#'
#' @details
#' This function is used to store and organize data for use in a
#' conservation planning [problem()] that has multiple management
#' zones.
#' In all cases, the data for each zone is input as a separate argument.
#' The correct arguments depends on the type of planning unit data
#' used when building the conservation planning [problem()].
#'
#' \describe{
#'
#' \item{[problem()] will have [terra::rast()] or [sf::st_sf()] planning units}{
#'   [terra::rast()] object can be supplied to specify the expected amount of
#'   each feature within each planning unit under each management zone.
#'   Data for each zone should be specified as separate
#'   arguments, and the data for each feature in a given zone are specified
#'   in separate layers in a [terra::rast()] object.
#'   Note that all layers for a given zone must have missing (`NA`) values in
#'   exactly the same cells.}
#'
#' \item{[problem()] will have [sf::st_sf()] or `data.frame` planning units}{
#'   `character` vector containing column names can
#'   be supplied to specify the expected amount of each feature under each
#'   zone. Note that these columns must not contain any missing (`NA`) values.}
#'
#' \item{[problem()] will have [sf::st_sf()], `data.frame`, or
#'   `matrix` planning units}{
#'   `data.frame` object can be supplied to specify the
#'   expected amount of each feature under each zone.
#'   Following conventions used in *Marxan*, the
#'   `data.frame` object should contain the following columns.
#'   \describe{
#'     \item{pu}{`integer` planning unit identifier.}
#'     \item{species}{`integer` feature identifier.}
#'     \item{amount}{`numeric` amount of the feature in the
#'     planning unit for a given zone.}
#'   }
#'
#'   Note that data for each zone are specified in a separate argument, and
#'   the data contained in a single `data.frame` object should correspond to
#'   a single zone. Also, note that data are not required for all
#'   combinations of planning units, features, and zones. The expected amount of
#'   features in planning units under management zones that are
#'   missing from the table are assumed to be zero.}
#'
#' }
#'
#' @return A [`Zones-class`] object.
#'
#' @seealso
#' See [problem()] for information on using this function to generate
#' a prioritization with multiple management zones.
#'
#' @aliases Zones-class ZonesCharacter ZonesRaster ZonesSpatRaster Zones
#'
#' @examples
#' \dontrun{
#' # load planning unit data
#' sim_pu_raster <- get_sim_pu_raster()
#'
#  # simulate distributions for three species under two management zones
#' zone_1 <- simulate_species(sim_pu_raster, 3)
#' zone_2 <- simulate_species(sim_pu_raster, 3)
#'
#' # create zones using two SpatRaster objects
#' # each object corresponds to a different zone and each layer corresponds to
#' # a different species
#' z <- zones(
#'   zone_1, zone_2,
#'   zone_names = c("zone_1", "zone_2"),
#'   feature_names = c("feature_1", "feature_2", "feature_3")
#' )
#' print(z)
#'
#' # note that the do.call function can also be used to create a Zones object
#' # this method for creating a Zones object can be helpful when there are many
#' # management zones
#' l <- list(
#'   zone_1, zone_2,
#'   zone_names = c("zone_1", "zone_2"),
#'   feature_names = c("feature_1", "feature_2", "feature_3")
#' )
#' z <- do.call(zones, l)
#' print(z)
#'
#' # create zones using character vectors corresponding to column names
#' # of a data.frame or Spatial object that contain the amount
#' # of each species expected different management zones
#' z <- zones(
#'   c("spp1_zone1", "spp2_zone1"),
#'   c("spp1_zone2", "spp2_zone2"),
#'   c("spp1_zone3", "spp2_zone3"),
#'   zone_names = c("zone1", "zone2", "zone3"),
#'   feature_names = c("spp1", "spp2")
#' )
#' print(z)
#' }
#' @export
zones <- function(..., zone_names = NULL, feature_names = NULL) {
  # parse arguments
  args <- list(...)
  # assign default zones names
  if (is.null(zone_names) && is.null(names(args)))
    zone_names <- as.character(seq_along(args))
  if (is.null(zone_names) && !is.null(names(args)))
    zone_names <- names(args)
  # set names of args to NULL
  names(args) <- NULL
  # check arguments
  assert(
    all_elements_inherit(args, c("SpatRaster", "Raster", "character")),
    no_duplicates(zone_names),
    assertthat::noNA(zone_names)
  )
  assert(
    length(zone_names) == length(args),
    msg = c(
      paste(
        "The number of {.code ...} arguments must match the number of elements",
        "in {.arg zone_names}."
      ),
      "x" = "Number of {.code ...} arguments: {.val {length(args)}}.",
      "x" = "{.code length(zone_names)}: {.val {length(zone_names)}}."
    )
  )
  # throw deprecation notice if needed
  if (inherits(args[[1]], "Raster")) {
    cli_warning(raster_pkg_deprecation_notice)
  }
  # checks for SpatRaster/Raster input
  if (inherits(args[[1]], c("SpatRaster", "Raster"))) {
    # set functions for calculating lengths
    if (inherits(args[[1]], "SpatRaster")) {
      n_fun <- terra::nlyr
    } else if (inherits(args[[1]], "Raster")) {
      n_fun <- raster::nlayers
    }
    # set names using defaults if none set
    if (is.null(feature_names)) {
      feature_names <- as.character(seq_len(n_fun(args[[1]])))
    }
    # check feature names
    assert(
      length(feature_names) == n_fun(args[[1]]),
      msg = c(
        paste(
          "The number of layers in each {.code ...} argument must match the",
          "number of elements in {.arg feature_names}."
        ),
        "x" = paste(
          "Number of layers in first {.code ...} argument:",
          "{.val {n_fun(args[[1]])}}."
        ),
        "x" = "{.code length(feature_names)}: {.val {length(feature_names)}}."
      )
    )
    # data integrity checks
    assert(
      length(unique(vapply(args, n_fun, numeric(1)))) == 1,
      msg = "All {.code ...} arguments must have the same number of layers."
    )
    assert(
      all(vapply(args, n_fun, numeric(1)) >= 1),
      msg = "All {.code ...} arguments must have at least one layer."
    )
    assert(
      all(vapply(args, is_comparable_raster, logical(1), x = args[[1]])),
      msg = paste(
        "All {.code ...} arguments must have the same",
        "spatial resolutions, extents, coordinate reference systems, and",
        "dimensionality (rows / columns)."
      )
    )
    # set class
    zone_class <- ifelse(
      inherits(args[[1]], "SpatRaster"),
      "ZonesSpatRaster",
      "ZonesRaster"
    )
  }
  # checks for character input
  if (inherits(args[[1]], "character")) {
    # assign default feature names
    if (is.null(feature_names)) {
      feature_names <- as.character(seq_along(args[[1]]))
    }
    # check feature names
    assert(
      length(feature_names) == length(args[[1]]),
      msg = c(
        paste(
          "The number of elements in each {.code ...} argument must match the",
          "number of elements in {.arg feature_names}."
        ),
        "x" = paste(
          "Number of elements in first {.code ...} argument:",
          "{.val {length(args[[1]])}}."
        ),
        "x" = "{.code length(feature_names)}: {.val {length(feature_names)}}."
      )
    )
    # data integrity checks
    assert(
      length(unique(vapply(args, length, numeric(1)))) == 1,
      msg = "All {.code ...} arguments must have the same number of elements."
    )
    for (i in seq_along(args)) {
      assert(
        assertthat::noNA(args[[i]]),
        msg =
         "All {.code ...} arguments must not contain any {.val NA} values"
       )
    }
    zone_class <- "ZonesCharacter"
  }
  # check feature names
  assert(
    no_duplicates(feature_names),
    assertthat::noNA(feature_names)
  )
  # return Zones class object
  structure(
    args,
    zone_names = zone_names,
    feature_names = feature_names,
    class = c(zone_class, "Zones")
  )
}
