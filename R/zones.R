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

#' Management zones
#'
#' Organize data for multiple features for multiple management zones.
#' Specifically, the data should describe the expected amount of each
#' feature within each planning unit given each management zone.
#' For example, the data could describe the occupancy
#' (e.g. presence/absence), probability of occurrence, or
#' abundance expected for each feature when each planning unit
#' is allocated to a different zone.
#'
#' @param ... [raster::raster()] or `character` objects that
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
#' \item{planning unit data are a [`Raster-class`] or [`Spatial-class`] object}{
#'   [`Raster-class`] object can be supplied to specify the expected amount of
#'   each feature within each planning unit under each management zone.
#'   Data for each zone should be specified as separate
#'   arguments, and the data for each feature in a given zone are specified
#'   in separate layers in a [raster::stack()] object.
#'   Note that all layers for a given zone must have `NA` values in exactly the
#'   same cells.}
#' \item{planning unit data are a [`Spatial-class`] or `data.frame`
#'   object}{`character` vector containing column names can
#'   be supplied to specify the expected amount of each feature under each
#'   zone. Note that these columns must not contain any `NA` values.}
#' \item{planning unit data are a [`Spatial-class`], `data.frame`, or
#'   `matrix` object}{`data.frame` object can be supplied to specify the
#'   expected amount of each feature under each zone.
#'   Following conventions used in *Marxan*, the
#'   `data.frame` object should contain the following columns.
#'   \describe{
#'     \item{pu}{`integer` planning unit identifier.}
#'     \item{species}{`integer` feature identifier.}
#'     \item{amount}{`numeric` amount of the feature in the
#'     planning unit for a given zone.}
#'   }
#'   Note that data for each zone are specified in a separate argument, and
#'   the data contained in a single `data.frame` object should correspond to
#'   a single zone. Also, note that data are not required for all
#'   combinations of planning units, features, and zones. The expected amount of
#'   features in planning units under management zones that are
#'   missing from the table are assumed to be zero.}
#' }
#'
#' @return [`Zones-class`] object.
#'
#' @seealso [problem()].
#'
#' @aliases Zones-class ZonesCharacter ZonesRaster Zones
#'
#' @examples
#' # load planning unit data
#' data(sim_pu_raster)
#'
#  # simulate distributions for three species under two management zones
#' zone_1 <- simulate_species(sim_pu_raster, 3)
#' zone_2 <- simulate_species(sim_pu_raster, 3)
#'
#' # create zones using two raster stack objects
#' # each object corresponds to a different zone and each layer corresponds to
#' # a different species
#' z <- zones(zone_1, zone_2, zone_names = c("zone_1", "zone_2"),
#'            feature_names = c("feature_1", "feature_2", "feature_3"))
#' print(z)
#'
#' # note that the do.call function can also be used to create a Zones object
#' # this method for creating a Zones object can be helpful when there are many
#' # management zones
#' l <- list(zone_1, zone_2, zone_names = c("zone_1", "zone_2"),
#'           feature_names = c("feature_1", "feature_2", "feature_3"))
#' z <- do.call(zones, l)
#' print(z)
#'
#' # create zones using character vectors that represent the names of
#' # fields (columns) in a data.frame or Spatial object that contain the amount
#' # of each species expected different management zones
#' z <- zones(c("spp1_zone1", "spp2_zone1"),
#'            c("spp1_zone2", "spp2_zone2"),
#'            c("spp1_zone3", "spp2_zone3"),
#'            zone_names = c("zone1", "zone2", "zone3"),
#'            feature_names = c("spp1", "spp2"))
#' print(z)
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
  # check zone names
  assertthat::assert_that(length(zone_names) == length(args),
                          msg = paste("number of supplied zones does not match",
                                      "number of the supplied zone names"))
  # assert that arguments are valid
  assertthat::assert_that(all(vapply(args, inherits, logical(1), "Raster")) ||
                          all(vapply(args, inherits, logical(1),
                                     "character")),
                          msg = paste("all arguments do not inherit from",
                                      "Raster or character classes"))
  assertthat::assert_that(anyDuplicated(names(args)) == 0)
  # checks for Raster input
  if (inherits(args[[1]], "Raster")) {
    # assign default feature names
    if (is.null(feature_names))
      feature_names <- as.character(seq_len(raster::nlayers(args[[1]])))
    # check feature names
    assertthat::assert_that(length(feature_names) == raster::nlayers(args[[1]]),
                            msg = paste("number of supplied features does not",
                                        "match the number of supplied feature",
                                        "names"))
    assertthat::assert_that(anyDuplicated(feature_names) == 0,
                            !anyNA(feature_names))
    for (i in seq_along(args))
      names(args[[i]]) <- feature_names
    # data integrity checks
    assertthat::assert_that(length(unique(vapply(args, raster::nlayers,
                                                 numeric(1)))) == 1,
                            msg = paste("all arguments do not have the same",
                                        "number of layers"))
    assertthat::assert_that(all(vapply(args, raster::compareRaster, logical(1),
                                       x = args[[1]], stopiffalse = FALSE,
                                       crs = TRUE, res = TRUE,
                                       tolerance = 1e-5)),
                            msg = paste("all arguments do not have the same",
                                        "spatial properties"))
    assertthat::assert_that(all(vapply(args, raster::nlayers, numeric(1)) >= 1),
                            msg = "all argument must have at least one layer")
    zone_class <- "ZonesRaster"
  }
  # checks for character input
  if (inherits(args[[1]], "character")) {
    # assign default feature names
    if (is.null(feature_names))
      feature_names <- as.character(seq_along(args[[1]]))
    # check feature names
    assertthat::assert_that(length(feature_names) == length(args[[1]]),
                            msg = paste("number of supplied features does not",
                                        "match the number of supplied feature",
                                        "names"))
    assertthat::assert_that(anyDuplicated(feature_names) == 0,
                            !anyNA(feature_names))
    # data integrity checks
    assertthat::assert_that(length(unique(vapply(args, length,
                                               numeric(1)))) == 1,
                          msg = paste("all arguments must all have the same",
                                      "number of features"))
    for (i in seq_along(args)) {
      assertthat::assert_that(all(!is.na(args[[i]])),
                              msg = paste("argument", i, "is contains NA",
                                          "values"))
    }
    zone_class <- "ZonesCharacter"
  }
  # return Zones class object
  structure(args, zone_names = zone_names, feature_names = feature_names,
            class = c(zone_class, "Zones"))
}
