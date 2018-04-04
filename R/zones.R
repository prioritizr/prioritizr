#' @include internal.R generics.R
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
#' Organize biodiversity data into the expected amount of different features
#' under different management zones.
#'
#' @param ... \code{\link[raster]{raster}} or \code{character} objects that
#'   pertain to the biodiversity data. See Details for more information.
#'
#' @param zone_names \code{character} names of the management zones. Defaults
#'   to \code{NULL} which results in sequential integers.
#'
#' @param feature_names \code{character} names of the features zones. Defaults
#'   to \code{NULL} which results in sequential integers.
#'
#' @details This function is used to store and organize data for use in a
#'   conservation planning \code{\link{problem}} that has multiple management
#'   zones. In all cases, the data for each zone is input as a separate
#'   argument. The correct arguments depends on the type of planning unit data
#'   used when building the conservation planning \code{\link{problem}}.
#'
#'   \describe{
#'
#'   \item{\code{\link[raster]{Raster-class}},
#'     \code{\link[sp]{Spatial-class}}}{\code{\link[raster]{Raster-class}}
#'     data denoting the amount of each feature present assuming each
#'     management zone. Data for each zone are specified in separate
#'     arguments, and the data for each feature in a given zone are specified
#'     in separate layers in a \code{\link[raster]{stack}} object. Note that
#'     all layers for a given zone must have \code{NA} values in exactly the
#'     same cells.}
#'
#'   \item{\code{\link{Spatial}}, \code{data.frame}}{\code{character} vector
#'     with column names that correspond to the abundance or occurrence of
#'     different features in each planning unit for each zone. Note that
#'     these columns must not contain any \code{NA} values.}
#'
#'   \item{\code{\link{Spatial}}, \code{data.frame} or
#'     \code{matrix}}{\code{data.frame} denoting the amount of each feature
#'     in each zone. Following conventions used in \emph{Marxan},
#'     \code{data.frame} objects should be supplied with the columns:
#'
#'     \describe{
#'
#'     \item{\code{"pu"}}{\code{integer} planning unit identifier.}
#'
#'     \item{\code{"species"}}{\code{integer} feature identifier.}
#'
#'     \item{\code{"amount"}}{\code{numeric} amount of the feature in the
#'       planning unit for a given zone.}
#'
#'     }
#'
#'     Note that data for each zone are specified in a separate argument, and
#'     the data contained in a single \code{data.frame} object correspond to
#'     a single zone. Also, note that data are not required for all
#'     combinations of planning units, features, and zones. The amounts of
#'     features in planning units assuming different management zones that are
#'     missing from the table are treated as zero.}
#'
#'   }
#'
#' @return \code{\link{Zones-class}} object.
#'
#' @seealso \code{\link{problem}}, \code{\link{Zones-methods}}.
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
  args <- list(...)
  names(args) <- NULL
  # assign default zones names
  if (is.null(zone_names))
    zone_names <- as.character(seq_along(args))
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

#' Zones methods
#'
#' Methods to help working with \code{\link{Zones-class}} objects.
#'
#' @param x \code{\link{Zones}} object.
#'
#' @details The following methods are available which output:
#'   \describe{
#'     \item{number_of_zones}{\code{integer} number of zones.}
#'
#'     \item{number_of_features}{\code{integer} number of features.}
#'
#'     \item{zone_names}{\code{character} zone names.}
#'
#'     \item{feature_names}{\code{character} feature names.}
#'
#'     \item{as.list}{\code{list} representation of the data.}
#'
#'   }
#'
#' @name Zones-methods
#'
#' @examples
#' # load ZonesRaster data
#' data(sim_features_zones)
#'
#' # print object
#' print(sim_features_zones)
#'
#' # print properties of the object
#' print(number_of_zones(sim_features_zones))
#' print(number_of_features(sim_features_zones))
#' print(zone_names(sim_features_zones))
#' print(feature_names(sim_features_zones))
#'
#' # convert zones object to a list
#' str(as.list(sim_features_zones), max.level = 1)
#'
#' # create ZoneCharacter object
#' z <- zones(c("spp1_z1", "spp2_z1"), c("spp1_z2", "spp2_z2"),
#'            feature_names = c("spp1", "spp2"),
#'            zone_names = c("z1", "z2"))
#'
#' # print object
#' print(z)
#'
#' # print properties of the object
#' print(number_of_zones(z))
#' print(number_of_features(z))
#' print(zone_names(z))
#' print(feature_names(z))
#'
#' # convert zones object to a list
#' str(as.list(z), max.level = 1)
#'
#' @aliases number_of_features,ZonesRaster-method number_of_zones,ZonesRaster-method zone_names,ZonesRaster-method feature_names,ZonesRaster-method number_of_features,ZonesCharacter-method number_of_zones,ZonesCharacter-method zone_names,ZonesCharacter-method feature_names,ZonesCharacter-method
NULL

#' @name Zones-methods
#'
#' @rdname Zones-methods
#'
#' @usage \S4method{number_of_zones}{ZonesRaster}(x)
#'
methods::setMethod("number_of_zones", "ZonesRaster", function(x) length(x))

#' @name Zones-methods
#'
#' @rdname Zones-methods
#'
#' @usage \S4method{number_of_zones}{ZonesCharacter}(x)
#'
methods::setMethod("number_of_zones", "ZonesCharacter", function(x) length(x))

#' @name Zones-methods
#'
#' @rdname Zones-methods
#'
#' @usage \S4method{number_of_features}{ZonesRaster}(x)
#'
methods::setMethod("number_of_features", "ZonesRaster",
  function(x) raster::nlayers(x[[1]]))

#' @name Zones-methods
#'
#' @rdname Zones-methods
#'
#' @usage \S4method{number_of_features}{ZonesCharacter}(x)
#'
methods::setMethod("number_of_features", "ZonesCharacter",
  function(x) length(x[[1]]))

#' @name Zones-methods
#'
#' @rdname Zones-methods
#'
#' @usage \S4method{zone_names}{ZonesRaster}(x)
#'
methods::setMethod("zone_names", "ZonesRaster",
  function(x) attr(x, "zone_names"))

#' @name Zones-methods
#'
#' @rdname Zones-methods
#'
#' @usage \S4method{zone_names}{ZonesCharacter}(x)
#'
methods::setMethod("zone_names", "ZonesCharacter",
  function(x) attr(x, "zone_names"))

#' @name Zones-methods
#'
#' @rdname Zones-methods
#'
#' @usage \S4method{feature_names}{ZonesRaster}(x)
#'
methods::setMethod("feature_names", "ZonesRaster",
  function(x) attr(x, "feature_names"))

#' @name Zones-methods
#'
#' @rdname Zones-methods
#'
#' @usage \S4method{feature_names}{ZonesCharacter}(x)
#'
methods::setMethod("feature_names", "ZonesCharacter",
  function(x) attr(x, "feature_names"))
