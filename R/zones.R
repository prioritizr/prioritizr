#' @include internal.R
#' @export
methods::setOldClass("Zones")
methods::setOldClass("ZonesCharacter")
methods::setOldClass("ZonesRaster")
NULL

#' Management zones
#'
#' Data for different biodiversity features in different management zones.
#'
#' @param ... \code{\link[raster]{raster}} or \code{character} objects. See
#'   Details for more information.
#'
#' @param zone_names \code{character} names of the management zones. Defaults
#'   to \code{NULL} which results in sequential integers.
#'
#' @param feature_names \code{character} names of the features zones. Defaults
#'   to \code{NULL} which results in sequential integers.
#'
#' @details This function is used to store and organize data for creating
#' \code{\link{ConservationProblem}} objects that have multiple management
#' zones. In all cases, data for different zones are input as different
#' arguments. The correct input depends on the type of planning unit data
#' used when building the conservation \code{\link{problem}}.
#' \describe{
#'   \item{\code{\link[raster]{Raster-class}},
#'     \code{\link[sp]{Spatial-class}}}{\code{\link[raster]{Raster-class}}
#'     data denoting the amount of each feature present assuming each
#'     management zone. Data for each zone are specified in seperate
#'     arguments, and the data for each feature in a given zone are specified
#'     in separate layers in a \code{\link[raster]{stack}} object. Note that
#'     all layers for a given zone must have \code{NA} values in exactly the
#'     same cells.}
#'   \item{\code{\link{Spatial}}, \code{data.frame}}{\code{character} vector
#'       with column names that correspond to the abundance or occurrence of
#'       different features in each planning unit for each zone. Note that
#'       these columns must not contain any \code{NA} values.}
#'   \item{\code{\link{Spatial}}, \code{data.frame} or
#'     \code{matrix}}{\code{data.frame} denoting the amount of each feature
#'     in each zone. Following conventions used in \emph{Marxan},
#'     \code{data.frame} objects should be supplied with the columns:
#'    \describe{
#'      \item{\code{"pu"}}{\code{integer} planning unit identifier.}
#'      \item{\code{"species"}}{\code{integer} feature identifier.}
#'      \item{\code{"amount"}}{\code{numeric} amount of the feature in the
#'        planning unit for a given zone.}
#'     }
#'     Note that data for each zone are specified in a seperate argument, and
#'     the data contained in a single \code{data.frame} object correspond to
#'     a single zone. Also, note that data are not required for all
#'     combinations of planning units, features, and zones. The amounts of
#'     features in planning units assuming different management zones that are
#'     missing from the table are treated as zero.}
#' }
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
#' # each object corresponds to a different zone
#  # each layer corresponds to a different species
#' z <- zones(zone_1, zone_2, zone_names = c("zone_1", "zone_2"),
#'            feature_names = c("feature_1", "feature_2"))
#' print(z)
#'
#' # note that do.call can also be used to create a Zones object
#' # this can be helpful when dealing with many zones
#' l <- list(zone_1, zone_2, zone_names = c("zone_1", "zone_2"),
#'           feature_names = c("feature_1", "feature_2"))
#' z <- do.call(zones, l)
#' print(z)
#'
#' # create zones using character vectors that represent the names of
#' # fields (columns) in a data.frame or Spatial object that contain the amount
#' # of each species under a different management zone.
#' z <- zones(c("spp1_zone1", "spp2_zone1"),
#'            c("spp1_zone2", "spp2_zone2"),
#'            c("spp1_zone3", "spp2_zone3"),
#'            zone_names = c("zone1", "zone2"),
#'            feature_names = c("spp1". "spp2", "spp3"))
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
  assertthat::assert_that(length(unique(vapply(args, function(x) class(x)[[1]],
                                               character(1)))) == 1,
                          msg = paste("all arguments do not inherit from the",
                                      "same class"))
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
#
#' @param ... not used.
#'
#' @details The following methods are available which output:
#'   \describe{
#'     \item{n_zone}{\code{integer} number of zones.}
#'
#'     \item{n_feature}{\code{integer} number of features.}
#'
#'     \item{zone_names}{\code{character} zone names.}
#'
#'     \item{feature_names}{\code{character} feature names.}
#'
#'     \item{as.list}{\code{list} representation of the data.}
#'
#'   }
#'
#' @name ZonesMethods
#'
#' @aliases Zones-methods
#'
#' @examples
#' # load zones data
#' data(sim_features_zones)
#'
#' #print object
#' print(sim_features_zones)
#'
#' # print properties of the object
#' n_zone(sim_features_zones)
#' n_feature(sim_features_zones)
#' zone_names(sim_features_zones)
#' feature_names(sim_features_zones)
#'
#' # convert zones object to a list
#' str(as.list(sim_features_zones), max.level = 1)
NULL

#' @rdname ZonesMethods
#' @export
n_zone <- function(x, ...) UseMethod("n_zone")

#' @rdname ZonesMethods
#' @method n_zone Zones
#' @export
n_zone.Zones <- function(x, ...) {
  assertthat::assert_that(inherits(x, "Zones"))
  length(x)
}

#' @rdname ZonesMethods
#' @export
n_feature <- function(x, ...) UseMethod("n_feature")

#' @rdname ZonesMethods
#' @method n_feature ZonesCharacter
#' @export
n_feature.ZonesCharacter <- function(x, ...) {
  length(x[[1]])
}

#' @rdname ZonesMethods
#' @method n_feature ZonesCharacter
#' @export
n_feature.ZonesRaster <- function(x, ...) {
  raster::nlayers(x[[1]])
}

#' @rdname ZonesMethods
#' @export
zone_names <- function(x, ...) UseMethod("zone_names")

#' @rdname ZonesMethods
#' @method zone_names Zones
#' @export
zone_names.Zones <- function(x, ...) {
  attr(x, "zone_names")
}

#' @rdname ZonesMethods
#' @export
feature_names <- function(x, ...) UseMethod("feature_names")

#' @rdname ZonesMethods
#' @method feature_names Zones
#' @export
feature_names.Zones <- function(x, ...) {
  attr(x, "feature_names")
}