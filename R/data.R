#' @include internal.R
NULL

#' Get simulated conservation planning data
#'
#' A set of functions are available for importing simulated datasets. These
#' datasets are designed for creating small example spatial prioritizations.
#'
#' @section Single zone datasets:
#'
#' The following functions are provided for generating spatial prioritizations
#' that only contain a single management zone.
#'
#' \describe{
#'
#' \item{`get_sim_pu_raster()`}{Import planning unit data that are stored
#'   in raster format.
#'   Here, cell values indicate planning unit cost and missing (`NA`)
#'   values indicate that a cell is not a planning unit.}
#'
#' \item{`get_sim_locked_in_raster()`}{Import planning unit data
#'   that are stored in raster format.
#'   Here, cell values are binary and indicate if planning units should be
#'   locked in to a solution.}
#'
#' \item{`get_sim_locked_out_raster()`}{Import planning unit data
#'   that are stored in raster format.
#'   Here, cell values are binary and indicate if planning units
#'   should be locked out from a solution.}
#'
#' \item{`get_sim_pu_polygons()`}{Import planning unit data stored in vector
#'   format. Here, planning units are represented using spatial polygons
#'   (e.g., each polygon corresponds to a different management areas).
#'   The data contains columns indicating the expenditure
#'   required for prioritizing each planning unit (`"cost"` column), if the
#'   planning units should be selected in the solution (`"locked_in"` column),
#'   and if the planning units should never be selected in the solution
#'   (`"locked_out"` column).}
#'
#' \item{`get_sim_pu_points()`}{Import planning unit data stored in vector
#'   format. Here, planning units are represented using spatial lines
#'   (e.g., each line corresponds to a different section along a river) .
#'   The attribute table follows the same conventions as for
#'   `sim_pu_polygons`.}
#'
#' \item{`get_sim_pu_lines()`}{Import planning unit data stored in vector
#'   format. Here, planning units are represented using spatial points
#'   (e.g., each point corresponds to a different site) .
#'   The attribute table follows the same conventions as for
#'   `sim_pu_polygons`.}
#'
#' \item{`get_sim_features()`}{Import feature data stored in raster format.
#'   Here, data describe the spatial distribution of ten species.
#'   Each layer corresponds to a different species, and
#'   cell values indicate habitat suitability.}
#'
#'  \item{`get_sim_phylogeny()`}{Import phylogenetic tree for the ten species.}
#'
#' }
#'
#' @section Multiple zone datasets:
#'
#' The following functions are provided for generating spatial prioritizations
#' that contain multiple management zones.
#'
#' \describe{
#'
#' \item{`get_sim_zones_pu_raster()`}{Import planning unit data
#'   for multiple management zones that are stored in raster format.
#'   Here, each layer indicates the cost for a different management
#'   zone. Cells with `NA` values in a given zone indicate that a
#'   planning unit cannot be allocated to that zone in a solution.
#'   Additionally, cells with `NA` values in all layers are not a
#'   planning unit.}
#'
#' \item{`get_sim_zones_pu_polygons()`}{Import planning unit data for
#'   multiple management zones stored in vector format.
#'   Here, planning units are represented using spatial polygons.
#'   The data contains columns indicating the
#'   expenditure required for prioritizing each planning unit under different
#'   management zones (`"cost_1"`, `"cost_2"`, and `"cost_3"` columns), and a
#'   series
#'   of columns indicating the value that each planning unit that should be
#'   assigned in the solution (`"locked_1"`, `"locked_2"`, `"locked_3"`
#'   columns).
#'   In these locked columns, planning units that should not be locked to a
#'   specific value are assigned a missing (`NA`) value.}
#'
#' \item{`get_sim_zones_features()`}{Import feature data for multiple
#'   management zones stored in raster format.
#'   Here, data describe the spatial distribution of ten species
#'   under three different management zones.}
#'
#' }
#'
#' @aliases sim_pu_polygons sim_zones_pu_polygons sim_pu_points sim_pu_lines sim_pu_raster sim_locked_in_raster sim_locked_out_raster sim_zones_pu_raster sim_features sim_zones_features sim_phylogeny get_sim_pu_polygons get_sim_zones_pu_polygons get_sim_pu_points get_sim_pu_lines get_sim_pu_raster get_sim_locked_in_raster get_sim_locked_out_raster get_sim_zones_pu_raster get_sim_features get_sim_zones_features get_sim_phylogeny
#'
#' @format
#' \describe{
#'
#' \item{`get_sim_pu_polygons()`}{[sf::st_sf()] object.}
#'
#' \item{`get_sim_zones_pu_polygons()`}{[sf::st_sf()] object.}
#'
#' \item{`get_sim_pu_lines()`}{[sf::st_sf()] object.}
#'
#' \item{`get_sim_pu_points()`}{[sf::st_sf()] object.}
#'
#' \item{`get_sim_pu_raster()`}{[terra::rast()] object.}
#'
#' \item{`get_sim_zones_pu_raster()`}{[terra::rast()] object.}
#'
#' \item{`get_sim_locked_in_raster()`}{[terra::rast()] object.}
#'
#' \item{`get_sim_locked_out_raster()`}{[terra::rast()] object.}
#'
#' \item{`get_sim_features()`}{[terra::rast()] object.}
#'
#' \item{`get_sim_zones_features()`}{[ZonesRaster()] object.}
#'
#' \item{`get_sim_phylogeny()`}{[terra::rast()] object.}
#'
#' }
#'
#' @keywords datasets
#'
#' @examples
#' # load data
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
#' sim_pu_lines <- get_sim_pu_lines()
#' sim_pu_points <- get_sim_pu_points()
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_locked_in_raster <- get_sim_locked_in_raster()
#' sim_locked_out_raster <- get_sim_locked_out_raster()
#' sim_phylogeny <- get_sim_phylogeny()
#' sim_features <- get_sim_features()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # plot raster data
#' \dontrun{
#' par(mfrow = c(1, 3))
#' plot(sim_pu_raster, main = "planning units (raster)", axes = FALSE)
#' plot(sim_locked_in_raster, main = "locked in units (raster)", axes = FALSE)
#' plot(sim_locked_out_raster, main = "locked out units (raster)", axes = FALSE)
#'
#' # plot vector planning unit data
#' par(mfrow = c(1, 1))
#' plot(sim_pu_polygons)
#' plot(sim_pu_lines)
#' plot(sim_pu_points)
#'
#' # plot vector planning unit data for multiple management zones
#' plot(sim_zones_pu_polygons)
#'
#' # plot phylogeny data
#' par(mfrow = c(1, 1))
#' plot(sim_phylogeny, main = "simulated phylogeny")
#'
#' # plot feature data
#' par(mfrow = c(1, 1))
#' plot(sim_features, axes = FALSE)
#'
#' # plot cost data for multiple management zones
#' par(mfrow = c(1, 1))
#' plot(sim_zones_pu_raster, axes = FALSE)
#'
#' # plot feature data for multiple management zones
#' plot_names <- paste0(
#'   "Species ",
#'    rep(
#'      seq_len(number_of_zones(sim_zones_features)),
#'      number_of_features(sim_zones_features)
#'    ),
#'    " (zone ",
#'    rep(
#'      seq_len(number_of_features(sim_zones_features)),
#'      each = number_of_zones(sim_zones_features)
#'    ),
#'    ")"
#' )
#' plot(
#'   terra::rast(as.list(sim_zones_features)),
#'    main = plot_names, axes = FALSE
#' )
#' }
#' @name sim_data
NULL

#' @rdname sim_data
#' @export
get_sim_pu_polygons <- function() {
  sf::st_set_crs(
    sf::read_sf(
      system.file("extdata", "sim_pu_polygons.gpkg", package = "prioritizr")
    ),
    na_crs
  )
}

#' @rdname sim_data
#' @export
get_sim_zones_pu_polygons <- function() {
  sf::st_set_crs(
    sf::read_sf(
      system.file(
        "extdata", "sim_zones_pu_polygons.gpkg", package = "prioritizr"
      )
    ),
    na_crs
  )
}

#' @rdname sim_data
#' @export
get_sim_pu_lines <- function() {
  sf::st_set_crs(
    sf::read_sf(
      system.file("extdata", "sim_pu_lines.gpkg", package = "prioritizr")
    ),
    na_crs
  )
}

#' @rdname sim_data
#' @export
get_sim_pu_points <- function() {
  sf::st_set_crs(
    sf::read_sf(
      system.file("extdata", "sim_pu_points.gpkg", package = "prioritizr")
    ),
    na_crs
  )
}

#' @rdname sim_data
#' @export
get_sim_pu_raster <- function() {
  x <- terra::rast(
    system.file("extdata", "sim_pu_raster.tif", package = "prioritizr")
  )
  terra::crs(x) <- na_crs
  x
}

#' @rdname sim_data
#' @export
get_sim_locked_in_raster <- function() {
  x <- terra::rast(
    system.file("extdata", "sim_locked_in_raster.tif", package = "prioritizr")
  )
  terra::crs(x) <- na_crs
  x
}

#' @rdname sim_data
#' @export
get_sim_locked_out_raster <- function() {
  x <- terra::rast(
    system.file("extdata", "sim_locked_out_raster.tif", package = "prioritizr")
  )
  terra::crs(x) <- na_crs
  x
}

#' @rdname sim_data
#' @export
get_sim_zones_pu_raster <- function() {
  x <- terra::rast(
    system.file("extdata", "sim_zones_pu_raster.tif", package = "prioritizr")
  )
  terra::crs(x) <- na_crs
  names(x) <- paste0("zone_", seq_len(terra::nlyr(x)))
  x
}

#' @rdname sim_data
#' @export
get_sim_features <- function() {
  x <- terra::rast(
   system.file("extdata", "sim_features.tif", package = "prioritizr")
  )
  terra::crs(x) <- na_crs
  names(x) <- paste0("feature_", seq_len(terra::nlyr(x)))
  x
}

#' @rdname sim_data
#' @export
get_sim_zones_features <- function() {
  x <- zones(
    terra::rast(
       system.file(
         "extdata", "sim_zones_features_1.tif", package = "prioritizr"
       )
    ),
    terra::rast(
       system.file(
         "extdata", "sim_zones_features_2.tif", package = "prioritizr"
       )
     ),
    terra::rast(
       system.file(
         "extdata", "sim_zones_features_3.tif", package = "prioritizr"
       )
     ),
    zone_names = c("zone_1", "zone_2", "zone_3"),
    feature_names = c(
      "feature_1", "feature_2", "feature_3", "feature_4", "feature_5"
    )
  )
  for (i in seq_along(x)) terra::crs(x[[i]]) <- na_crs
  x
}

#' @rdname sim_data
#' @export
get_sim_phylogeny <- function() {
  ape::read.nexus(
   system.file("extdata", "sim_phylogeny.txt", package = "prioritizr")
  )
}
