#' @include internal.R problem.R
NULL

#' @name problem
#' @usage \S4method{problem}{Raster,Raster}(x, features, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Raster", features = "Raster"),
  function(x, features, run_checks = TRUE, ...) {
    assertthat::assert_that(
      inherits(x, "Raster"),
      raster::nlayers(x) == 1,
      no_extra_arguments(...)
    )
    .Deprecated(msg = raster_pkg_deprecation_notice)
    problem(
      x,
      zones(features, zone_names = names(x), feature_names = names(features)),
      run_checks = run_checks,
      ...
    )
})

#' @name problem
#' @usage \S4method{problem}{Raster,ZonesRaster}(x, features, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Raster", features = "ZonesRaster"),
  function(x, features, run_checks = TRUE, ...) {
    .Deprecated(msg = raster_package_deprecation_notice)
    # create problem based on supported class to perform checks and calculations
    p <- problem(
      terra::rast(x),
      as.ZonesSpatRaster(features),
      run_checks = run_checks,
      ...
    )
    # return result with Raster data
    pproto(
      NULL,
      ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(
        cost = x,
        features = features,
        rij_matrix = p$data$rij,
        feature_abundances_in_total_units = p$data$fatu
      )
    )
})

#' @name problem
#' @usage \S4method{problem}{Spatial,Raster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "Raster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    assertthat::assert_that(assertthat::is.string(cost_column))
    problem(
      x,
      zones(
        features, zone_names = cost_column, feature_names = names(features)
      ),
      cost_column = cost_column,
      run_checks = run_checks,
      ...
    )
})

#' @name problem
#' @usage \S4method{problem}{Spatial,ZonesRaster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "ZonesRaster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    .Deprecated(msg = sp_package_deprecation_notice)
    .Deprecated(msg = raster_package_deprecation_notice)
    # assert valid arguments
    assertthat::assert_that(
      is_inherits(
        x,
        c(
          "SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
          "SpatialPointsDataFrame"
        )
      )
    )
    # create problem based on supported class to perform checks and calculations
    p <- problem(
      sf::st_as_sf(x),
      as.ZonesSpatRaster(features),
      run_checks = run_checks,
      ...
    )
    # return result with Raster data
    pproto(
      NULL,
      ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(
        cost = x,
        features = features,
        rij_matrix = p$data$rij,
        feature_abundances_in_total_units = p$data$fatu
      )
    )
})

#' @name problem
#' @usage \S4method{problem}{Spatial,character}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "character"),
  function(x, features, cost_column, ...) {
    assertthat::assert_that(assertthat::is.string(cost_column))
    problem(
      x,
      zones(features, feature_names = features, zone_names = cost_column),
      cost_column = cost_column,
      ...
    )
})

#' @name problem
#' @usage \S4method{problem}{Spatial,ZonesCharacter}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "ZonesCharacter"),
  function(x, features, cost_column, ...) {
    .Deprecated(msg = sp_package_deprecation_notice)
    # assert that arguments are valid
    assertthat::assert_that(
      is_inherits(
        x,
        c(
          "SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
          "SpatialPointsDataFrame"
        )
      )
    )
    # create problem based on supported class to perform checks and calculations
    p <- problem(
      sf::st_as_sf(x),
      features,
      run_checks = run_checks,
      ...
    )
    # return result with Raster data
    pproto(
      NULL,
      ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(
        cost = x,
        features = features,
        rij_matrix = p$data$rij,
        feature_abundances_in_total_units = p$data$fatu
      )
    )
})

#' @name problem
#' @usage \S4method{problem}{sf,Raster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "sf", features = "Raster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    assertthat::assert_that(assertthat::is.string(cost_column))
    problem(
      x,
      zones(
        features,
        zone_names = cost_column,
        feature_names = names(features)
      ),
      cost_column = cost_column,
      run_checks = run_checks,
      ...
    )
})

#' @name problem
#' @usage \S4method{problem}{sf,ZonesRaster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "sf", features = "ZonesRaster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    .Deprecated(msg = raster_package_deprecation_notice)
    # create problem based on supported class to perform checks and calculations
    p <- problem(
      x,
      as.ZonesSpatRaster(features),
      run_checks = run_checks,
      ...
    )
    # return result with Raster data
    pproto(
      NULL,
      ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(
        cost = x,
        features = features,
        rij_matrix = p$data$rij,
        feature_abundances_in_total_units = p$data$fatu
      )
    )
})
