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
    .Deprecated(msg = raster_pkg_deprecation_notice)
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "Raster"),
      inherits(features, "ZonesRaster"),
      assertthat::is.flag(run_checks),
      no_extra_arguments(...),
      raster::nlayers(x) > 0,
      number_of_features(features) > 0,
      raster::nlayers(x) == number_of_zones(features),
      is_comparable_raster(x, features)
    )
    if (run_checks) {
      assertthat::assert_that(
        any_nonNA(x),
        all_positive(x)
      )
      verify_that(all_positive(x))
      verify_that(any_nonzero(x))
      verify_that(all_positive(features))
      verify_that(any_nonzero(features))
    }
    # convert x to RasterLayer if has only one layer
    if (
      inherits(x, c("RasterStack", "RasterBrick")) &&
      raster::nlayers(x) == 1
    ) {
      x <- x[[1]]
    }
    # create rij matrix
    rij <- suppressWarnings(
      lapply(
        as.list(features),
        function(f) rij_matrix(x, `names<-`(f, feature_names(features)))
      )
    )
    names(rij) <- zone_names(features)
    # calculate feature abundances in total units
    fatu <- vapply(
      features, raster::cellStats, numeric(number_of_features(features)), "sum"
    )
    if (!is.matrix(fatu)) {
      fatu <- matrix(
        fatu, ncol = number_of_zones(features),
        nrow = number_of_features(features)
      )
    }
    colnames(fatu) <- zone_names(features)
    rownames(fatu) <- feature_names(features)
    # create ConservationProblem object
    pproto(
      NULL,
      ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(
        cost = x,
        features = features,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
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
    .Deprecated(msg = sp_pkg_deprecation_notice)
    .Deprecated(msg = raster_pkg_deprecation_notice)
    # assert valid arguments
    # assert that arguments are valid
    assertthat::assert_that(
      is_inherits(
        x,
        c(
          "SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
          "SpatialPointsDataFrame"
        )
      ),
      no_extra_arguments(...),
      nrow(x) > 0,
      is.character(cost_column),
      assertthat::noNA(cost_column),
      all_match_of(cost_column, names(x)),
      length(cost_column) == number_of_zones(features),
      all_columns_inherit(x[, cost_column], "numeric"),
      assertthat::is.flag(run_checks)
    )
    # further validation checks
    assertthat::assert_that(
      all_columns_any_finite(x[, cost_column]),
      is_same_crs(x, features),
      is_spatial_extents_overlap(x, features)
    )
    verify_that(all_positive(x[, cost_column]))
    verify_that(any_nonzero(x[, cost_column]))
    if (run_checks) {
      verify_that(all_positive(features))
      verify_that(any_nonzero(features))
    }
    # compute rij matrix including non-planning unit cells
    rij <- suppressWarnings(rij_matrix(x, raster::stack(as.list(features))))
    rij <- lapply(seq_len(number_of_zones(features)), function(i) {
      m <- rij[
        ((i - 1) * number_of_features(features)) +
          seq_len(number_of_features(features)), ,
        drop = FALSE]
      rownames(m) <- feature_names(features)
      m
    })
    # calculate feature abundances in total units
    fatu <- vapply(
      rij, Matrix::rowSums, numeric(number_of_features(features)), na.rm = TRUE
    )
    if (!is.matrix(fatu)) {
      fatu <- matrix(
        fatu, nrow = number_of_features(features),
        ncol = number_of_zones(features)
      )
    }
    rownames(fatu) <- feature_names(features)
    colnames(fatu) <- zone_names(features)
    # create rij matrix
    pos <- which(
      rowSums(!is.na(as.matrix(x@data[, cost_column, drop = FALSE]))) > 0
    )
    rij <- lapply(rij, function(x) x[, pos, drop = FALSE])
    names(rij) <- zone_names(features)
    # create ConservationProblem object
    pproto(
      NULL,
      ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
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
    .Deprecated(msg = sp_pkg_deprecation_notice)
    # assert that arguments are valid
    assertthat::assert_that(
      is_inherits(
        x,
        c(
          "SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
          "SpatialPointsDataFrame"
        )
      ),
      inherits(features, "ZonesCharacter"),
      no_extra_arguments(...),
      nrow(x) > 0,
      is.character(cost_column),
      assertthat::noNA(cost_column),
      number_of_zones(features) == length(cost_column),
      all_match_of(cost_column, names(x)),
      all_positive(x[, cost_column]),
      all_columns_any_finite(x[, cost_column])
    )
    assertthat::assert_that(
      all_match_of(unlist(as.list(features)), names(x)),
      msg = paste(
        "argument to features contains column names that are",
        "not present in the argument to x"
      )
    )
    verify_that(all_positive(x[, cost_column]))
    verify_that(any_nonzero(x[, cost_column]))
    verify_that(all_positive(x[, unlist(features)]))
    verify_that(any_nonzero(x[, unlist(features)]))
    # create rij matrix
    pos <- which(
      rowSums(!is.na(as.matrix(x@data[, cost_column, drop = FALSE]))) > 0
    )
    rij <- lapply(features, function(z) {
      r <- t(as.matrix(x@data[pos, z, drop = FALSE]))
      r[is.na(r)] <- 0
      rownames(r) <- feature_names(features)
      methods::as(r, "sparseMatrix")
    })
    names(rij) <- zone_names(features)
    # calculate feature abundances in total units
    fatu <- colSums(
      x@data[, unlist(as.list(features)), drop = FALSE],
      na.rm = TRUE
    )
    fatu <- matrix(
      fatu,
      ncol = number_of_zones(features),
      nrow = number_of_features(features),
      dimnames = list(feature_names(features), zone_names(features))
    )
    # create ConservationProblem object
    pproto(
      NULL,
      ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
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
    .Deprecated(msg = raster_pkg_deprecation_notice)
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "sf"),
      no_extra_arguments(...),
      nrow(x) > 0,
      is.character(cost_column),
      assertthat::noNA(cost_column),
      all_match_of(cost_column, names(x)),
      length(cost_column) == number_of_zones(features),
      assertthat::is.flag(run_checks),
      all_columns_inherit(x[, cost_column], "numeric"),
      all_columns_any_finite(x[, cost_column]),
      is_same_crs(x, features),
      is_spatial_extents_overlap(x, features)
    )
    assertthat::assert_that(
      all(!st_geometry_classes(x) %in% c("GEOMETRYCOLLECTION", "MULTIPOINT"))
    )
    # further validation checks
    verify_that(all_positive(x[, cost_column]))
    verify_that(any_nonzero(x[, cost_column]))
    if (run_checks) {
      all_positive(features)
      any_nonzero(features)
    }
    # compute rij matrix including non-planning unit cells
    rij <- suppressWarnings(rij_matrix(x, raster::stack(as.list(features))))
    rij <- lapply(seq_len(number_of_zones(features)), function(i) {
      m <- rij[
        ((i - 1) * number_of_features(features)) +
          seq_len(number_of_features(features)), ,
        drop = FALSE]
      rownames(m) <- feature_names(features)
      m
    })
    # calculate feature abundances in total units
    fatu <- vapply(
      rij, Matrix::rowSums, numeric(number_of_features(features)), na.rm = TRUE
    )
    if (!is.matrix(fatu)) {
      fatu <- matrix(
        fatu, nrow = number_of_features(features),
        ncol = number_of_zones(features)
      )
    }
    rownames(fatu) <- feature_names(features)
    colnames(fatu) <- zone_names(features)
    # create rij matrix
    pos <- which(
      rowSums(!is.na(as.matrix(
        sf::st_drop_geometry(x)[, cost_column, drop = FALSE]
      ))) > 0
    )
    rij <- lapply(rij, function(x) x[, pos, drop = FALSE])
    names(rij) <- zone_names(features)
    # create ConservationProblem object
    pproto(
      NULL,
      ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
})
