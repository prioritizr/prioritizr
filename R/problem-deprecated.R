#' @include internal.R problem.R
NULL

#' @name problem
#' @usage \S4method{problem}{Raster,Raster}(x, features, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Raster", features = "Raster"),
  function(x, features, run_checks = TRUE, ...) {
    assert(
      inherits(x, "Raster"),
      raster::nlayers(x) == 1
    )
    assert_dots_empty()
    problem(
      x,
      suppressWarnings(
        zones(features, zone_names = names(x), feature_names = names(features))
      ),
      run_checks = run_checks,
      ...
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{Raster,ZonesRaster}(x, features, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Raster", features = "ZonesRaster"),
  function(x, features, run_checks = TRUE, ...) {
    cli_warning(raster_pkg_deprecation_notice)
    # assert that arguments are valid
    assert(
      inherits(x, "Raster"),
      inherits(features, "ZonesRaster"),
      assertthat::is.flag(run_checks),
      raster::nlayers(x) > 0,
      number_of_features(features) > 0,
      raster::nlayers(x) == number_of_zones(features),
      is_comparable_raster(x, features)
    )
    assert_dots_empty()
    if (run_checks) {
      assert(any_nonNA(x))
      verify(
        all_positive(x),
        any_nonzero(x),
        all_positive(features),
        any_nonzero(features),
        any_nonNA(features)
      )
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
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{Spatial,Raster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "Raster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    assert_required(cost_column)
    assert(assertthat::is.string(cost_column))
    problem(
      x,
      suppressWarnings(
        zones(
          features, zone_names = cost_column, feature_names = names(features)
        )
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
    cli_warning(sp_pkg_deprecation_notice)
    cli_warning(raster_pkg_deprecation_notice)
    # assert valid arguments
    # assert that arguments are valid
    assert_required(cost_column)
    assert(
      is_inherits(
        x,
        c(
          "SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
          "SpatialPointsDataFrame"
        )
      ),
      nrow(x) > 0,
      is.character(cost_column),
      assertthat::noNA(cost_column),
      all_match_of(cost_column, names(x)),
      length(cost_column) == number_of_zones(features),
      all_columns_inherit(x[, cost_column], "numeric"),
      assertthat::is.flag(run_checks)
    )
    assert_dots_empty()
    # further validation checks
    assert(
      all_columns_any_finite(x[, cost_column]),
      is_same_crs(x, features),
      is_spatial_extents_overlap(x, features)
    )
    verify(
      all_positive(x[, cost_column]),
      any_nonzero(x[, cost_column])
    )
    if (run_checks) {
      verify(
        all_positive(features),
        any_nonzero(features),
        any_nonNA(features)
      )
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
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{Spatial,character}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "character"),
  function(x, features, cost_column, ...) {
    assert_required(cost_column)
    assert(assertthat::is.string(cost_column))
    problem(
      x,
      zones(features, feature_names = features, zone_names = cost_column),
      cost_column = cost_column,
      ...
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{Spatial,ZonesCharacter}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "ZonesCharacter"),
  function(x, features, cost_column, ...) {
    cli_warning(sp_pkg_deprecation_notice)
    # assert that arguments are valid
    assert_required(cost_column)
    assert(
      is_inherits(
        x,
        c(
          "SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
          "SpatialPointsDataFrame"
        )
      ),
      inherits(features, "ZonesCharacter"),
      nrow(x) > 0,
      is.character(cost_column),
      assertthat::noNA(cost_column),
      number_of_zones(features) == length(cost_column),
      all_match_of(cost_column, names(x)),
      all_columns_any_finite(x[, cost_column])
    )
    assert_dots_empty()
    assert(
      all_match_of(unlist(as.list(features)), names(x)),
      msg = paste(
        "{.arg features} contains column names that are",
        "not present in {.arg x}"
      )
    )
    assert(
      all_columns_inherit(
        x[, unlist(as.list(features)), drop = FALSE], c("integer", "numeric")
      ),
      msg = c(
        "{.arg features} must refer to {.cls numeric} columns of {.arg x}."
      )
    )
    verify(
      all_positive(x[, cost_column]),
      any_nonzero(x[, cost_column]),
      all_positive(x[, unlist(features)]),
      any_nonzero(x[, unlist(features)]),
      all_columns_any_finite(x[, unlist(features)])
    )
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
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{sf,Raster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "sf", features = "Raster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    assert_required(cost_column)
    assert(assertthat::is.string(cost_column))
    problem(
      x,
      suppressWarnings(
        zones(
          features,
          zone_names = cost_column,
          feature_names = names(features)
        )
      ),
      cost_column = cost_column,
      run_checks = run_checks,
      ...
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{sf,ZonesRaster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "sf", features = "ZonesRaster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    cli_warning(raster_pkg_deprecation_notice)
    # assert that arguments are valid
    assert_required(cost_column)
    assert(
      inherits(x, "sf"),
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
    assert_dots_empty()
    assert(
      all(!st_geometry_classes(x) %in% c("GEOMETRYCOLLECTION", "MULTIPOINT"))
    )
    # further validation checks
    verify(
      all_positive(x[, cost_column]),
      any_nonzero(x[, cost_column])
    )
    if (run_checks) {
      verify(
        all_positive(features),
        any_nonzero(features),
        any_nonNA(features)
      )
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
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)
