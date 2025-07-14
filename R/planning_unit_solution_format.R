#' @include internal.R
NULL

#' Planning unit solution format
#'
#' Format planning unit solution status data into a planning unit dataset.
#'
#' @param x [problem()] object.
#'
#' @param status `matrix` or `list` of `matrix` objects with solution
#'  status values.
#'
#' @param data planning unit data. These data may be an a
#' `numeric` vector, `matrix`, `data.frame`, `Spatial`, `sf`, `Raster`,
#' or `SpatRaster` object. If `missing`, then these data are automatically
#' extracted from `x`. In most cases, this argument should not be supplied
#' so that the planning unit data can be handled automatically.
#'
#' @param prefix `character` prefix for naming different columns or layers in
#' the output.
#' Defaults to `NULL` such that no prefix is applied.
#'
#' @param append `logical` indicating if the columns in the planning unit
#' dataset should be appended to the output? Note that this only has an effect
#' if the planning units in `x` are a `sf`, `Spatial`, or `data.frame` object.
#' Defaults to `FALSE`.
#'
#' @param ... not used.
#'
#' @param call Caller environment.
#'
#' @return
#' If `status` is a `matrix` vector, then an object of the same class as the
#' planning units in `x` is returned (i.e., a
#' `numeric` vector, `matrix`, `data.frame`, `Spatial`, `sf`, `Raster`,
#' or `SpatRaster` object). Otherwise, if `status` is a `list`, then a
#' list of objects of the same class as the planning unit is `x` is returned.
#'
#' @name planning_unit_solution_format
#'
#' @noRd
methods::setGeneric(
  "planning_unit_solution_format",
  signature = methods::signature("x", "status", "data"),
  function(x, status, data, ...) {
    assert_required(x)
    assert_required(status)
    assert(
      is_conservation_problem(x),
      is_inherits(status, c("matrix", "list"))
    )
    if (is.list(status)) {
      assert(all_elements_inherit(status, "matrix"))
    }
    standardGeneric("planning_unit_solution_format")
  }
)

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "matrix", "missing"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    planning_unit_solution_format(
      x, status, data = x$data$cost,
      prefix = prefix, append = append, call = call
    )
  }
)

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "list", "missing"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    # create a prefix for each element of status
    if (is.null(prefix)) {
      prefix <- list(NULL)[rep(1, length(status))]
    }
    if (length(prefix) == 1) {
      prefix <- list(prefix)[rep(1, length(status))]
    }
    # assert prefix specified for each status
    assert(
      identical(length(prefix), length(status)),
      call = call,
      .internal = TRUE,
      msg = paste0(
        "{.arg prefix} should contain a single `character` value or a ",
        "value for each element of status."
      )
    )
    # process data
    if (inherits(x$data$cost, c("sf", "Spatial", "data.frame"))) {
      ### if sf/Spatial/data.frame, then process separately in matrix format to
      ### reduce memory requirements
      out <- lapply(seq_along(status), function(i) {
        planning_unit_solution_format(
          x, status[[i]], data = data.frame(1),
          prefix = prefix[[i]], append = FALSE, call = call
        )
      })
      ### cbind the columns together
      out <- do.call(cbind, out)
      ### if append, then add in additional columns from the planning unit
      ### dataset
      if (isTRUE(append)) {
        if (inherits(x$data$cost, "sf")) {
          out <- cbind(sf::st_drop_geometry(x$data$cost), out)
        } else if (inherits(x$data$cost, "Spatial")) {
          out <- cbind(x$data$cost@data, out)
        } else if (inherits(x$data$cost, "data.frame")) {
          out <- cbind(x$data$cost, out)
        }
      }
      ### perform additional processing needed for sf/Spatial datasets
      if (inherits(x$data$cost, "sf")) {
        out <- tibble::as_tibble(out)
        out$geometry <- sf::st_geometry(x$data$cost)
        out <- sf::st_sf(out, crs = sf::st_crs(x$data$cost))
      } else if (inherits(x$data$cost, "Spatial")) {
        d <- out
        rownames(d) <- rownames(x$data$cost)
        out <- x$data$cost
        out@data <- d
      } else {
        out <- tibble::as_tibble(out)
      }
    } else {
      ### otherwise, then process separately in planning unit data format
      out <- lapply(seq_along(status), function(i) {
        planning_unit_solution_format(
          x, status[[i]], append = append, call = call
        )
      })
      ### set names
      names(out) <- prefix
    }
    # return result
    out
  }
)

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "matrix", "numeric"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    # assert valid arguments
    assert(
      identical(ncol(status), x$number_of_zones()),
      identical(nrow(status), x$number_of_planning_units()),
      .internal = TRUE
    )
    # initialize matrices
    out <- c(NA_real_, x$number_of_total_units())
    # overwrite status for planning units with missing cost values
    status[is.na(x$planning_unit_costs())] <- NA_real_
    # prepare output
    out[x$planning_unit_indices()] <- status
    # return output
    out
})

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "matrix", "matrix"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    # assert valid arguments
    assert(
      identical(ncol(status), x$number_of_zones()),
      identical(nrow(status), x$number_of_planning_units()),
      .internal = TRUE
    )
    # initialize matrices
    out <- matrix(
      NA_real_,
      nrow = x$number_of_total_units(),
      ncol = x$number_of_zones()
    )
    # overwrite status for planning units with missing cost values
    status[is.na(x$planning_unit_costs())] <- NA_real_
    # assign status values
    out[x$planning_unit_indices(), ] <- status
    # assign names
    if (is.null(prefix)) {missing
      colnames(out) <- x$zone_names()
    } else if (identical(ncol(out), 1L)) {
      colnames(out) <- prefix
    } else {
      colnames(out) <- paste0(prefix, "_", x$zone_names())
    }
    # return output
    out
})

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "matrix", "data.frame"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    # assert valid arguments
    assert(
      identical(
        length(status),
        x$number_of_planning_units() * x$number_of_zones()
      ),
      .internal = TRUE,
      msg = c(
        paste0(
          "{.arg status} must have ",
          x$number_of_planning_units() * x$number_of_zones(),
          " elements."
        ),
        "x" = "{.arg status} has {length(status)} element{?s}."
      )
    )
    # prepare data in matrix format
    out <- planning_unit_solution_format(
      x, status, matrix(1), prefix = prefix, append = append, call = call
    )
    # if needed, append columns from the planning unit dataset
    out <- as.data.frame(out)
    if (isTRUE(append)) {
      out <- cbind(x$data$cost, out)
    }
    # return result
    tibble::as_tibble(out)
})

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "matrix", "Spatial"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    # assert valid arguments
    assert(
      identical(
        length(status),
        x$number_of_planning_units() * x$number_of_zones()
      ),
      .internal = TRUE,
      msg = c(
        paste0(
          "{.arg status} must have ",
          x$number_of_planning_units() * x$number_of_zones(),
          " elements."
        ),
        "x" = "{.arg status} has {length(status)} element{?s}."
      )
    )
    # extract data
    data <- x$data$cost
    # prepare data in matrix format
    out <- planning_unit_solution_format(
      x, status, matrix(1), prefix = prefix, append = append, call = call
    )
    # if needed, append columns from the planning unit dataset
    out <- as.data.frame(out)
    if (isTRUE(append)) {
      out <- cbind(data@data, out)
    }
    # return result
    rownames(out) <- rownames(data@data)
    data@data <- out
    data
})

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "matrix", "sf"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    # assert valid arguments
    assert(
      identical(
        length(status),
        x$number_of_planning_units() * x$number_of_zones()
      ),
      .internal = TRUE,
      msg = c(
        paste0(
          "{.arg status} must have ",
          x$number_of_planning_units() * x$number_of_zones(),
          " elements."
        ),
        "x" = "{.arg status} has {length(status)} element{?s}."
      )
    )
    # prepare data in matrix format
    out <- planning_unit_solution_format(
      x, status, matrix(1), prefix = prefix, append = append, call = call
    )
    # if needed, append columns from the planning unit dataset
    out <- as.data.frame(out)
    if (isTRUE(append)) {
      out <- cbind(sf::st_drop_geometry(x$data$cost), out)
    }
    # convert to sf
    out <- tibble::as_tibble(out)
    out$geometry <- sf::st_geometry(x$data$cost)
    sf::st_sf(out, crs = sf::st_crs(x$data$cost))
})

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "matrix", "Raster"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    # assert valid arguments
    assert(
      identical(
        length(status),
        x$number_of_planning_units() * x$number_of_zones()
      ),
      .internal = TRUE,
      msg = c(
        paste0(
          "{.arg status} must have ",
          x$number_of_planning_units() * x$number_of_zones(),
          " elements."
        ),
        "x" = "{.arg status} has {length(status)} element{?s}."
      )
    )
    # extract data
    idx <- x$planning_unit_indices()
    data <- x$data$cost
    # prepare output
    out <- data
    for (i in seq_len(raster::nlayers(out))) {
      out[[i]][idx] <- status[, i]
      out[[i]][raster::Which(is.na(data[[i]]), cells = TRUE)] <- NA_real_
    }
    if (is.null(prefix)) {
      names(out) <- x$zone_names()
    } else if (isTRUE(raster::nlayers(out) == 1L)) {
      names(out) <- prefix
    } else {
      names(out) <- paste0(prefix, "_", x$zone_names())
    }
    # return result
    out
})

methods::setMethod(
  "planning_unit_solution_format",
  methods::signature("ConservationProblem", "matrix", "SpatRaster"),
  function(
    x, status, data, prefix = NULL, append = FALSE, call = fn_caller_env()
  ) {
    # assert valid arguments
    assert(
      identical(
        length(status),
        x$number_of_planning_units() * x$number_of_zones()
      ),
      .internal = TRUE,
      msg = c(
        paste0(
          "{.arg status} must have ",
          x$number_of_planning_units() * x$number_of_zones(),
          " elements."
        ),
        "x" = "{.arg status} has {length(status)} element{?s}."
      )
    )
    # extract data
    idx <- x$planning_unit_indices()
    data <- x$data$cost
    # prepare result
    out <- terra::as.list(data)
    for (i in seq_along(out)) {
      out[[i]][idx] <- status[, i]
      out[[i]] <- terra::mask(out[[i]], data[[i]])
    }
    out <- terra::rast(out)
    if (is.null(prefix)) {
      names(out) <- x$zone_names()
    } else if (isTRUE(terra::nlyr(out) == 1)) {
      names(out) <- prefix
    } else {
      names(out) <- paste0(prefix, "_", x$zone_names())
    }
    # return result
    out
})
