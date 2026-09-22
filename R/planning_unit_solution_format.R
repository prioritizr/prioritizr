#' @include internal.R
NULL

#' Planning unit solution format
#'
#' Format planning unit solution status data into a planning unit dataset.
#'
#' @param x [problem()] object.
#'
#' @param status `matrix` or `list` of `matrix` objects with solution
#' status values.
#'
#' @param data planning unit data. These data may be an a
#' `numeric` vector, `matrix`, `data.frame`, `sf`,
#' or `SpatRaster` object. If `data` is missing, then planning unit data are
#' automatically extracted from `x`.
#' In most cases, `data` should not be supplied
#' so that the planning unit data can be handled automatically.
#'
#' @param prefix `character` prefix for naming different columns or layers in
#' the output.
#' Defaults to `NULL` such that no prefix is applied.
#'
#' @param append `logical` indicating if the columns in the planning unit
#' dataset should be appended to the output? Note that this only has an effect
#' if the planning units in `x` are a `sf` or `data.frame` object.
#' Defaults to `FALSE`.
#'
#' @param ... not used.
#'
#' @param call Caller environment.
#'
#' @return
#' If `status` is a `matrix` vector, then an object of the same class as the
#' planning units in `x` is returned (i.e., a
#' `numeric` vector, `matrix`, `data.frame`, `sf`,
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
    if (inherits(x$data$cost, c("sf", "data.frame"))) {
      ### if sf/data.frame, then process separately in matrix format to
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
        } else if (inherits(x$data$cost, "data.frame")) {
          out <- cbind(x$data$cost, out)
        }
      }
      ### perform additional processing needed for sf or data.frame datasets
      if (inherits(x$data$cost, "sf")) {
        out <- tibble::as_tibble(out)
        out$geometry <- sf::st_geometry(x$data$cost)
        out <- sf::st_sf(out, crs = sf::st_crs(x$data$cost))
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
    # prepare result
    out <- terra::rast(x$data$cost)
    for (i in seq_len(ncol(status))) {
      out[[i]][idx] <- status[, i]
    }
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
