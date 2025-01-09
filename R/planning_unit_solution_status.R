#' @include internal.R
NULL

#' Planning unit solution status
#'
#' Extract planning unit solution status values.
#'
#' @param x [problem()] object.
#'
#' @param solution object with solution data.
#'
#' @param ... not used.
#'
#' @param call Caller environment.
#'
#' @return A `matrix` object containing solution values for planning units.
#'
#' @name planning_unit_solution_status
#'
#' @noRd
methods::setGeneric(
  "planning_unit_solution_status",
  signature = methods::signature("x", "solution"),
  function(x, solution, ...) {
    assert_required(x)
    assert_required(solution)
    assert(
      is_conservation_problem(x),
      is_inherits(
        solution,
        c(
          "numeric", "matrix", "data.frame", "Spatial", "sf", "Raster",
          "SpatRaster"
        )
      )
    )
    standardGeneric("planning_unit_solution_status")
  }
)

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution, call = fn_caller_env()) {
    # assert valid arguments
    ## data type
    assert(
      is.numeric(solution),
      is.numeric(x$data$cost),
      is.matrix(x$data$cost),
      call = call,
      msg = c(
        paste(
          "{.arg x} and {.arg solution} must have",
          "the same type of planning unit data."
        ),
        "x" = "{.arg x} has {.cls {class(x$data$cost)}} planning units.",
        "x" = "{.arg solution} has {.cls {class(solution)}} planning units."
      )
    )
    ## dimensionality
    assert(
      number_of_total_units(x) == length(solution),
      number_of_zones(x) == 1,
      call = call
    )
    ## solution values
    assert(
      suppressWarnings(min(solution, na.rm = TRUE) >= 0),
      suppressWarnings(max(solution, na.rm = TRUE) <= 1),
      call = call,
      msg = paste(
        "{.arg solution} must only contain values between",
        "{.val {0}} and {.val {1}}."
      )
    )
    # extract status
    internal_planning_unit_solution_status(
      x, matrix(solution, ncol = 1), call = call
    )
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution, call = fn_caller_env()) {
    # assert valid arguments
    ## data types
    assert(
      is.matrix(solution),
      is.numeric(solution),
      is.matrix(x$data$cost),
      is.numeric(x$data$cost),
      call = call,
      msg = c(
        paste(
          "{.arg x} and {.arg solution} must have",
          "the same type of planning unit data."
        ),
        "x" = "{.arg x} has {.cls {class(x$data$cost)}} planning units.",
        "x" = "{.arg solution} is a {.cls {class(solution)}}."
      )
    )
    ## number of columns
    msg <- ifelse(
      number_of_zones(x) == 1,
      "{.arg solution} must have a single column with solution values.",
      paste(
        "{.arg solution} must have {number_of_zones(x)} column{?s}",
        "(one column for each zone in {.arg x})."
      )
    )
    assert(number_of_zones(x) == ncol(solution), msg = msg, call = call)
    ## number of units
    assert(number_of_total_units(x) == nrow(solution), call = call)
    ## values in the solution
    assert(
      suppressWarnings(min(solution, na.rm = TRUE) >= 0),
      suppressWarnings(max(solution, na.rm = TRUE) <= 1),
      call = call,
      msg = paste(
        "{.arg solution} must only contain values",
        "between {.val {0}} and {.val {1}}."
      )
    )
    # return status
    internal_planning_unit_solution_status(x, solution, call = call)
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution, call = fn_caller_env()) {
    # assert valid arguments
    ## data types
    assert(
      is.data.frame(solution),
      is.data.frame(x$data$cost),
      call = call,
      msg = c(
        paste(
          "{.arg x} and {.arg solution} must have",
          "the same type of planning unit data."
        ),
        "x" = "{.arg x} has {.cls {class(x$data$cost)}} planning units.",
        "x" = "{.arg solution} is a {.cls {class(x)}}."
      )
    )
    ## number of columns
    msg <- ifelse(
      number_of_zones(x) == 1,
      "{.arg solution} must have a single column with solution values.",
      paste(
        "{.arg solution} must have {number_of_zones(x)} column{?s}",
        "(one column for each zone in {.arg x})."
      )
    )
    assert(number_of_zones(x) == ncol(solution), msg = msg, call = call)
    ## number of units
    assert(number_of_total_units(x) == nrow(solution), call = call)
    ## solution values
    assert(
      is.numeric(unlist(solution)),
      call = call,
      msg = paste(
        "{.arg solution} must only contain columns with numeric values."
      )
    )
    assert(
      suppressWarnings(min(unlist(solution), na.rm = TRUE) >= 0),
      suppressWarnings(max(unlist(solution), na.rm = TRUE) <= 1),
      call = call,
      msg = paste(
        "{.arg solution} must only contain columns with",
        "values between {.val {0}} and {.val {1}}."
      )
    )
    # return status
    internal_planning_unit_solution_status(x, as.matrix(solution), call = call)
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution, call = fn_caller_env()) {
    # assert valid arguments
    cli_warning(sp_pkg_deprecation_notice)
    ## data types
    assert(
      is_inherits(
        solution,
        c(
          "SpatialPointsDataFrame", "SpatialLinesDataFrame",
          "SpatialPolygonsDataFrame"
        )
      ),
      call = call,
      msg = c(
        paste(
          "{.arg x} and {.arg solution} must have",
          "the same type of planning unit data."
        ),
        "x" = "{.arg x} has {.cls {class(x$data$cost)}} planning units.",
        "x" = "{.arg solution} is a {.cls {class(x)}}."
      )
    )
    ## number of columns
    msg <- ifelse(
      number_of_zones(x) == 1,
      "{.arg solution} must have a single column with solution values.",
      paste(
        "{.arg solution} must have {number_of_zones(x)} column{?s}",
        "(one column for each zone in {.arg x})."
      )
    )
    assert(number_of_zones(x) == ncol(solution), msg = msg, call = call)
    ## number of units
    assert(number_of_total_units(x) == nrow(solution@data), call = call)
    ## spatial information
    assert(
      is_same_crs(x$data$cost, solution),
      call = call,
      msg = paste(
        "{.arg x} and {.arg solution} must have the same coordinate reference",
        "system."
      )
    )
    ## solution values
    assert(
      is.numeric(unlist(solution@data)),
      call = call,
      msg = c(
        "{.arg solution} must only contain columns with numeric values."
      )
    )
    assert(
      suppressWarnings(min(unlist(solution@data), na.rm = TRUE) >= 0),
      suppressWarnings(max(unlist(solution@data), na.rm = TRUE) <= 1),
      call = call,
      msg = paste(
        "{.arg solution} must only contain columns",
        "with values between {.val {0}} and {.val {1}}."
      )
    )
    # return status
    internal_planning_unit_solution_status(
      x, as.matrix(solution@data), call = call
    )
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution, call = fn_caller_env()) {
    # assert valid arguments
    ## data types
    assert(
      inherits(solution, "sf"),
      inherits(x$data$cost, "sf"),
      call = call,
      msg = c(
        paste(
          "{.arg x} and {.arg solution} must have",
          "the same type of planning unit data."
        ),
        "x" = "{.arg x} has {.cls {class(x$data$cost)}} planning units.",
        "x" = "{.arg solution} is a {.cls {class(x)}}."
      )
    )
    ## number of columns
    msg <- ifelse(
      number_of_zones(x) == 1,
      "{.arg solution} must have a single column with solution values.",
      paste(
        "{.arg solution} must have {number_of_zones(x)} column{?s}",
        "(one column for each zone in {.arg x})."
      )
    )
    assert(
      number_of_zones(x) == ncol(sf::st_drop_geometry(solution)),
      msg = msg,
      call = call
    )
    ## number of units
    assert(number_of_total_units(x) == nrow(solution), call = call)
    ## spatial information
    assert(
      is_same_crs(x$data$cost, solution),
      msg = paste(
        "{.arg x} and {.arg solution} must have the same coordinate reference",
        "system."
      ),
      call = call
    )
    ## solution values
    assert(
      is.numeric(unlist(sf::st_drop_geometry(solution))),
      call = call,
      msg = "{.arg solution} must only contain columns with numeric values."
    )
    assert(
      suppressWarnings(
        min(unlist(sf::st_drop_geometry(solution)), na.rm = TRUE) >= 0
      ),
      suppressWarnings(
        max(unlist(sf::st_drop_geometry(solution)), na.rm = TRUE) <= 1
      ),
      call = call,
      msg = paste(
        "{.arg solution} must only contain columns with",
        "values between {.val {0}} and {.val {1}}."
      )
    )
    # perform calculations
    solution <- sf::st_drop_geometry(solution)
    internal_planning_unit_solution_status(x, as.matrix(solution), call = call)
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution, call = fn_caller_env()) {
    # assert valid arguments
    cli_warning(raster_pkg_deprecation_notice)
    ## data types
    assert(
      inherits(solution, "Raster"),
      inherits(x$data$cost, "Raster"),
      call = call,
      msg = c(
        paste(
          "{.arg x} and {.arg solution} must have",
          "the same type of planning unit data."
        ),
        "x" = "{.arg x} has {.cls {class(x$data$cost)}} planning units.",
        "x" = "{.arg solution} is a {.cls {class(x)}}."
      )
    )
    ## dimensionality
    assert(
      number_of_zones(x) == raster::nlayers(solution),
      is_comparable_raster(x$data$cost, solution[[1]]),
      call = call
    )
    ## spatial information
    assert(
      is_same_crs(x$data$cost, solution),
      call = call,
      msg = paste(
        "{.arg x} and {.arg solution} must have the same coordinate reference",
        "system."
      )
    )
    ## values in the solution
    assert(
      min(raster::cellStats(solution, "min")) >= 0,
      max(raster::cellStats(solution, "max")) <= 1,
      call = call,
      msg =
        "{.arg solution} must contain values between {.val {0}} and {.val {1}}."
      )
    # subset planning units with finite cost values
    pos <- unname(x$planning_unit_indices())
    if (raster::nlayers(solution) > 1) {
      pos2 <- unname(raster::Which(max(!is.na(solution)) == 1, cells = TRUE))
    } else {
      pos2 <- unname(raster::Which(!is.na(solution), cells = TRUE))
    }
    assert(
      identical(pos, pos2),
      call = call,
      msg = paste(
        "{.arg solution} must have missing ({.val {NA}})",
        "values in exactly the same planning units as the cost data",
        "for {.arg x}."
      )
    )
    solution <- solution[pos2]
    if (!is.matrix(solution))
      solution <- matrix(solution, ncol = 1)
    assert(
      all(is.na(c(x$planning_unit_costs())) == is.na(c(solution))),
      call = call,
      msg = paste(
        "{.arg solution} must have missing ({.val {NA}})",
        "values in exactly the same planning units as the cost data",
        "for {.arg x}."
      )
    )
    # return status
    solution
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "SpatRaster"),
  function(x, solution, call = fn_caller_env()) {
    # assert valid arguments
    ## data types
    assert(
      inherits(solution, "SpatRaster"),
      is_numeric_values(solution),
      inherits(x$data$cost, "SpatRaster"),
      call = call,
      msg = c(
        paste(
          "{.arg x} and {.arg solution} must have",
          "the same type of planning unit data."
        ),
        "x" = "{.arg x} has {.cls {class(x$data$cost)}} planning units.",
        "x" = "{.arg solution} is a {.cls {class(x)}}."
      )
    )
    ## dimensionality
    assert(
      number_of_zones(x) == terra::nlyr(solution),
      is_comparable_raster(x$data$cost, solution[[1]]),
      call = call
    )
    ## spatial information
    assert(
      is_same_crs(x$data$cost, solution),
      call = call,
      msg = paste(
        "{.arg x} and {.arg solution} must have the same coordinate reference",
        "system."
      )
    )
    ## values in the solution
    assert(
      min(terra::global(solution, "min", na.rm = TRUE)[[1]]) >= 0,
      max(terra::global(solution, "max", na.rm = TRUE)[[1]]) <= 1,
      call = call,
      msg = paste(
        "{.arg solution} must only contain",
        "values between {.val {0}} and {.val {1}}."
      )
    )
    # subset planning units with finite cost values
    pos <- unname(x$planning_unit_indices())
    pos2 <- unname(terra::cells(terra::allNA(solution), 0)[[1]])
    assert(
      identical(pos, pos2),
      call = call,
      msg = paste(
        "{.arg solution} must have missing ({.val {NA}})",
        "values in exactly the same planning units as the cost data",
        "for {.arg x}."
      )
    )
    solution <- as.matrix(solution[pos2])
    assert(
      all(is.na(c(x$planning_unit_costs())) == is.na(c(solution))),
      call = call,
      msg = paste(
        "{.arg solution} must have missing ({.val {NA}})",
        "values in exactly the same planning units as the cost data",
        "for {.arg x}."
      )
    )
    # return status
    solution
})

#' Internal helper function
#'
#' This helper function is used to extract solution status values
#' from a matrix containing all total units in the problem.
#' It is used when the planning units are in
#' `numeric`, `matrix`, `data.frame`, `Spatial`, [terra::rast()] or
#' [sf::st_sf()] format.
#'
#' @inheritParams planning_unit_solution_status
#'
#' @noRd
internal_planning_unit_solution_status <- function(x, solution,
                                                   call = fn_caller_env()) {

  # assert valid arguments
  assert(is_conservation_problem(x), call = call)
  assert(
    is.matrix(solution),
    .internal = TRUE,
    msg = "Failed to extract solution status data."
  )
  # subset planning units from total units
  pos <- unname(x$planning_unit_indices())
  pos2 <- unname(which(rowSums(is.na(solution)) != ncol(solution)))
  assert(
    identical(pos, pos2),
    call = call,
    msg = paste(
      "{.arg solution} must have missing ({.val {NA}})",
      "values in exactly the same planning units as the cost data",
      "for {.arg x}."
    )
  )
  out <- solution[pos, , drop = FALSE]
  assert(
    all(is.na(c(x$planning_unit_costs())) == is.na(c(out))),
    call = call,
    msg = paste(
      "{.arg solution} must have missing ({.val {NA}})",
      "values in exactly the same planning units as the cost data",
      "for {.arg x}."
    )
  )
  # force removal of rownames
  rownames(out) <- NULL
  # return result
  out
}

#' Solution format documentation
#'
#' @param x `character` name of argument.
#'
#' @noRd
solution_format_documentation <- function(x) {
  assert(assertthat::is.string(x))
  paste0("
\\describe{

\\item{`x` has `numeric` planning units}{The argument to `", x , "` must be a
  `numeric` vector with each element corresponding to a different planning
  unit. It should have the same number of planning units as those
  in the argument to `x`. Additionally, any planning units missing
  cost (`NA`) values should also have missing (`NA`) values in the
  argument to `", x , "`.
}

\\item{`x` has `matrix` planning units}{The argument to `", x , "` must be a
  `matrix` vector with each row corresponding to a different planning
  unit, and each column correspond to a different management zone.
  It should have the same number of planning units and zones
  as those in the argument to `x`. Additionally, any planning units
  missing cost (`NA`) values for a particular zone should also have a
  missing (`NA`) values in the argument to `", x , "`.
}

\\item{`x` has [terra::rast()] planning units}{The argument to `", x , "`
  be a [terra::rast()] object where different grid cells (pixels) correspond
  to different planning units and layers correspond to
  a different management zones. It should have the same dimensionality
  (rows, columns, layers), resolution, extent, and coordinate reference
  system as the planning units in the argument to `x`. Additionally,
  any planning units missing cost (`NA`) values for a particular zone
  should also have missing (`NA`)  values in the argument to `", x , "`.
}

\\item{`x` has `data.frame` planning units}{The argument to `", x , "` must
  be a `data.frame` with each column corresponding to a different zone,
  each row corresponding to a different planning unit, and cell values
  corresponding to the solution value. This means that if a `data.frame`
  object containing the solution also contains additional columns, then
  these columns will need to be subsetted prior to using this function
  (see below for example with [sf::sf()] data).
  Additionally, any planning units missing cost
  (`NA`) values for a particular zone should also have missing (`NA`)
  values in the argument to `", x , "`.
}

\\item{`x` has [sf::sf()] planning units}{The argument to `", x , "` must be
  a [sf::sf()] object with each column corresponding to a different
  zone, each row corresponding to a different planning unit, and cell values
  corresponding to the solution value. This means that if the
  [sf::sf()] object containing the solution also contains additional
  columns, then these columns will need to be subsetted prior to using this
  function (see below for example).
  Additionally, the argument to `", x , "` must also have the same
  coordinate reference system as the planning unit data.
  Furthermore, any planning units missing cost
  (`NA`) values for a particular zone should also have missing (`NA`)
  values in the argument to `", x , "`.
}
}
")
}
