#' @include internal.R
NULL

#' Planning unit solution status
#'
#' Extract planning unit solution status values.
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object.
#'
#' @param solution object with solution data.
#'
#' @return `matrix` object containing solution values for planning units.
#'
#' @name planning_unit_solution_status
#'
#' @noRd
methods::setGeneric(
  "planning_unit_solution_status",
  signature = methods::signature("x", "solution"),
  function(x, solution) standardGeneric("planning_unit_solution_status"))

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution) {
    # assert valid arguments
    ## data type
    assertthat::assert_that(
      is.numeric(solution),
      is.numeric(x$data$cost),
      is.matrix(x$data$cost),
      msg = paste(
        "argument to x and solution object have",
        "planning unit data in different formats"))
    ## dimensionality
    assertthat::assert_that(
      number_of_total_units(x) == length(solution),
      number_of_zones(x) == 1)
    ## solution values
    assertthat::assert_that(
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1,
      msg = paste("solution object should only contain",
                  "values between zero and one"))
    # extract status
    internal_planning_unit_solution_status(x, matrix(solution, ncol = 1))
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution) {
    # assert valid arguments
    ## data types
    assertthat::assert_that(
      is.matrix(solution),
      is.numeric(solution),
      is.matrix(x$data$cost),
      is.numeric(x$data$cost),
      msg = paste(
        "argument to x and solution object have",
        "planning unit data in different formats"))
    ## number of columns
    msg <- ifelse(
      number_of_zones(x) == 1,
      paste("solution object should contain a single column",
            "with solution values"),
      paste("solution object should contain a column for each",
            "management zone with solution values"))
    assertthat::assert_that(number_of_zones(x) == ncol(solution), msg = msg)
    ## number of units
    assertthat::assert_that(
      number_of_total_units(x) == nrow(solution))
    ## values in the solution
    assertthat::assert_that(
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1,
      msg = paste("solution object should only contain",
                  "values between zero and one"))
    # return status
    internal_planning_unit_solution_status(x, solution)
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution) {
    # assert valid arguments
    ## data types
    assertthat::assert_that(
      is.data.frame(solution),
      is.data.frame(x$data$cost),
      msg = paste(
        "argument to x and solution object have",
        "planning unit data in different formats"))
    ## number of columns
    msg <- ifelse(
      number_of_zones(x) == 1,
      paste("solution object should contain a single column",
            "with solution values"),
      paste("solution object should contain a column for each",
            "management zone with solution values"))
    assertthat::assert_that(number_of_zones(x) == ncol(solution), msg = msg)
    ## number of units
    assertthat::assert_that(
      number_of_total_units(x) == nrow(solution))
    ## solution values
    assertthat::assert_that(
      is.numeric(unlist(solution)),
      msg = paste("solution object should only contain columns",
                  "with numeric values"))
    assertthat::assert_that(
      min(unlist(solution), na.rm = TRUE) >= 0,
      max(unlist(solution), na.rm = TRUE) <= 1,
      msg = paste("solution object should only contain columns",
                  "with values between zero and one"))
    # return status
    internal_planning_unit_solution_status(x, as.matrix(solution))
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution) {
    # assert valid arguments
    ## data types
    assertthat::assert_that(
      inherits(solution, c("SpatialPointsDataFrame", "SpatialLinesDataFrame",
                           "SpatialPolygonsDataFrame")),
      class(x$data$cost)[1] == class(solution)[1],
      msg = paste(
        "argument to x and solution object have",
        "planning unit data in different formats"))

    ## number of columns
    msg <- ifelse(
      number_of_zones(x) == 1,
      paste("solution object should contain a single column",
            "with solution values"),
      paste("solution object should contain a column for each",
            "management zone with solution values"))
    assertthat::assert_that(number_of_zones(x) == ncol(solution), msg = msg)
    ## number of units
    assertthat::assert_that(
      number_of_total_units(x) == nrow(solution@data))
    ## spatial information
    assertthat::assert_that(
      sf::st_crs(x$data$cost@proj4string) == sf::st_crs(solution@proj4string))
    ## solution values
    assertthat::assert_that(
      is.numeric(unlist(solution@data)),
      msg = paste("solution object should only contain columns",
                  "with numeric values"))
    assertthat::assert_that(
      min(unlist(solution@data), na.rm = TRUE) >= 0,
      max(unlist(solution@data), na.rm = TRUE) <= 1,
      msg = paste("solution object should only contain columns",
                  "with values between zero and one"))
    # return status
    internal_planning_unit_solution_status(x, as.matrix(solution@data))
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution) {
    # assert valid arguments
    ## data types
    assertthat::assert_that(
      inherits(solution, "sf"),
      inherits(x$data$cost, "sf"),
      msg = paste(
        "argument to x and solution object have",
        "planning unit data in different formats"))
    ## number of columns
    msg <- ifelse(
      number_of_zones(x) == 1,
      paste("solution object should contain a single column",
            "with solution values"),
      paste("solution object should contain a column for each",
            "management zone with solution values"))
    assertthat::assert_that(
      number_of_zones(x) == ncol(sf::st_drop_geometry(solution)),
      msg = msg)
    ## number of units
    assertthat::assert_that(
      number_of_total_units(x) == nrow(solution))
    ## spatial information
    assertthat::assert_that(
      sf::st_crs(x$data$cost) == sf::st_crs(solution))
    ## solution values
    assertthat::assert_that(
      is.numeric(unlist(sf::st_drop_geometry(solution))),
      msg = paste("solution object should only contain columns",
                  "with numeric values"))
    assertthat::assert_that(
      min(unlist(sf::st_drop_geometry(solution)), na.rm = TRUE) >= 0,
      max(unlist(sf::st_drop_geometry(solution)), na.rm = TRUE) <= 1,
      msg = paste("solution object should only contain columns",
                  "values between zero and one"))
    # perform calculations
    solution <- sf::st_drop_geometry(solution)
    internal_planning_unit_solution_status(x, as.matrix(solution))
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution) {
    # assert valid arguments
    ## data types
    assertthat::assert_that(
      inherits(solution, "Raster"),
      inherits(x$data$cost, "Raster"),
      msg = paste(
        "argument to x and solution object have",
        "planning unit data in different formats"))
    ## dimensionality
    assertthat::assert_that(
      number_of_zones(x) == raster::nlayers(solution),
      is_comparable_raster(x$data$cost, solution[[1]]))
    ## spatial information
    assertthat::assert_that(
      sf::st_crs(x$data$cost@crs) == sf::st_crs(solution@crs))
    ## values in the solution
    assertthat::assert_that(
      min(raster::cellStats(solution, "min")) >= 0,
      max(raster::cellStats(solution, "max")) <= 1,
      msg = paste("solution object should only contain",
                  "values between zero and one"))
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    if (raster::nlayers(solution) > 1) {
      pos2 <- raster::Which(max(!is.na(solution)) == 1, cells = TRUE)
    } else {
      pos2 <- raster::Which(!is.na(solution), cells = TRUE)
    }
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA values in the",
           " solution")
    solution <- solution[pos2]
    if (!is.matrix(solution))
      solution <- matrix(solution, ncol = 1)
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution))))
     stop("planning units with NA cost data must have NA values in the",
          " solution")
    # return status
    solution
})

#' Internal helper function
#'
#' This helper function is used to extract solution status values
#' from a matrix containing all total units in the problem.
#' It is used when the planning units are in
#' `numeric`, `matrix`, `data.frame`, `Spatial`, or `sf` format.
#'
#' @inheritParams planning_unit_solution_status
#'
#' @noRd
internal_planning_unit_solution_status <- function(x, solution) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    is.matrix(solution),
      msg = paste(
        "argument to x and solution object have",
        "planning unit data in different formats"))
  # subset planning units from total units
  pos <- x$planning_unit_indices()
  pos2 <- which(rowSums(is.na(solution)) != ncol(solution))
  if (!setequal(pos, pos2))
    stop("planning units with NA cost values must also have NA values in the",
         " solution")
  out <- solution[pos, , drop = FALSE]
  if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(out))))
    stop("planning units with NA cost values must also have NA values in the",
         " solution")
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
  assertthat::assert_that(assertthat::is.string(x))
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

\\item{`x` has [`Raster-class`] planning units}{The argument to `", x , "`
  be a [`Raster-class`] object where different grid cells (pixels) correspond
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

\\item{`x` has [`Spatial-class`] planning units}{The argument to `", x , "`
  must be a [`Spatial-class`] object with each column corresponding to a
  different zone, each row corresponding to a different planning unit, and
  cell values corresponding to the solution value. This means that if the
  [`Spatial-class`] object containing the solution also contains additional
  columns, then these columns will need to be subsetted prior to using this
  function (see below for example with [sf::sf()] data).
  Additionally, the argument to `", x , "` must also have the same
  coordinate reference system as the planning unit data.
  Furthermore, any planning units missing cost
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
