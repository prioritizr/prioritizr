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
    assertthat::assert_that(
      is.numeric(solution),
      is.numeric(x$data$cost), is.matrix(x$data$cost))
    assertthat::assert_that(
      number_of_total_units(x) == length(solution),
      number_of_zones(x) == 1,
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1)
    # extract status
    internal_planning_unit_solution_status(x, matrix(solution, ncol = 1))
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      is.matrix(solution), is.numeric(solution),
      is.matrix(x$data$cost), is.numeric(x$data$cost))
    assertthat::assert_that(
      number_of_total_units(x) == nrow(solution),
      number_of_zones(x) == ncol(solution),
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1)
    # return status
    internal_planning_unit_solution_status(x, solution)
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      is.data.frame(solution),
      is.data.frame(x$data$cost))
    assertthat::assert_that(
      number_of_zones(x) == ncol(solution),
      number_of_total_units(x) == nrow(solution),
      is.numeric(unlist(solution)),
      min(unlist(solution), na.rm = TRUE) >= 0,
      max(unlist(solution), na.rm = TRUE) <= 1)
    # return status
    internal_planning_unit_solution_status(x, as.matrix(solution))
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, c("SpatialPointsDataFrame", "SpatialLinesDataFrame",
                           "SpatialPolygonsDataFrame")),
      class(x$data$cost)[1] == class(solution)[1])
    assertthat::assert_that(
      number_of_zones(x) == ncol(solution@data),
      sf::st_crs(x$data$cost@proj4string) == sf::st_crs(solution@proj4string),
      number_of_total_units(x) == nrow(solution@data),
      is.numeric(unlist(solution@data)),
      min(unlist(solution@data), na.rm = TRUE) >= 0,
      max(unlist(solution@data), na.rm = TRUE) <= 1)
    # return status
    internal_planning_unit_solution_status(x, as.matrix(solution@data))
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, "sf"),
      inherits(x$data$cost, "sf"))
    assertthat::assert_that(
      sf::st_crs(x$data$cost) == sf::st_crs(solution),
      number_of_zones(x) == ncol(sf::st_drop_geometry(solution)),
      number_of_total_units(x) == nrow(solution),
      is.numeric(unlist(sf::st_drop_geometry(solution))),
      min(unlist(sf::st_drop_geometry(solution)), na.rm = TRUE) >= 0,
      max(unlist(sf::st_drop_geometry(solution)), na.rm = TRUE) <= 1)
    # perform calculations
    solution <- sf::st_drop_geometry(solution)
    internal_planning_unit_solution_status(x, as.matrix(solution))
})

methods::setMethod(
  "planning_unit_solution_status",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, "Raster"),
      inherits(x$data$cost, "Raster"))
    assertthat::assert_that(
      number_of_zones(x) == raster::nlayers(solution),
      sf::st_crs(x$data$cost@crs) == sf::st_crs(solution@crs),
      is_comparable_raster(x$data$cost, solution[[1]]),
      min(raster::cellStats(solution, "min")) >= 0,
      max(raster::cellStats(solution, "max")) <= 1)
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    if (raster::nlayers(solution) > 1) {
      pos2 <- raster::Which(max(!is.na(solution)) == 1, cells = TRUE)
    } else {
      pos2 <- raster::Which(!is.na(solution), cells = TRUE)
    }
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution <- solution[pos2]
    if (!is.matrix(solution))
      solution <- matrix(solution, ncol = 1)
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution))))
     stop("planning units with NA cost data must have NA allocations in the",
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
    is.matrix(solution))
  # subset planning units from total units
  pos <- x$planning_unit_indices()
  pos2 <- which(rowSums(is.na(solution)) != ncol(solution))
  if (!setequal(pos, pos2))
    stop("planning units with NA cost data must have NA values in the",
         " solution")
  out <- solution[pos, , drop = FALSE]
  if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(out))))
    stop("planning units with NA cost data must have NA values in the",
         " solution")
  # force removal of rownames
  rownames(out) <- NULL
  # return result
  out
}
