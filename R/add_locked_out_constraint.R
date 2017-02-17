#' @include internal.R Constraint-proto.R
NULL

#' Add locked out constraints
#'
#' Add constraints to ensure that certain planning units are locked out 
#' from the solution. For example, it may be useful to lock out planning 
#' units that have been substantially altered by anthropogenic development,
#' and so contain little remaining habitat.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param locked_in Object that determines which planning units that should be 
#'   locked out. See details for more information.
#'
#' @details The locked out planning units can be specified in several 
#'   different ways:
#'
#'   \describe{
#'   
#'   \item{\code{integer} \code{vector}}{indices for which planning units should
#'     be locked out.}
#'
#'   \item{\code{character}}{column name in the attribute table with 
#'     \code{logical} values indicating if planning units should be locked out. 
#'     Note that \code{locked_out} can only a \code{character} if the planning
#'     units in \code{x} are a \code{\link{SpatialPolygonsDataFrame-class}}, 
#'     \code{\link{SpatialLinesDataFrame-class}}, or 
#'     \code{\link{SpatialPointsDataFrame-class}} object.}
#'
#'   \item{\code{\link{Raster-class}} object}{with \code{logical} cells values. 
#'     Planning units in \code{x} that spatially intersect with at least one 
#'     \code{TRUE} pixel are locked in.}
#'
#'   \item{\code{\link{Spatial-class}} object.}{planning units in \code{x} that
#'     spatially intersect with \code{locked_in} are locked in.}
#'
#'  }
#'
#' @return \code{\link{ConservationProblem}} object.
#'
#' @examples
#' # create basic problem
#' p <- problem(sim_pu_polygons, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.2)
#'
#' # create problem with added locked out constraints using integers
#' p2 <- p %>% add_locked_out_constraint(which(sim_pu_polygons$locked_in))
#'
#' # create problem with added locked out constraints using a field name
#' p3 <- p %>% add_locked_out_constraint('locked_out')
#'
#' # create problem with added locked out constraints using raster data
#' p4 <- p %>% add_locked_out_constraint(sim_locked_out_raster)
#'
#' # create problem with added locked out constraints using spatial polygons 
#' # data
#' locked_out <- sim_locked_in_polygons[sim_pu_polygons$locked_out == 1,]
#' p5 <- p %>% add_locked_out_constraint(locked_out)
#'
#' # solve problems
#' s <- stack(solve(p), solve(p2), solve(p3), solve(p4), solve(p5))
#' names(s) <- c('basic solution', 'locked out (integer input)', 
#'               'locked out (character input)', 'locked out (raster input)', 
#'               'locked out (polygons input)')
#'
#' # plot solutions
#' plot(s)
#' 
#' @seealso \code{\link{constraints}} for all the available constraints.
#'
#' @export
add_locked_out_constraint <- function(x, locked_out) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, 'ConservationProblem'),
    inherits(locked_out, c('character', 'numeric',  'Spatial', 'RasterLayer')))
  pu_status <- rep(0L, length(x$number_of_planning_units()))
  # create parameters    
  if (inherits(locked_out, 'character')) {
    assertthat::assert_that(inherits(x$data$cost, 'Spatial'),
      isTRUE(locked_out %in% names(x$data$cost)),
      isTRUE(inherits(x$data$cost[[locked_out]], 'logical')))
    pu_status[which(x$data$cost[[locked_out]])] <- 1L 
  } else if (inherits(locked_out, 'numeric')) {
    assertthat::assert_that(isTRUE(all(is.finite(locked_out))), 
      isTRUE(all(round(locked_out)) == locked_out),
      isTRUE(max(locked_out) < x$number_of_planning_units()),
      isTRUE(min(locked_out >= 1)))
    pu_status[as.integer(locked_out)] <- 1L
  } else if (inherits(locked_out, 'Raster')) {
    assertthat::assert_that(locked_out@file@datanotation == 'LOG1S', 
      raster::nlayers(locked_out)==1, 
      raster::compareRaster(x$data$features[[1]], locked_out, crs=TRUE, res=TRUE,
        tolerance=1e-5, stopiffalse=FALSE))
    if (inherits(x$data$cost, 'Spatial')) {
      ext <- fast_extract(locked_out, x$data$cost, fun='max', na.rm=TRUE, 
        df=TRUE)
      pu_status[ext[[2]][ext[[3]]==1]] <- 1L
    } else if (inherits(x$data$cost, 'Raster')) {
      z <- raster::zonal(x$data$cost, locked_out, fun='max')
      z <- z[which(z[,3]==1),2]
      pu_status[intersect(x$planning_unit_indices(), z)] <- 1L
    } else {
      stop('argument to x has cost data of unrecognized format')
    }
  } else if (inherits(locked_out, 'Spatial')) {
    asserthat::assert_that(
      raster::compareCRS(x@proj4string, x$data$features@crs),
      isTRUE(rgeos::gIntersects(
        as(raster::extent(locked_out), 'SpatialPolygons'),
        as(raster::extent(x$data$features), 'SpatialPolygons'))))
    if (inherits(x$data$cost, 'Spatial')) {
      intersects <- rgeos::gIntersects(x$data$cost, locked_out, byid=TRUE, 
        returnDense=FALSE)
      intersects <- rcpp_list_to_matrix_indices(rcpp_list_to_matrix_indices)
      pu_status[unique(intersects@i)] <- 1L
    } else if (inherits(x$data$cost, 'Raster')) {
      ext <- fast_extract(x$data$cost[[1]], locked_out, cellnumbers=TRUE, 
        sp=FALSE, df=TRUE)
      pu_status[intersect(x$planning_unit_indices(), unique(ext$cell))] <- 1L
    } else {
      stop('argument to x has cost data of unrecognized format')
    }
  } else {
    stop('argument to locked_out is of unrecognized format')
  }
  p <- parameters(binary_parameter('Apply constraint?', 1L),
    binary_parameter_array('Planning units', pu_status,
      as.character(seq_len(x$number_of_planning_units()))))
  # create new constraint object
  x$add_constraint(pproto(
    'LockedOutConstraint',
    Constraint,
    name='Locked out',
    parameters=p,
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      if (self$parameters$get('Apply constraint?')==1)
        rcpp_apply_locked_out_constraint(x$ptr, 
          self$parameters$get('Planning units')[[1]])
      invisible(TRUE)
    }))
  # return problem
  return(x)
}
