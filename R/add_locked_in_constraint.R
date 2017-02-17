#' @include internal.R Constraint-proto.R
NULL

#' Add locked in constraints
#'
#' Add constraints constraints to lock in planning units
#' so that they are selected in the solution. For example, it may
#' be desirable to lock in planning units already within existing
#' protected areas so that the solutions fills in the gaps in the existing
#' reserve network.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param locked_in Object that determines which planning units that should be 
#'   locked in. See details for more information.
#'
#' @details The locked in planning units can be specified in several 
#'   different ways:
#'
#'   \describe{
#'   
#'   \item{\code{integer} \code{vector}}{indices for which planning units should
#'     be locked in.}
#'
#'   \item{\code{character}}{column name in the attribute table with 
#'     \code{logical} values indicating if planning units should be locked in. 
#'     Note that \code{locked_in} can only a \code{character} if the planning
#'     units in \code{x} are a \code{\link{SpatialPolygonsDataFrame-class}}, 
#'     \code{\link{SpatialLinesDataFrame-class}}, or 
#'     \code{\link{SpatialPointsDataFrame-class}} object.}
#'
#'   \item{\code{\link[raster]{Raster-class}} object}{with \code{logical} cells 
#'     values. Planning units in \code{x} that spatially intersect with at 
#'     least one \code{TRUE} pixel are locked in.}
#'
#'   \item{\code{\link[sp]{Spatial-class}} object.}{planning units in \code{x} 
#'     that spatially intersect with \code{locked_in} are locked in.}
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
#' # create problem with added locked in constraints using integers
#' p2 <- p %>% add_locked_in_constraint(which(sim_pu_polygons$locked_in))
#'
#' # create problem with added locked in constraints using a field name
#' p3 <- p %>% add_locked_in_constraint('locked_in')
#'
#' # create problem with added locked in constraints using raster data
#' p4 <- p %>% add_locked_in_constraint(sim_locked_in_raster)
#'
#' # create problem with added locked in constraints using spatial polygons data
#' locked_in <- sim_pu_polygons[sim_pu_polygons$locked_in == 1,]
#' p5 <- p %>% add_locked_in_constraint(locked_in)
#'
#' # solve problems
#' s <- stack(solve(p), solve(p2), solve(p3), solve(p4), solve(p5))
#' names(s) <- c('basic solution', 'locked in (integer input)', 
#'               'locked in (character input)', 'locked in (raster input)', 
#'               'locked in (polygons input)')
#'
#' # plot solutions
#' plot(s)
#' 
#' @seealso \code{\link{constraints}} for all the available constraints.
#'
#' @export
add_locked_in_constraint <- function(x, locked_in) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, 'ConservationProblem'),
    inherits(locked_in, c('character', 'numeric',  'Spatial', 'RasterLayer')))
  pu_status <- rep(0L, length(x$number_of_planning_units()))
  # create parameters    
  if (inherits(locked_in, 'character')) {
    assertthat::assert_that(inherits(x$data$cost, 'Spatial'),
      isTRUE(locked_in %in% names(x$data$cost)),
      isTRUE(inherits(x$data$cost[[locked_in]], 'logical')))
    pu_status[which(x$data$cost[[locked_in]])] <- 1L 
  } else if (inherits(locked_in, 'numeric')) {
    assertthat::assert_that(isTRUE(all(is.finite(locked_in))), 
      isTRUE(all(round(locked_in)) == locked_in),
      isTRUE(max(locked_in) < x$number_of_planning_units()),
      isTRUE(min(locked_in >= 1)))
    pu_status[as.integer(locked_in)] <- 1L
  } else if (inherits(locked_in, 'Raster')) {
    assertthat::assert_that(locked_in@file@datanotation == 'LOG1S', 
      raster::nlayers(locked_in)==1, 
      raster::compareRaster(x$data$features[[1]], locked_in, crs=TRUE, res=TRUE,
        tolerance=1e-5, stopiffalse=FALSE))
    if (inherits(x$data$cost, 'Spatial')) {
    
      ext <- fast_extract(locked_in, x$data$cost, fun='max', na.rm=TRUE, 
        sp=FALSE, df=TRUE)
      pu_status[ext[[2]][ext[[3]]==1]] <- 1L
    } else if (inherits(x$data$cost, 'Raster')) {
      z <- raster::zonal(x$data$cost, locked_in, fun='max')
      z <- z[which(z[,3]==1),2]
      pu_status[intersect(x$planning_unit_indices(), z)] <- 1L
    } else {
      stop('argument to x has cost data of unrecognized format')
    }
  } else if (inherits(locked_in, 'Spatial')) {
    asserthat::assert_that(
      raster::compareCRS(x@proj4string, x$data$features@crs),
      isTRUE(rgeos::gIntersects(
        as(raster::extent(locked_in), 'SpatialPolygons'),
        as(raster::extent(x$data$features), 'SpatialPolygons'))))
    if (inherits(x$data$cost, 'Spatial')) {
      intersects <- rgeos::gIntersects(x$data$cost, locked_in, byid=TRUE, 
        returnDense=FALSE)
      intersects <- rcpp_list_to_matrix_indices(rcpp_list_to_matrix_indices)
      pu_status[unique(intersects@i)] <- 1L
    } else if (inherits(x$data$cost, 'Raster')) {
      ext <- fast_extract(x$data$cost[[1]], locked_in, cellnumbers=TRUE, 
        sp=FALSE, df=TRUE)
      pu_status[intersect(x$planning_unit_indices(), unique(ext$cell))] <- 1L
    } else {
      stop('argument to x has cost data of unrecognized format')
    }
  } else {
    stop('argument to locked_in is of unrecognized format')
  }
  p <- parameters(binary_parameter('Apply constraint?', 1L),
    binary_parameter_array('Planning units', pu_status,
      as.character(seq_len(x$number_of_planning_units()))))
  # create new constraint object
  x$add_constraint(pproto(
    'LockedInConstraint',
    Constraint,
    name='Locked in',
    parameters=p,
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x$ptr, 'OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      if (self$parameters$get('Apply constraint?')==1)
        rcpp_apply_locked_in_constraint(x$ptr, 
          self$parameters$get('Planning units')[[1]])
      invisible(TRUE)
    }))
  # return problem
  return(x)
}
