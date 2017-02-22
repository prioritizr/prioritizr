#' @include internal.R Constraint-proto.R intersecting_units.R
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
#'   \item{\code{\link[raster]{Raster-class}} object}{Planning units in \code{x} 
#'     that intersect with cells in \code{y} that are not equal to zero and 
#'     not \code{NA} are locked in.}
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
#' p1 <- problem(sim_pu_polygons, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.2)
#'
#' # create problem with added locked in constraints using integers
#' p2 <- p1 %>% add_locked_in_constraint(which(sim_pu_polygons$locked_in))
#'
#' # create problem with added locked in constraints using a field name
#' p3 <- p1 %>% add_locked_in_constraint('locked_in')
#'
#' # create problem with added locked in constraints using raster data
#' p4 <- p1 %>% add_locked_in_constraint(sim_locked_in_raster)
#'
#' # create problem with added locked in constraints using spatial polygons data
#' locked_in <- sim_pu_polygons[sim_pu_polygons$locked_in == 1,]
#' p5 <- p1 %>% add_locked_in_constraint(locked_in)
#'
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#' s5 <- solve(p5)
#' 
#' # plot solutions
#' par(mfrow=c(3,2))
#' plot(s1, main='none locked out')
#' plot(s1[s1$solution==1,], col='darkgreen', add=TRUE)
#'
#' plot(s2, main='locked out (integer input)')
#' plot(s2[s2$solution==1,], col='darkgreen', add=TRUE)
#'
#' plot(s3, main='locked out (character input)')
#' plot(s3[s3$solution==1,], col='darkgreen', add=TRUE)
#'
#' plot(s4, main='locked out (raster input)')
#' plot(s4[s4$solution==1,], col='darkgreen', add=TRUE)
#'
#' plot(s5, main='locked out (polygon input)')
#' plot(s5[s5$solution==1,], col='darkgreen', add=TRUE)
#'
#' @seealso \code{\link{constraints}} for all the available constraints.
#'
#' @name add_locked_in_constraint
#'
#' @exportMethod add_locked_in_constraint
#'
#' @export
methods::setGeneric('add_locked_in_constraint', 
                    signature=methods::signature('x', 'locked_in'),
                    function(x, locked_in) 
                      standardGeneric('add_locked_in_constraint'))


#' @name add_locked_in_constraint
#' @rdname add_locked_in_constraint
methods::setMethod('add_locked_in_constraint', 
  methods::signature('ConservationProblem', 'numeric'),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_in, c('integer', 'numeric')), 
        isTRUE(all(is.finite(locked_in))), 
        isTRUE(all(round(locked_in) == locked_in)),
        isTRUE(max(locked_in) <= x$number_of_total_units()),
        isTRUE(min(locked_in) >= 1))
    # create parameters 
    p <- parameters(binary_parameter('apply constraint?', 1L))
    # create new constraint object
    x$add_constraint(pproto(
      'LockedInConstraint',
      Constraint,
      name='Locked in planning units',
      data=list(locked_in=as.integer(locked_in)),
      parameters=p,
      calculate=function(self, x) {
        assertthat::assert_that(inherits(x, 'ConservationProblem'))
        if (is.Waiver(self$get_data('locked_in_indices'))) {
          if (inherits(x$get_data('cost'), 'Raster')) {
            # get locked in units
            units <- self$get_data('locked_in')
            # if cost layer is a raster convert cell indices to relative 
            # indices based on which cells in the cost layer are 
            # finite (ie. not NA)
            units <- match(units, raster::Which(!is.na(x$get_data('cost')), 
              cells=TRUE))
            units <- units[!is.na(units)]
            self$set_data('locked_in_indices', units)
          } else {
            self$set_data('locked_in_indices', self$get_data('locked_in'))
          }
        }
        invisible(TRUE)
      },
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, 'OptimizationProblem'),
          inherits(y, 'ConservationProblem'))
        if (self$parameters$get('apply constraint?')==1) {          
          # apply constraint
          invisible(rcpp_apply_locked_in_constraint(x$ptr, 
            self$get_data('locked_in_indices')))
        }
        invisible(TRUE)
      }))
  }
)

#' @name add_locked_in_constraint
#' @rdname add_locked_in_constraint
methods::setMethod('add_locked_in_constraint', 
  methods::signature('ConservationProblem', 'character'),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      assertthat::is.string(locked_in),
      inherits(x$data$cost, 'Spatial'), 
      isTRUE('data' %in% slotNames(x$data$cost)),
      isTRUE(locked_in %in% names(x$data$cost)),
      isTRUE(inherits(x$data$cost[[locked_in]], 'logical')))
    # add constraint
    add_locked_in_constraint(x, which(x$data$cost[[locked_in]]))
  }
)

#' @name add_locked_in_constraint
#' @rdname add_locked_in_constraint
methods::setMethod('add_locked_in_constraint', 
  methods::signature('ConservationProblem', 'Spatial'),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_in, 'Spatial'))
    # add constraints
    add_locked_in_constraint(x, intersecting_units(x$data$cost, locked_in))
  }
)

#' @name add_locked_in_constraint
#' @rdname add_locked_in_constraint
methods::setMethod('add_locked_in_constraint', 
  methods::signature('ConservationProblem', 'Raster'),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_in, 'Raster'), 
      isTRUE(raster::cellStats(locked_in, 'sum') > 0))
    add_locked_in_constraint(x, intersecting_units(x$data$cost, locked_in))
  }
)
