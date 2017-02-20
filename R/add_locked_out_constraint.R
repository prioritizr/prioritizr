#' @include internal.R Constraint-proto.R intersecting_units.R
NULL

#' Add locked out constraint
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
#' @name add_locked_out_constraint
#'
#' @exportMethod add_locked_out_constraint
#'
#' @export
methods::setGeneric('add_locked_out_constraint', 
                    signature=methods::signature('x', 'locked_out'),
                    function(x, locked_out) 
                      standardGeneric('add_locked_out_constraint'))


#' @name add_locked_out_constraint
#' @rdname add_locked_out_constraint
methods::setMethod('add_locked_out_constraint', 
  methods::signature('ConservationProblem', 'numeric'),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_out, c('integer', 'numeric')), 
        isTRUE(all(is.finite(locked_out))), 
        isTRUE(all(round(locked_out) == locked_out)),
        isTRUE(max(locked_out) <= x$number_of_total_units()),
        isTRUE(min(locked_out) >= 1))
    # create parameters 
    p <- parameters(binary_parameter('apply constraint?', 1L))
    # create new constraint object
    x$add_constraint(pproto(
      'LockedOutConstraint',
      Constraint,
      name='Locked out planning units',
      data=list(locked_out=as.integer(locked_out)),
      parameters=p,
      calculate=function(self, x) {
        assertthat::assert_that(inherits(x, 'ConservationProblem'))
        if (is.Waiver(self$get_data('locked_out_indices'))) {
          if (inherits(x$get_data('cost'), 'Raster')) {
            # get locked in units
            units <- self$get_data('locked_out')
            # if cost layer is a raster convert cell indices to relative 
            # indices based on which cells in the cost layer are 
            # finite (ie. not NA)
            units <- match(units, raster::Which(!is.na(x$get_data('cost')), 
              cells=TRUE))
            units <- units[!is.na(units)]
            self$set_data('locked_out_indices', units)
          } else {
            self$set_data('locked_out_indices', self$get_data('locked_out'))
          }
        }
        invisible(TRUE)
      },
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, 'OptimizationProblem'),
          inherits(y, 'ConservationProblem'))
        if (self$parameters$get('apply constraint?')==1) {
          # apply constraint
          rcpp_apply_locked_out_constraint(x$ptr, 
            self$get_data('locked_out_indices'))
        }
        invisible(TRUE)
      }))
  }
)

#' @name add_locked_out_constraint
#' @rdname add_locked_out_constraint
methods::setMethod('add_locked_out_constraint', 
  methods::signature('ConservationProblem', 'character'),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      assertthat::is.string(locked_out),
      inherits(x$data$cost, 'Spatial'), 
      isTRUE('data' %in% slotNames(x$data$cost)),
      isTRUE(locked_out %in% names(x$data$cost)),
      isTRUE(inherits(x$data$cost[[locked_out]], 'logical')))
    # add constraint
    add_locked_out_constraint(x, which(x$data$cost[[locked_out]]))
  }
)

#' @name add_locked_out_constraint
#' @rdname add_locked_out_constraint
methods::setMethod('add_locked_out_constraint', 
  methods::signature('ConservationProblem', 'Spatial'),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_out, 'Spatial'))
    # add constraints
    add_locked_out_constraint(x, intersecting_units(x$data$cost, locked_out))
  }
)

#' @name add_locked_out_constraint
#' @rdname add_locked_out_constraint
methods::setMethod('add_locked_out_constraint', 
  methods::signature('ConservationProblem', 'Raster'),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_out, 'Raster'), 
      isTRUE(raster::cellStats(locked_out, 'sum') > 0))
    add_locked_out_constraint(x, intersecting_units(x$data$cost, locked_out))
  }
)
