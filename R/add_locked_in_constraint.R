#' @include internal.R generics.R Constraint-proto.R intersecting_units.R
NULL

#' @export
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
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, 'OptimizationProblem'),
          inherits(y, 'ConservationProblem'))
        if (self$parameters$get('apply constraint?')==1) {
          # get locked in units
          units <- self$data$locked_in
          # if cost layer is a raster convert cell indices to relative 
          # indices based on which cells in the cost layer are 
          # finite (ie. not NA)
          if (inherits(y$data$cost, 'Raster')) {
            units <- match(units, raster::Which(!is.na(y$data$cost), 
              cells=TRUE))
            units <- units[!is.na(units)]
          }
          # apply constraint
          invisible(rcpp_apply_locked_in_constraint(x$ptr, units))
        }
        invisible(TRUE)
      }))
    # return problem
    return(x)
  }
)

#' @export
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

#' @export
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

#' @export
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
