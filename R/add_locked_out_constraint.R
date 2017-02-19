#' @include internal.R generics.R Constraint-proto.R intersecting_units.R
NULL

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
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, 'OptimizationProblem'),
          inherits(y, 'ConservationProblem'))
        if (self$parameters$get('apply constraint?')==1) {
          # get locked in units
          units <- self$data$locked_out
          # if cost layer is a raster convert cell indices to relative 
          # indices based on which cells in the cost layer are 
          # finite (ie. not NA)
          if (inherits(y$data$cost, 'Raster')) {
            units <- match(units, raster::Which(!is.na(y$data$cost), 
              cells=TRUE))
            units <- units[!is.na(units)]
          }
          # apply constraint
          invisible(rcpp_apply_locked_out_constraint(x$ptr, units))
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
