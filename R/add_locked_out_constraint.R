#' @include internal.R Constraint-proto.R
NULL

#' @export
methods::setMethod('add_locked_out_constraint', 
  methods::signature('ConservationProblem', 'numeric'),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_out, 'numeric'), isTRUE(all(is.finite(locked_out))), 
        isTRUE(all(round(locked_out)) == locked_out),
        isTRUE(max(locked_out) < x$number_of_planning_units()),
        isTRUE(min(locked_out) >= 1))
    # create parameters 
    pu_status <- rep(0L, length(x$number_of_planning_units()))
    pu_status[as.integer(locked_out)] <- 1L
    p <- parameters(binary_parameter('Apply constraint?', 1L),
    binary_parameter_array('Planning units', pu_status,
      as.character(seq_len(x$number_of_planning_units()))))
    # create new constraint object
    x$add_constraint(pproto(
      'LockedOutConstraint',
      Constraint,
      name='Locked out planning units',
      parameters=p,
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x$ptr, 'OptimizationProblem'),
          inherits(y, 'ConservationProblem'))
        if (self$parameters$get('apply constraint?')==1)
          invisible(rcpp_apply_locked_out_constraint(x$ptr, 
            self$parameters$get('planning units')[[1]]))
      }))
    # return problem
    return(x)
  }
)

#' @export
methods::setMethod('add_locked_out_constraint', 
  methods::signature('ConservationProblem', 'character'),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      assertthat::is.string(locked_out),
      isTRUE(locked_out %in% names(x$data$cost)),
      isTRUE(inherits(x$data$cost[[locked_out]], 'logical')))
    # add constraint
    add_locked_out_constraint(x, which(x$data$cost[[locked_out]]))
  }
)

#' @export
methods::setMethod('add_locked_out_constraint', 
  methods::signature('ConservationProblem', 'Spatial'),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_out, 'Spatial'))
    # add constraints
    add_locked_out_constraint(x, intersecting_units(x$cost$data, locked_out))
  }
)
  

#' @export
methods::setMethod('add_locked_out_constraint', 
  methods::signature('ConservationProblem', 'Raster'),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, 'ConservationProblem'),
      inherits(locked_out, 'Raster'))
    # add constraints
    add_locked_out_constraint(x, intersecting_units(x$cost$data, locked_out))
})
