#' @include internal.R generics.R ConservationProblem-proto.R OptimizationProblem-proto.R compile.R
NULL

#' @export
methods::setMethod('solve', signature(a='OptimizationProblem', b='Solver'),
  function(a, b) b$solve(a)
)

#' @export
methods::setMethod('solve', signature(a='ConservationProblem', b='missing'),
  function(a, b) {
    # assign solver
    if (inherits(a$solver, 'Waiver'))
      a <- add_default_solver(a)
    # compile and solve optimisation problem
    opt <- compile.ConservationProblem(a)
    sol <- solve(opt, a$solver)[seq_len(opt$number_of_planning_units())]
    # check that solution is valid
    if (is.null(sol)) {
      stop('conservation problem is infeasible')
     } 
    # extract solution and return Raster or Spatial object
    pu <- a$data$cost
    if (inherits(pu, 'Raster')) {
      pu[raster::Which(!is.na(pu))] <- sol
    } else if (inherits(pu, 'Spatial')) {
      pu$solution <- sol
    }
    return(pu)
  }
)
