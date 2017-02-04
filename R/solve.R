#' @include internal.R generics.R ConservationProblem-class.R OptimizationProblem-class.R compile.R
NULL

#' @export
methods::setMethod('solve', signature(a='OptimizationProblem', b='Solver'),
  function(a, b) b$solve(a))

#' @export
methods::setMethod('solve', signature(a='ConservationProblem', b='missing'),
  function(a, b) {
    # assign solver
    if (inherits(a$solver, 'waiver'))
      a$solver <- default_solver()
    # compile and solve optimisation problem
    opt <- compile.ConservationProblem(a)
    sol <- solve(opt, a$solver)
    # extract solution and return Raster or Spatial object
    pu <- a$cost
    pu_sol <- sol[pu_indices_in_obj(opt)]
    if (inherits(pu, 'RasterLayer')) {
      pu[Which(!is.na(pu))] <- pu_sol
    } else if (inherits(pu, 'Spatial')) {
      pu$solution <- pu_sol
    }
    pu
  }
)

