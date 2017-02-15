#' @include internal.R generics.R ConservationProblem-proto.R
#'   OptimizationProblem-proto.R compile.R
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
    sol <- solve(opt, a$solver)[seq_len(opt$number_of_planning_units())]
    # extract solution and return Raster or Spatial object
    pu <- a$data$cost
    if (inherits(pu, 'RasterLayer')) {
      pu[Which(!is.na(pu))] <- pu_sol
    } else if (inherits(pu, 'Spatial')) {
      pu$solution <- pu_sol
    }
    pu
  }
)

