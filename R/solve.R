#' @include internal.R ConservationProblem-proto.R OptimizationProblem-proto.R compile.R
NULL

#' Solve a problem
#'
#' Solve a \code{\link{ConservationProblem}} or an \code{OptimizationProblem}.
#'
#' @param a \code{\link{ConservationProblem}} or an \code{OptimizationProblem}
#'   object.
#'
#' @param b \code{\link{Solver}} object. Not need if \code{a} is an
#'   \code{\link{ConservationProblem}} object.
#'
#' @details If a \code{\link{OptimizationProblem}} is supplied, then the solution
#'   is returned as a \code{logical} showing the status of each planning unit.
#'   If a \code{\link{ConservationProblem}} is supplied, then the 
#'   solution is returned in the format of the cost data contained inside it:
#'   \itemize{
#'   \item{\code{\link[raster]{RasterLayer-class}}}{the solution is returned as a
#'      \code{\link[raster]{RasterLayer-class}} object. Cell values denote the 
#'      decision.}
#'   \item{\code{Spatial}}{the solution is returned as a 
#'      \code{\link{SpatialPolygonsDataFrame}} or
#'      \code{\link{SpatialLinesDataFrame}} with a "Solution" column containing
#'      the decision values.}
#'   }
#'
#' @return A \code{\link[raster]{RasterLayer-class}}, \code{\link[sp]{Spatial}}, or a 
#'   \code{numeric} vector containing the solution depending on the
#'   argument to \code{a}.
#'
#' @examples
#' # build minimal conservation problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decision() %>%
#'   add_default_solver()
#'
#' # solve the problem
#' s <- solve(p)
#'
#' # print the solution
#' print(s)
#'
#' @name solve
#'
#' @importFrom Matrix solve
#'
#' @exportMethod solve
#'
#' @export
NULL

#' @name solve
#'
#' @rdname solve
#'
#' @usage solve(a,b) # OptimizationProblem,Solver
methods::setMethod('solve', signature(a='OptimizationProblem', b='Solver'),
  function(a, b) b$solve(a)
)

#' @name solve
#'
#' @rdname solve
#'
#' @usage solve(a,b) # ConservationProblem,missing
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
