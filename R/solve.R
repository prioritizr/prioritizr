#' @include internal.R ConservationProblem-proto.R OptimizationProblem-proto.R compile.R
NULL

#' Solve a problem
#'
#' Solve a \code{\link{ConservationProblem-class}} or an
#' \code{\link{OptimizationProblem-class}}.
#'
#' @param a \code{\link{ConservationProblem-class}} or an
#'   \code{\link{OptimizationProblem-class}} object.
#'
#' @param b \code{\link{Solver-class}} object. Not used if \code{a} is an
#'   \code{\link{ConservationProblem-class}} object.
#'
#' @param ... arguments passed to \code{\link{compile}}.
#'
#' @details If a \code{\link{OptimizationProblem-class}} is supplied, then the
#'   solution is returned as a \code{logical} showing the status of each
#'   planning unit. If a \code{\link{ConservationProblem-class}} is supplied,
#'   then the  solution is returned in the format of the cost data contained
#'   inside it:
#'   \itemize{
#'   \item{\code{\link[raster]{RasterLayer-class}}}{the solution is returned as
#'      a \code{\link[raster]{RasterLayer-class}} object. Cell values denote
#'      the decision.}
#'   \item{\code{\link[sp]{Spatial-class}}}{the solution is returned as a
#'      \code{\link[sp]{SpatialPolygonsDataFrame}} or
#'      \code{\link[sp]{SpatialLinesDataFrame}} with a "Solution" column
#'      containing the decision values.}
#'   }
#'
#' @return A \code{\link[raster]{RasterLayer-class}},
#'   \code{\link[sp]{Spatial-class}}, or a \code{numeric} vector containing the
#'   solution depending on the argument to \code{a}.
#'
#' @examples
#' # build minimal conservation problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions()
#'
#' \donttest{
#' # solve the problem
#' s <- solve(p)
#'
#' # print the solution
#' print(s)
#'
#' # plot solution
#' plot(s, main="solution")
#' }
#'
#' @name solve
#'
#' @importFrom Matrix solve
#'
#' @exportMethod solve
#'
#' @aliases solve,OptimizationProblem,Solver-method solve,ConservationProblem,missing-method
#'
#' @export
NULL

#' @name solve
#'
#' @rdname solve
methods::setMethod(
  "solve",
  signature(a = "OptimizationProblem", b = "Solver"),
  function(a, b, ...) b$solve(a)
)

#' @name solve
#'
#' @rdname solve
methods::setMethod(
  "solve",
  signature(a = "ConservationProblem", b = "missing"),
  function(a, b, ...) {
    # assign solver
    if (inherits(a$solver, "Waiver"))
      a <- add_default_solver(a)
    # compile and solve optimisation problem
    opt <- compile.ConservationProblem(a, ...)
    sol <- solve(opt, a$solver)[seq_len(opt$number_of_planning_units())]
    # check that solution is valid
    if (is.null(sol)) {
      stop("conservation problem is infeasible")
     }
    # extract solution and return Raster or Spatial object
    pu <- a$data$cost
    if (inherits(pu, "Raster")) {
      pu[raster::Which(!is.na(pu))] <- sol
    } else if (inherits(pu, c("data.frame", "Spatial"))) {
      pu$solution <- sol
    } else {
      stop("planning unit data is of an unrecognized class")
    }
    return(pu)
  }
)
