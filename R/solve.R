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
#' plot(s, main = "solution", axes = FALSE, box = FALSE)
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
    ## solve problem
    # assign solver
    if (inherits(a$solver, "Waiver"))
      a <- add_default_solver(a)
    if (inherits(a$portfolio, "Waiver"))
      a <- add_default_portfolio(a)
    # compile and solve optimisation problem
    opt <- compile.ConservationProblem(a, ...)
    sol <- a$portfolio$run(opt, a$solver)
    # check that solution is valid
    if (is.null(sol)) {
      stop("conservation problem is infeasible")
    }
    ## format solutions
    # create solution data
    pu <- a$data$cost
    if (inherits(pu, "Raster")) {
      # RasterLayer planning units
      ret <- lapply(sol, function(s) {
        pu[raster::Which(!is.na(pu))] <-
          s[[1]][seq_len(a$number_of_planning_units())]
        return(pu)
      })
      ret <- raster::stack(ret)
      if (length(sol) == 1)
        ret <- ret[[1]]
      names(ret) <- paste0("solution_", seq_along(sol))
    } else if (inherits(pu, c("data.frame", "Spatial"))) {
      # Spatial* or data.frame planning units
      sol2 <- vapply(sol, `[[`, numeric(length(sol[[1]][[1]])), 1)
      sol2 <- sol2[seq_len(a$number_of_planning_units()), , drop = FALSE]
      sol2 <- stats::setNames(as.data.frame(sol2), paste0("solution_",
                                                          seq_along(sol)))
      if (inherits(pu, "Spatial")) {
        ret <- pu
        ret@data <- cbind(ret@data, sol2)
      } else {
        ret <- cbind(pu, sol2)
      }
    } else if (is.numeric(pu)) {
      # numeric planning units
      if (length(sol) == 1) {
        ret <- pu
        ret[!is.na(pu)] <- sol[[1]][[1]][seq_len(a$number_of_planning_units())]
      } else {
        ret <- matrix(NA, ncol = length(pu), nrow = length(sol))
        rownames(ret) <- paste0("solution_", seq_along(sol))
        ret_rows <- rep(seq_along(sol), each = sum(!is.na(pu)))
        ret_cols <- rep(which(!is.na(pu)), length(sol))
        sol2 <- vapply(sol, `[[`, numeric(length(sol[[1]][[1]])), 1)
        sol2 <- sol2[seq_len(a$number_of_planning_units()), , drop = FALSE]
        ret[matrix(c(ret_rows, ret_cols), ncol = 2)] <- c(sol2)
      }
    } else {
      stop("planning unit data is of an unrecognized class")
    }
    # add attributes
    attr(ret, "objective") <- stats::setNames(vapply(sol, `[[`, numeric(1), 2),
                                              paste0("solution_",
                                                     seq_along(sol)))
    attr(ret, "status") <- stats::setNames(vapply(sol, `[[`, character(1), 3),
                                           paste0("solution_", seq_along(sol)))
    attr(ret, "runtime") <- stats::setNames(vapply(sol, `[[`, numeric(1), 4),
                                            paste0("solution_", seq_along(sol)))
    # return object
    return(ret)
  }
)
