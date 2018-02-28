#' @include internal.R ConservationProblem-proto.R OptimizationProblem-proto.R compile.R
NULL

#' Solve a problem
#'
#' Solve a \code{\link{ConservationProblem-class}} or an
#' \code{\link{OptimizationProblem-class}} object.
#'
#' @param a \code{\link{ConservationProblem-class}} or an
#'   \code{\link{OptimizationProblem-class}} object.
#'
#' @param b \code{\link{Solver-class}} object. Not used if \code{a} is an
#'   \code{\link{ConservationProblem-class}} object.
#'
#' @param ... arguments passed to \code{\link{compile}}.
#'
#' @details The object returned from this function depends on the argument to
#'   \code{a}. If the argument to \code{a} is an
#'   \code{\link{OptimizationProblem-class}} object, then the
#'   solution is returned as a \code{logical} \code{vector} showing the status
#'   of each planning unit in each zone. On the other hand, if the argument
#'   to \code{a} is an \code{\link{ConservationProblem-class}} object,
#'   then the type of object returned depends on the number of solutions
#'   generated and the type data used to represent planning unit costs in the
#'   argument to \code{a}.
#'
#'   \describe{
#'
#'   \item{\code{numeric}}{\code{vector} containing the solution. Here,
#'     Each element corresponds to a different planning unit. If
#'     multiple solutions are generated, then the solution is returned as
#'     a \code{list} of \code{numeric} \code{vectors}.}
#'
#'   \item{\code{matrix}}{containing \code{numeric} values for the solution.
#'     Here, rows correspond to different planning units,
#'     and fields (columns) correspond to different  management zones. If
#'     multiple solutions are generated, then the solution is returned as
#'     a \code{list} of \code{matrix} objects.}
#'
#'   \item{\code{\link[raster]{Raster}}}{containing the solution in pixel
#'      values. If the argument to \code{x} contains a single management zone,
#'      then a \code{RasterLayer} object will be returned. Otherwise, if the
#'      argument to \code{x} contains multiple zones, then a
#'      \code{\link[raster]{RasterStack-class}} object
#'      will be returned containing a different layer for each management zone.
#'      If multiple solutions are generated, then the solution is returned as
#'      a \code{list} of \code{Raster} objects.}
#'
#'   \item{\code{\link[sp]{Spatial-class}} or \code{data.frame}}{
#'      containing the solution in fields (columns). Here, each row
#'      corresponds to a different planning unit. If the argument to \code{x}
#'      contains a single zone, the fields containing solutions are named
#'      \code{"solution_XXX"} where \code{"XXX"} corresponds to the solution
#'      number. If the argument to \code{x} contains multiple zones, the fields
#'      containing solutions are named \code{"solution_XXX_YYY"} where
#'      \code{"XXX"} corresponds to the solution and \code{"YYY"} is the name
#'      of the management zone.}
#'
#'   }
#'
#' Since this function returns an object that specifies how much of each
#' planning unit is allocated to each management zone, it may be useful to use
#' the \code{\link{categorize_by_zone}} function to reformat the output
#' for problems containing multiple zones.
#'
#' @return A \code{numeric}, \code{matrix},
#'   \code{\link[raster]{RasterLayer-class}}, or
#'   \code{\link[sp]{Spatial-class}} object containing the solution to
#'   the problem.
#'
#' @seealso \code{\link{problem}}, \code{\link{solvers}},
#'   \code{\link{categorize_by_zone}}
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
    # format solutions into planning unit by zones matrix
    na_pos <- which(is.na(a$planning_unit_costs()), arr.ind = TRUE)
    sol_status <- lapply(sol, function(x) {
      m <- matrix(x[[1]][seq_len(a$number_of_planning_units() *
                                 a$number_of_zones())],
                  nrow = a$number_of_planning_units(),
                  ncol = a$number_of_zones())
      m[na_pos] <- NA_real_
      return(m)
    })
    # create solution data
    pu <- a$data$cost
    if (inherits(pu, "Raster")) {
      # RasterLayer planning units
      if (raster::nlayers(pu) == 1) {
        pos <- raster::Which(!is.na(pu), cells = TRUE)
      } else {
        pos <- raster::Which(max(!is.na(pu)) > 0, cells = TRUE)
      }
      pu <- suppressWarnings(raster::setValues(pu[[1]], NA))
      ret <- lapply(sol_status, function(s) {
        ret <- lapply(seq_len(ncol(s)), function(z) {
          pu[pos] <- s[, z]
          return(pu)
        })
        if (length(ret) > 1) {
          ret <- raster::stack(ret)
        } else {
          ret <- ret[[1]]
        }
        return(ret)
      })
      names(ret) <- paste0("solution_", seq_along(sol))
    } else if (inherits(pu, c("data.frame", "Spatial"))) {
      # Spatial* or data.frame planning units
      sol_status <- do.call(cbind, sol_status)
      if (a$number_of_zones() == 1) {
        colnames(sol_status) <- paste0("solution_", seq_along(sol))
      } else {
        colnames(sol_status) <- paste0("solution_",
                                       rep(seq_along(sol),
                                          each = a$number_of_zones()), "_",
                                       rep(a$zone_names(), length(sol)))
      }
      # add in NA values for planning units that contained NA values in
      # all zones that were discarded from the mathematical formulation
      # to reduce overheads
      pos <- which(rowSums(!is.na(as.matrix(
               as.data.frame(pu)[, a$data$cost_column, drop = FALSE]))) > 0)
      if (!identical(pos, seq_len(nrow(sol_status)))) {
        sol_status2 <- matrix(NA, nrow = a$number_of_total_units(),
                              ncol = ncol(sol_status))
        sol_status2[pos, ] <- sol_status
        dimnames(sol_status2) <- dimnames(sol_status)
      } else {
        sol_status2 <- sol_status
      }
      # cbind solutions to planning unit data
      if (inherits(pu, "Spatial")) {
        ret <- pu
        ret@data <- cbind(ret@data, as.data.frame(sol_status2))
      } else {
        ret <- cbind(pu, as.data.frame(sol_status2))
      }
    } else if (is.matrix(pu)) {
      # matrix planning units
      # add in NA values for planning units that contained NA values in
      # all zones that were discarded from the mathematical formulation
      # to reduce overheads
      pos <- which(rowSums(!is.na(pu)) > 0)
      ret <- lapply(sol_status, function(s) {
        pu[pos, ] <- s
        return(pu)
      })
    } else {
      stop("planning unit data is of an unrecognized class")
    }
    # if ret is a list of matrices with a single column then convert to numeric
    if (is.matrix(ret[[1]]) && ncol(ret[[1]]) == 1)
      ret <- lapply(ret, as.numeric)
    # if ret is a list with a single element then extract the element
    if (length(ret) == 1)
      ret <- ret[[1]]
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
