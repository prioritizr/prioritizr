#' @include internal.R pproto.R  ConservationProblem-proto.R OptimizationProblem-proto.R compile.R problem.R solve.R presolve_check.R
NULL

#' Replacement cost
#'
#' TODO
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param solution \code{numeric}, \code{matrix}, \code{data.frame},
#'   \code{\link[raster]{Raster-class}}, or \code{\link[sp]{Spatial-class}}
#'   object. See the Details section for more information.
#'
#' @param run_checks \code{logical} flag indicating whether presolve checks
#'   should be run prior solving the problem. These checks are performed using
#'   the \code{\link{presolve_check}} function. Defaults to \code{TRUE}.
#'   Skipping these checks may reduce run time for large problems.
#'
#' @param force \code{logical} flag indicating if an attempt to should be
#'   made to solve the problem even if potential issues were detected during
#'   the presolve checks. Defaults to \code{FALSE}.
#'
#' @param ... not used.
#'
#' @details Note that all arguments to \code{solution} must correspond
#'   to the planning unit data in the argument to \code{x} in terms
#'   of data representation, dimensionality, and spatial attributes (if
#'   applicable). This means that if the planning unit data in \code{x}
#'   is a \code{numeric} vector then the argument to \code{solution} must be a
#'   \code{numeric} vector with the same number of elements, if the planning
#'   unit data in \code{x} is a \code{\link[raster]{RasterLayer-class}} then the
#'   argument to \code{solution} must also be a
#'   \code{\link[raster]{RasterLayer-class}} with the same number of rows and
#'   columns and the same resolution, extent, and coordinate reference system,
#'   if the planning unit data in \code{x} is a \code{\link[sp]{Spatial-class}}
#'   object then the argument to \code{solution} must also be a
#'   \code{\link[sp]{Spatial-class}} object and have the same number of spatial
#'   features (e.g. polygons) and have the same coordinate reference system,
#'   if the planning units in \code{x} are a \code{data.frame} then the
#'   argument to \code{solution} must also be a \code{data.frame} with each
#'   column correspond to a different zone and each row correspond to
#'   a different planning unit, and values correspond to the allocations
#'   (e.g. values of zero or one).
#'
#'   Solutions must have planning unit statuses set to missing (\code{NA})
#'   values for planning units that have missing (\code{NA}) cost data. For
#'   problems with multiple zones, this means that planning units must have
#'   missing (\code{NA}) allocation values in zones where they have missing
#'   (\code{NA}) cost data. In other words, planning units that have missing
#'   (\code{NA}) cost values in \code{x} should always have a missing
#'   (\code{NA}) value the argument to \code{solution}. If an argument is
#'   supplied to
#'   \code{solution} where this is not the case, then an error will be thrown.
#'
#' @return A \code{numeric}, \code{matrix},
#'   \code{\link[raster]{RasterLayer-class}}, or
#'   \code{\link[sp]{Spatial-class}} object containing the replacement costs
#'   for each planning unit in the solution.
#'
#' @examples
#' # TODO
#'
#' @references
#' # TODO
#'
#' @name replacement_cost
NULL

#' @name replacement_cost
#'
#' @rdname replacement_cost
#'
#' @exportMethod replacement_cost
#'
methods::setGeneric("replacement_cost",
  function(x, solution, ...) {
  standardGeneric("replacement_cost")
})

internal_replacement_cost <- function(x, indices, solution_obj, run_checks,
                                      force) {
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          is.integer(indices), length(indices) > 0,
                          assertthat::is.flag(run_checks),
                          assertthat::is.flag(force))
  # assign default solver and portfolio
  if (inherits(x$solver, "Waiver"))
    x <- add_default_solver(x)
  x <- add_default_portfolio(x)
  # run presolve check to try to identify potential problems
  if (run_checks) {
    ch <- presolve_check(x)
    if (!isTRUE(force) && !isTRUE(ch))
      stop(paste("problem failed presolve checks. For more information see",
                 "?presolve_check"))
  }
  # generate objective value for solution if unknown
  if (is.null(solution_obj))
    solution_obj <- attr(solve(x), "objective")
  # compile problem into optimization problem object
  opt <- compile.ConservationProblem(x)
  old_ub <- opt$ub()
  # iterate over solution and store replacement costs
  alt_solution_obj <- vapply(indices, FUN.VALUE = numeric(1), function(i) {
    # lock out i'th selected planning unit in solution
    opt$set_ub(i, 0)
    # solve problem
    sol <- x$portfolio$run(opt, x$solver)
    # check that solution is valid
    if (is.null(sol) || is.null(sol[[1]]$x)) {
      out <- Inf
    } else {
      out <- sol[[1]][[2]]
    }
    # reset upper bound
    opt$set_ub(i, old_ub[i])
    # return result
    out
  })
  alt_solution_obj - solution_obj
}

#' @name replacement_cost
#' @usage \S4method{replacement_cost}{ConservationProblem,numeric}(x, solution)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution, run_checks = TRUE, force = FALSE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.numeric(solution), sum(solution, na.rm = TRUE) > 1e-10,
      is.numeric(x$data$cost), is.matrix(x$data$cost),
      number_of_total_units(x) == length(solution),
      number_of_zones(x) == 1,
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1,
      assertthat::is.flag(run_checks), assertthat::is.flag(force),
      no_extra_arguments(...))
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    pos2 <- which(!is.na(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    # calculate replacement costs
    indices <- which(solution[pos] > 1e-10)
    rc <- internal_replacement_cost(x, indices, attr(solution, "objective"),
                                    run_checks, force)
    # return replacement costs
    out <- rep(NA_real_, x$number_of_total_units())
    out[pos] <- 0
    out[pos[indices]] <- c(rc)
    out
})

#' @name replacement_cost
#' @usage \S4method{replacement_cost}{replacement_cost,matrix}(x, solution)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution, run_checks = TRUE, force = FALSE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.matrix(solution), is.numeric(solution),
      sum(solution, na.rm = TRUE) > 1e-10,
      is.matrix(x$data$cost), is.numeric(x$data$cost),
      number_of_total_units(x) == nrow(solution),
      number_of_zones(x) == ncol(solution),
      min(solution, na.rm = TRUE) >= 0,
      max(solution, na.rm = TRUE) <= 1,
      assertthat::is.flag(run_checks), assertthat::is.flag(force),
      no_extra_arguments(...))
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution)) != ncol(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_replacement_cost(x, indices, attr(solution, "objective"),
                                    run_checks, force)
    # return replacement costs
    out <- matrix(0, nrow = x$number_of_total_units(),
                  ncol = x$number_of_zones())
    if (x$number_of_zones() > 1) {
      colnames(out) <- paste0("rc_", x$zone_names())
    } else {
      colnames(out) <- "rc"
    }
    out[which(is.na(as.matrix(x$data$cost)))] <- NA_real_
    out[which(solution > 1e-10)] <- rc
    out
})

#' @name replacement_cost
#' @usage \S4method{replacement_cost}{ConservationProblem,data.frame}(x, solution)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution, run_checks = TRUE, force = FALSE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.data.frame(solution),
      sum(as.matrix(solution), na.rm = TRUE) >= 1e-10,
      number_of_zones(x) == ncol(solution),
      number_of_total_units(x) == nrow(solution),
      is.data.frame(x$data$cost),
      is.numeric(unlist(solution)),
      min(unlist(solution), na.rm = TRUE) >= 0,
      max(unlist(solution), na.rm = TRUE) <= 1,
      assertthat::is.flag(run_checks), assertthat::is.flag(force),
      no_extra_arguments(...))
    # subset planning units with finite cost values
    solution <- as.matrix(solution)
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution)) != ncol(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_replacement_cost(x, indices, attr(solution, "objective"),
                                    run_checks, force)
    # return replacement costs
    out <- matrix(0, nrow = x$number_of_total_units(),
                  ncol = x$number_of_zones())
    if (x$number_of_zones() > 1) {
      colnames(out) <- paste0("rc_", x$zone_names())
    } else {
      colnames(out) <- "rc"
    }
    pos <- which(is.na(as.matrix(as.data.frame(x$data$cost)[,
      x$data$cost_column, drop = FALSE])))
    out[pos] <- NA_real_
    out[which(solution > 1e-10)] <- rc
    tibble::as_tibble(out)
})

#' @name replacement_cost
#' @usage \S4method{replacement_cost}{ConservationProblem,Spatial}(x, solution)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution, run_checks = TRUE, force = FALSE, ...) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, c("SpatialPointsDataFrame", "SpatialLinesDataFrame",
                           "SpatialPolygonsDataFrame")),
      number_of_zones(x) == ncol(solution@data),
      number_of_total_units(x) == nrow(solution@data),
      class(x$data$cost)[1] == class(solution)[1],
      is.numeric(unlist(solution@data)),
      min(unlist(solution@data), na.rm = TRUE) >= 0,
      max(unlist(solution@data), na.rm = TRUE) <= 1,
      assertthat::is.flag(run_checks), assertthat::is.flag(force),
      no_extra_arguments(...))
    # subset planning units with finite cost values
    sp_solution <- solution
    solution <- as.matrix(solution@data)
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution)) != ncol(solution))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_replacement_cost(x, indices, attr(solution, "objective"),
                                    run_checks, force)
    # return replacement costs
    out <- matrix(0, nrow = x$number_of_total_units(),
                  ncol = x$number_of_zones())
    if (x$number_of_zones() > 1) {
      colnames(out) <- paste0("rc_", x$zone_names())
    } else {
      colnames(out) <- "rc"
    }
    pos <- which(is.na(as.matrix(as.data.frame(x$data$cost@data)[,
      x$data$cost_column, drop = FALSE])))
    out[pos] <- NA_real_
    out[which(solution > 1e-10)] <- rc
    out <- as.data.frame(out)
    rownames(out) <- rownames(sp_solution@data)
    sp_solution@data <- out
    sp_solution
})

#' @name replacement_cost
#' @usage \S4method{replacement_cost}{ConservationProblem,Raster}(x, solution)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution, run_checks = TRUE, force = FALSE, ...) {
    assertthat::assert_that(
      inherits(solution, "Raster"),
      number_of_zones(x) == raster::nlayers(solution),
      raster::compareCRS(x$data$cost@crs, solution@crs),
      is_comparable_raster(x$data$cost, solution[[1]]),
      min(raster::cellStats(solution, "min")) >= 0,
      max(raster::cellStats(solution, "max")) <= 1,
      assertthat::is.flag(run_checks), assertthat::is.flag(force),
      no_extra_arguments(...))
    # subset planning units with finite cost values
    pos <- x$planning_unit_indices()
    if (raster::nlayers(solution) > 1) {
      pos2 <- raster::Which(max(!is.na(solution)) == 1, cells = TRUE)
    } else {
      pos2 <- raster::Which(!is.na(solution), cells = TRUE)
    }
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution <- solution[pos2]
    if (!is.matrix(solution))
      solution <- matrix(solution, ncol = 1)
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
})
