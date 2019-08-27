#' @include internal.R pproto.R  ConservationProblem-proto.R OptimizationProblem-proto.R compile.R problem.R solve.R presolve_check.R
NULL

#' Replacement cost
#'
#' Calculate irreplaceability scores for planning units selected in a solution
#' using the replacement cost method (Cabeza and Moilanen 2006).
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param solution \code{numeric}, \code{matrix}, \code{data.frame},
#'   \code{\link[raster]{Raster-class}}, or \code{\link[sp]{Spatial-class}}
#'   object. See the Details section for more information.
#'
#' @param rescale \code{logical} flag indicating if replacement cost
#'  values---excepting infinite (\code{Inf}) and zero values---should be
#'  rescaled to range between 0.01 and 1. Defaults to \code{TRUE}.
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
#' @param threads \code{integer} number of threads to use for the
#'   optimization algorithm. The default value of 1 will result in only
#'   one thread being used.
#'
#' @param ... not used.
#'
#' @details Using this method, the score for each planning unit is calculated
#'   as the difference in the objective value of a solution when each planning
#    unit is locked out and the optimization processes rerun with all other
#'   selected planning units locked in. In other words, the replacement cost
#'   metric corresponds to change in solution quality incurred if a given
#'   planning unit cannot be acquired when implementing the solution and the
#'   next best planning unit (or set of planning units) will need to be
#'   considered instead. Thus planning units with a higher score are more
#'   irreplaceable. For example, when using the minimum set objective function
#'   (\code{\link{add_min_set_objective}}), the replacement cost scores
#'   correspond to the additional costs needed to meet targets when each
#'   planning unit is locked out. When using the maximum utility
#'   objective function (\code{\link{add_max_utility_objective}}, the
#'   replacement cost scores correspond to the reduction in the utility when
#'   each planning unit is locked out. Infinite values mean that no feasible
#'   solution exists when planning units are locked out---they are
#'   absolutely essential for obtaining a solution (e.g. they contain rare
#'   species that are not found in any other planning units or were locked in).
#'   Zeros values mean that planning units can swapped with other planning units
#'   and this will have no effect on the performance of the solution at all
#'   (e.g. because they were only selected due to spatial fragmentation
#'   penalties). Since these calculations can take a long time to complete, we
#'   recommend calculating these scores without additional penalties (e.g.
#'   \code{\link{add_boundary_penalties}}) or constraints (e.g.
#'   \code{link{add_contiguity_constraints}}). They can be sped up further by
#'   using proportion-type decisions when calculating the scores (see below for
#'   an example).
#'
#'   Note that all arguments to \code{solution} must correspond
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
#' # seed seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem with binary decisions
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#'
#' # calculate irreplaceability scores
#' rc1 <- replacement_cost(p1, s1)
#'
#' # print irreplaceability scores
#' print(rc1)
#'
#' # plot irreplaceability scores
#' plot(rc1, main = "replacement cost", axes = FALSE, box = FALSE)
#' }
#'
#' # since replacement cost scores can take a long time to calculate with
#' # binary decisions, we can calculate them using proportion-type
#' # decision variables. Note we are still calculating the scores for our
#' # previous solution (s1), we are just using a different optimization
#' # problem when calculating the scores.
#' p2 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_proportion_decisions() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#'
#' # calculate irreplaceability scores using proportion type decisions
#' \donttest{
#' rc2 <- replacement_cost(p2, s1)
#'
#' # print irreplaceability scores based on proportion type decisions
#' print(rc2)
#'
#' # plot irreplacability scores based on proportion type decisions
#' # we can see that the irreplaceability values in rc1 and rc2 are similar,
#' # and this confirms that the proportion type decisions are a good
#' # approximation
#' plot(rc2, main = "replacement cost", axes = FALSE, box = FALSE)
#' }
#'
#' # build multi-zone conservation problem with binary decisions
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#' \donttest{
#' # solve the problem
#' s3 <- solve(p3)
#'
#' # print solution
#' print(s3)
#'
#' # plot solution
#' # each panel corresponds to a different zone, and data show the
#' # status of each planning unit in a given zone
#' plot(s3, main = paste0("zone ", seq_len(nlayers(s3))), axes = FALSE,
#'      box = FALSE)
#'
#' # calculate irreplaceability scores
#' rc3 <- replacement_cost(p3, s3)
#'
#' # plot  irreplaceability
#' # each panel corresponds to a different zone, and data show the
#' # irreplaceability of each planning unit in a given zone
#' plot(rc3, main = paste0("zone ", seq_len(nlayers(s3))), axes = FALSE,
#'      box = FALSE)
#' }
#'
#' @references
#' Cabeza M and Moilanen (2006) Replacement cost: A practical measure of site
#' value for cost-effective reserve planning. \emph{Biological Conservation},
#' 132:  336--342.
#'
#' @seealso \code{\link{irreplaceability}}.
#'
#' @aliases replacement_cost,ConservationProblem,numeric-method replacement_cost,ConservationProblem,matrix-method replacement_cost,ConservationProblem,data.frame-method replacement_cost,ConservationProblem,Spatial-method replacement_cost,ConservationProblem,Raster-method
#'
#' @name replacement_cost
#'
#' @rdname replacement_cost
#'
#' @exportMethod replacement_cost
methods::setGeneric("replacement_cost",
  function(x, solution, ...) {
  standardGeneric("replacement_cost")
})

internal_replacement_cost <- function(x, indices, rescale, run_checks, force,
                                      threads = 1L) {
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          is.integer(indices), length(indices) > 0,
                          assertthat::is.flag(rescale),
                          assertthat::is.flag(run_checks),
                          assertthat::is.flag(force),
                          isTRUE(all(is.finite(threads))),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
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
  # contruct problem
  opt <- compile.ConservationProblem(x)
  x$solver$calculate(opt)
  modelsense <- opt$modelsense()
  rm(opt)
  # lock in decisions in the solution
  x$solver$set_variable_lb(indices, rep.int(1, length(indices)))
  x$solver$set_variable_ub(indices, rep.int(1, length(indices)))
  # generate objective value for solution if unknown
  solution_obj <- x$solver$run()
  if (is.null(solution_obj) || is.null(solution_obj$x))
    stop("argument to solution is infeasible for this problem")
  solution_obj <- solution_obj[[2]]
  # prepare cluster for parallel processing
  if (isTRUE(threads > 1L)) {
    # initialize cluster
    cl <- parallel::makeCluster(threads, "PSOCK")
    # move data to workers
    parallel::clusterExport(cl, c("indices", "x"), envir = environment())
    # set default cluster
    doParallel::registerDoParallel(cl)
  }
  # iterate over decision variables in solution and store new objective values
  alt_solution_obj <- plyr::llply(distribute_load(length(indices)),
                                  .parallel = isTRUE(threads > 1),
                                  .fun = function(y) {
    vapply(indices[y], FUN.VALUE = numeric(1), function(i) {
      # lock out i'th selected planning unit in solution
      x$solver$set_variable_lb(i, 0)
      x$solver$set_variable_ub(i, 0)
      # solve problem
      sol <- x$solver$run()
      # check that solution is valid
      if (is.null(sol) || is.null(sol$x)) {
        out <- Inf
      } else {
        out <- sol[[2]]
      }
      # reset upper bound
      x$solver$set_variable_lb(i, 1)
      x$solver$set_variable_ub(i, 1)
      # return result
      out
    })
  })
  alt_solution_obj <- unlist(alt_solution_obj, recursive = TRUE,
                             use.names = FALSE)
  # kill works if needed
  if (isTRUE(threads > 1L)) {
    doParallel::stopImplicitCluster()
    cl <- parallel::stopCluster(cl)
  }
  # calculate replacement costs
  out <- alt_solution_obj - solution_obj
  if (identical(modelsense, "max")) # rescale values if maximization problem
    out <- out * -1
  # rescale values if specified
  if (rescale) {
    rescale_ind <- is.finite(out) & (abs(out) > 1e-10)
    out[rescale_ind] <- rescale(out[rescale_ind], to = c(0.01, 1))
  }
  # return result
  out
}

#' @name replacement_cost
#' @usage \S4method{replacement_cost}{ConservationProblem,numeric}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
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
    rc <- internal_replacement_cost(x, indices, rescale, run_checks, force,
                                    threads)
    # return replacement costs
    out <- rep(NA_real_, x$number_of_total_units())
    out[pos] <- 0
    out[pos[indices]] <- c(rc)
    out
})

#' @name replacement_cost
#' @usage \S4method{replacement_cost}{ConservationProblem,matrix}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
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
    rc <- internal_replacement_cost(x, indices, rescale, run_checks, force,
                                    threads)
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
#' @usage \S4method{replacement_cost}{ConservationProblem,data.frame}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
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
    solution_matrix <- as.matrix(solution)
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution_matrix)) != ncol(solution_matrix))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution_matrix[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_replacement_cost(x, indices, rescale, run_checks, force,
                                    threads)
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
#' @usage \S4method{replacement_cost}{ConservationProblem,Spatial}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
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
    solution_matrix <- as.matrix(solution@data)
    pos <- x$planning_unit_indices()
    pos2 <- which(rowSums(is.na(solution_matrix)) != ncol(solution_matrix))
    if (!setequal(pos, pos2))
      stop("planning units with NA cost data must have NA allocations in the",
           " solution")
    solution_pu <- solution_matrix[pos, , drop = FALSE]
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_pu))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_pu > 1e-10)
    rc <- internal_replacement_cost(x, indices, rescale, run_checks, force,
                                    threads)
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
    out[which(solution_matrix > 1e-10)] <- rc
    out <- as.data.frame(out)
    rownames(out) <- rownames(solution@data)
    solution@data <- out
    solution
})

#' @name replacement_cost
#' @usage \S4method{replacement_cost}{ConservationProblem,Raster}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname replacement_cost
methods::setMethod("replacement_cost",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
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
    solution_matrix <- solution[pos2]
    if (!is.matrix(solution_matrix))
      solution_matrix <- matrix(solution_matrix, ncol = 1)
    if (!all(is.na(c(x$planning_unit_costs())) == is.na(c(solution_matrix))))
     stop("planning units with NA cost data must have NA allocations in the",
          " solution")
    # calculate replacement costs
    indices <- which(solution_matrix > 1e-10)
    rc <- internal_replacement_cost(x, indices, rescale, run_checks, force,
                                    threads)
    # prepare output
    rc <- split(rc, which(solution_matrix > 1e-10, arr.ind = TRUE)[, 2])
    # return result
    out <- as.list(solution)
    if (x$number_of_zones() > 1) {
      names(out) <- paste0("rc_", x$zone_names())
    } else {
      names(out) <- "rc"
    }
    for (i in seq_along(out)) {
      out[[i]][!is.na(out[[i]])] <- 0
      out[[i]][solution[[i]] > 1e-10] <- rc[[i]]
    }
    if (length(out) > 1) {
      out <- raster::stack(out)
    } else {
      out <- out[[1]]
    }
    out
})
