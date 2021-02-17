#' @include internal.R pproto.R  ConservationProblem-proto.R OptimizationProblem-proto.R compile.R problem.R solve.R presolve_check.R
NULL

#' Evaluate solution importance using replacement cost scores
#'
#' Calculate importance scores for planning units selected in a solution
#' based on the replacement cost method (Cabeza and Moilanen 2006).
#'
#' @inheritParams eval_cost_summary
#'
#' @param rescale `logical` flag indicating if replacement cost
#'  values---excepting infinite (`Inf`) and zero values---should be
#'  rescaled to range between 0.01 and 1. Defaults to `TRUE`.
#'
#' @param run_checks `logical` flag indicating whether presolve checks
#'   should be run prior solving the problem. These checks are performed using
#'   the [presolve_check()] function. Defaults to `TRUE`.
#'   Skipping these checks may reduce run time for large problems.
#'
#' @param force `logical` flag indicating if an attempt to should be
#'   made to solve the problem even if potential issues were detected during
#'   the presolve checks. Defaults to `FALSE`.
#'
#' @param threads `integer` number of threads to use for the
#'   optimization algorithm. Defaults to 1 such that only a single
#'   thread is used.
#'
#' @param ... not used.
#'
#' @details
#' This function implements a modified version of the
#' replacement cost method (Cabeza and Moilanen 2006).
#' Specifically, the score for each planning unit is calculated
#' as the difference in the objective value of a solution when each planning
#' unit is locked out and the optimization processes rerun with all other
#' selected planning units locked in. In other words, the replacement cost
#' metric corresponds to change in solution quality incurred if a given
#' planning unit cannot be acquired when implementing the solution and the
#' next best planning unit (or set of planning units) will need to be
#' considered instead. Thus planning units with a higher score are more
#' important (and irreplaceable).
#' For example, when using the minimum set objective function
#' ([add_min_set_objective()]), the replacement cost scores
#' correspond to the additional costs needed to meet targets when each
#' planning unit is locked out. When using the maximum utility
#' objective function ([add_max_utility_objective()], the
#' replacement cost scores correspond to the reduction in the utility when
#' each planning unit is locked out. Infinite values mean that no feasible
#' solution exists when planning units are locked out---they are
#' absolutely essential for obtaining a solution (e.g. they contain rare
#' species that are not found in any other planning units or were locked in).
#' Zeros values mean that planning units can swapped with other planning units
#' and this will have no effect on the performance of the solution at all
#' (e.g. because they were only selected due to spatial fragmentation
#' penalties).
#'
#' These calculations can take a long time to complete for large
#' or complex conservation planning problems. As such, we using this
#' method for small or moderate-sized conservation planning problems
#' (e.g. < 30,000 planning units). To reduce run time, we
#' recommend calculating these scores without additional penalties (e.g.
#' [add_boundary_penalties()]) or spatial constraints (e.g.
#' [add_contiguity_constraints()]). To further reduce run time,
#' we recommend using proportion-type decisions when calculating the scores
#' (see below for an example).
#'
#' @inheritSection eval_cost_summary Solution format
#'
#' @return A `numeric`, `matrix`, `data.frame`
#'   [`RasterLayer-class`], [`Spatial-class`],
#'   or [sf::sf()] object containing the importance scores for each planning
#'   unit in the solution. Specifically, the returned object is in the
#'   same format as the planning unit data in the argument to `x`.
#'
#' @examples
#' \dontrun{
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
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#'
#' # calculate importance scores
#' rc1 <- eval_replacement_importance(p1, s1)
#'
#' # print importance scores
#' print(rc1)
#'
#' # plot importance scores
#' plot(rc1, main = "replacement cost", axes = FALSE, box = FALSE)
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
#' # calculate importance scores using proportion type decisions
#' rc2 <- eval_replacement_importance(p2, s1)
#'
#' # print importance scores based on proportion type decisions
#' print(rc2)
#'
#' # plot importance scores based on proportion type decisions
#' # we can see that the importance values in rc1 and rc2 are similar,
#' # and this confirms that the proportion type decisions are a good
#' # approximation
#' plot(rc2, main = "replacement cost", axes = FALSE, box = FALSE)
#'
#' # create minimal problem with polygon (sf) planning units
#' p3 <- problem(sim_pu_sf, sim_features, cost_column = "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.05) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # print solution
#' print(s3)
#'
#' # plot solution
#' plot(s3[, "solution_1"], main = "solution")
#'
#' # calculate importance scores
#' rc3 <- eval_rare_richness_importance(p3, s3[, "solution_1"])
#'
#' # plot importance scores
#' plot(rc3, main = "replacement cost")
#'
#' # build multi-zone conservation problem with raster data
#' p4 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve the problem
#' s4 <- solve(p4)
#'
#' # print solution
#' print(s4)
#'
#' # plot solution
#' # each panel corresponds to a different zone, and data show the
#' # status of each planning unit in a given zone
#' plot(s4, main = paste0("zone ", seq_len(nlayers(s4))), axes = FALSE,
#'      box = FALSE)
#'
#' # calculate importance scores
#' rc4 <- eval_replacement_importance(p4, s4)
#'
#' # plot importance
#' # each panel corresponds to a different zone, and data show the
#' # importance of each planning unit in a given zone
#' plot(rc4, main = paste0("zone ", seq_len(nlayers(s4))), axes = FALSE,
#'      box = FALSE)
#' }
#'
#' @references
#' Cabeza M and Moilanen A (2006) Replacement cost: A practical measure of site
#' value for cost-effective reserve planning. *Biological Conservation*,
#' 132: 336--342.
#'
#' @seealso [importance].
#'
#' @aliases eval_replacement_importance,ConservationProblem,numeric-method eval_replacement_importance,ConservationProblem,matrix-method eval_replacement_importance,ConservationProblem,data.frame-method eval_replacement_importance,ConservationProblem,Spatial-method eval_replacement_importance,ConservationProblem,sf-method eval_replacement_importance,ConservationProblem,Raster-method
#'
#' @name eval_replacement_importance
#'
#' @rdname eval_replacement_importance
#'
#' @exportMethod eval_replacement_importance
methods::setGeneric("eval_replacement_importance",
  function(x, solution, ...) {
  standardGeneric("eval_replacement_importance")
})

#' @name eval_replacement_importance
#' @usage \S4method{eval_replacement_importance}{ConservationProblem,numeric}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname eval_replacement_importance
methods::setMethod("eval_replacement_importance",
  methods::signature("ConservationProblem", "numeric"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.numeric(solution),
      no_extra_arguments(...))
    # extract planning unit solution status
    status <- planning_unit_solution_status(x, solution)
    # subset planning units with finite cost values
    idx <- x$planning_unit_indices()
    pos <- which(status > 1e-10)
    # calculate replacement costs
    v <- internal_eval_replacement_importance(
      x, pos, rescale, run_checks, force, threads)
    # return replacement costs
    out <- rep(NA_real_, x$number_of_total_units())
    out[idx] <- 0
    out[idx[pos]] <- c(v)
    out
})

#' @name eval_replacement_importance
#' @usage \S4method{eval_replacement_importance}{ConservationProblem,matrix}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname eval_replacement_importance
methods::setMethod("eval_replacement_importance",
  methods::signature("ConservationProblem", "matrix"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.matrix(solution), is.numeric(solution),
      no_extra_arguments(...))
    # extract data
    status <- planning_unit_solution_status(x, solution)
    # extract planning units in solution
    pos <- which(status > 1e-10)
    # calculate replacement costs
    v <- internal_eval_replacement_importance(
      x, pos, rescale, run_checks, force, threads)
    # initialize matrix
    m_total <- matrix(NA_real_, nrow = x$number_of_total_units(),
                      ncol = x$number_of_zones())
    m_pu <- matrix(0, nrow = x$number_of_planning_units(),
                   ncol = x$number_of_zones())
    m_pu[pos] <- c(v)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    m_total[x$planning_unit_indices(), ] <- m_pu
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_total) <- paste0("rc_", x$zone_names())
    } else {
      colnames(m_total) <- "rc"
    }
    # return result
    m_total
})

#' @name eval_replacement_importance
#' @usage \S4method{eval_replacement_importance}{ConservationProblem,data.frame}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname eval_replacement_importance
methods::setMethod("eval_replacement_importance",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
    # assert valid arguments
    assertthat::assert_that(
      is.data.frame(solution),
      no_extra_arguments(...))
    # extract data
    status <- planning_unit_solution_status(x, solution)
    # extract planning units in solution
    pos <- which(status > 1e-10)
    # calculate replacement costs
    v <- internal_eval_replacement_importance(
      x, pos, rescale, run_checks, force, threads)
    # initialize matrix
    m_total <- matrix(NA_real_, nrow = x$number_of_total_units(),
                      ncol = x$number_of_zones())
    m_pu <- matrix(0, nrow = x$number_of_planning_units(),
                   ncol = x$number_of_zones())
    m_pu[pos] <- c(v)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    m_total[x$planning_unit_indices(), ] <- m_pu
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_total) <- paste0("rc_", x$zone_names())
    } else {
      colnames(m_total) <- "rc"
    }
    # return result
    tibble::as_tibble(m_total)
})

#' @name eval_replacement_importance
#' @usage \S4method{eval_replacement_importance}{ConservationProblem,Spatial}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname eval_replacement_importance
methods::setMethod("eval_replacement_importance",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, c("SpatialPointsDataFrame", "SpatialLinesDataFrame",
                           "SpatialPolygonsDataFrame")),
      no_extra_arguments(...))
    # extract data
    status <- planning_unit_solution_status(x, solution)
    # extract planning units in solution
    pos <- which(status > 1e-10)
    # calculate replacement costs
    v <- internal_eval_replacement_importance(
      x, pos, rescale, run_checks, force, threads)
    # initialize matrix
    m_total <- matrix(NA_real_, nrow = x$number_of_total_units(),
                      ncol = x$number_of_zones())
    m_pu <- matrix(0, nrow = x$number_of_planning_units(),
                   ncol = x$number_of_zones())
    m_pu[pos] <- c(v)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    m_total[x$planning_unit_indices(), ] <- m_pu
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_total) <- paste0("rc_", x$zone_names())
    } else {
      colnames(m_total) <- "rc"
    }
    # return result
    out <- as.data.frame(m_total)
    rownames(out) <- rownames(solution@data)
    solution@data <- out
    solution
})

#' @name eval_replacement_importance
#' @usage \S4method{eval_replacement_importance}{ConservationProblem,sf}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname eval_replacement_importance
methods::setMethod("eval_replacement_importance",
  methods::signature("ConservationProblem", "sf"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(solution, "sf"),
      no_extra_arguments(...))
    # extract data
    status <- planning_unit_solution_status(x, solution)
    # extract planning units in solution
    pos <- which(status > 1e-10)
    # calculate replacement costs
    v <- internal_eval_replacement_importance(
      x, pos, rescale, run_checks, force, threads)
    # initialize matrix
    m_total <- matrix(NA_real_, nrow = x$number_of_total_units(),
                      ncol = x$number_of_zones())
    m_pu <- matrix(0, nrow = x$number_of_planning_units(),
                   ncol = x$number_of_zones())
    m_pu[pos] <- c(v)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    m_total[x$planning_unit_indices(), ] <- m_pu
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_total) <- paste0("rc_", x$zone_names())
    } else {
      colnames(m_total) <- "rc"
    }
    # return result
    out <- tibble::as_tibble(as.data.frame(m_total))
    sf::st_as_sf(
      out, geometry = sf::st_geometry(x$data$cost),
      crs = sf::st_crs(x$data$cost))
})

#' @name eval_replacement_importance
#' @usage \S4method{eval_replacement_importance}{ConservationProblem,Raster}(x, solution, rescale, run_checks, force, threads, ...)
#' @rdname eval_replacement_importance
methods::setMethod("eval_replacement_importance",
  methods::signature("ConservationProblem", "Raster"),
  function(x, solution, rescale = TRUE, run_checks = TRUE, force = FALSE,
           threads = 1L, ...) {
    assertthat::assert_that(
      inherits(solution, "Raster"),
      no_extra_arguments(...))
    # extract data
    status <- planning_unit_solution_status(x, solution)
    # extract planning units in solution
    pos <- which(status > 1e-10)
    # calculate replacement costs
    v <- internal_eval_replacement_importance(
      x, pos, rescale, run_checks, force, threads)
    # initialize matrix
    m_pu <- matrix(0, nrow = x$number_of_planning_units(),
                   ncol = x$number_of_zones())
    m_pu[pos] <- c(v)
    m_pu[is.na(x$planning_unit_costs())] <- NA_real_
    # add column names to matrix
    if (x$number_of_zones() > 1) {
      colnames(m_pu) <- paste0("rc_", x$zone_names())
    } else {
      colnames(m_pu) <- "rc"
    }
    # return result
    out <- raster::as.list(solution)
    for (i in seq_along(out)) {
      out[[i]][x$planning_unit_indices()] <- m_pu[, i]
      out[[i]][raster::Which(is.na(solution[[i]]), cells = TRUE)] <- NA_real_
    }
    if (length(out) > 1) {
      out <- raster::stack(out)
    } else {
      out <- out[[1]]
    }
    names(out) <- colnames(m_pu)
    out
})

internal_eval_replacement_importance <- function(
  x, indices, rescale, run_checks, force, threads = 1L) {
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
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
  # construct problem
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
  alt_solution_obj <- plyr::llply(distribute_load(length(indices), threads),
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
