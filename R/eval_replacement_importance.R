#' @include internal.R ConservationProblem-class.R OptimizationProblem-class.R compile.R problem.R solve.R presolve_check.R planning_unit_solution_format.R
NULL

#' Evaluate solution importance using replacement cost scores
#'
#' Calculate importance scores for planning units selected in a solution
#' based on the replacement cost method (Cabeza and Moilanen 2006).
#'
#' @inheritParams eval_cost_summary
#'
#' @param rescale `logical` flag indicating if replacement cost
#'  values -- excepting infinite (`Inf`) and zero values -- should be
#'  rescaled to range between 0.01 and 1. Defaults to `TRUE`.
#'
#' @param run_checks `logical` flag indicating whether presolve checks
#'   should be run prior solving the problem. These checks are performed using
#'   the [presolve_check()] function. Defaults to `TRUE`.
#'   Skipping these checks may reduce run time for large problems.
#'
#' @param force `logical` flag indicating if an attempt should be
#'   made to solve the problem even if potential issues were detected during
#'   the presolve checks. Defaults to `FALSE`.
#'
#' @param threads `integer` number of threads to use for the
#'   optimization algorithm. Defaults to 1 such that only a single
#'   thread is used.
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
#' absolutely essential for obtaining a solution (e.g., they contain rare
#' species that are not found in any other planning units or were locked in).
#' Zeros values mean that planning units can be swapped with other planning
#' units and this will have no effect on the performance of the solution at all
#' (e.g., because they were only selected due to spatial fragmentation
#' penalties).
#'
#' These calculations can take a long time to complete for large
#' or complex conservation planning problems. As such, we recommend using this
#' method for small or moderate-sized conservation planning problems
#' (e.g., < 30,000 planning units). To reduce run time, we
#' recommend calculating these scores without additional penalties (e.g.,
#' [add_boundary_penalties()]) or spatial constraints (e.g.,
#' [add_contiguity_constraints()]). To further reduce run time,
#' we recommend using proportion-type decisions when calculating the scores
#' (see below for an example).
#'
#' @inheritSection eval_cost_summary Solution format
#'
#' @return A `numeric`, `matrix`, `data.frame`,
#'   [terra::rast()], or [sf::sf()] object
#'   containing the importance scores for each planning
#'   unit in the solution. Specifically, the returned object is in the
#'   same format as the planning unit data in the argument to `x`.
#'
#' @seealso
#' See [importance] for an overview of all functions for evaluating
#' the importance of planning units selected in a solution.
#.
#' @family importances
#'
#' @examples
#' \dontrun{
#' # seed seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem with binary decisions
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # calculate importance scores
#' rc1 <- eval_replacement_importance(p1, s1)
#'
#' # print importance scores
#' print(rc1)
#'
#' # plot importance scores
#' plot(rc1, main = "replacement cost", axes = FALSE)
#'
#' # since replacement cost scores can take a long time to calculate with
#' # binary decisions, we can calculate them using proportion-type
#' # decision variables. Note we are still calculating the scores for our
#' # previous solution (s1), we are just using a different optimization
#' # problem when calculating the scores.
#' p2 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_proportion_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
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
#' plot(rc2, main = "replacement cost", axes = FALSE)
#'
#' # create minimal problem with polygon planning units
#' p3 <-
#'   problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
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
#' p4 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve the problem
#' s4 <- solve(p4)
#' names(s4) <- paste0("zone ", seq_len(terra::nlyr(s4)))
#'
#' # print solution
#' print(s4)
#'
#' # plot solution
#' # each panel corresponds to a different zone, and data show the
#' # status of each planning unit in a given zone
#' plot(s4, axes = FALSE)
#'
#' # calculate importance scores
#' rc4 <- eval_replacement_importance(p4, s4)
#' names(rc4) <- paste0("zone ", seq_len(terra::nlyr(s4)))
#'
#' # plot importance
#' # each panel corresponds to a different zone, and data show the
#' # importance of each planning unit in a given zone
#' plot(rc4, axes = FALSE)
#' }
#'
#' @references
#' Cabeza M and Moilanen A (2006) Replacement cost: A practical measure of site
#' value for cost-effective reserve planning. *Biological Conservation*,
#' 132: 336--342.
#'
#' @export
eval_replacement_importance <- function(x, solution, rescale = TRUE,
                                        run_checks = TRUE, force = FALSE,
                                        threads = 1L) {
  # assert valid arguments
  assert_required(x)
  assert_required(solution)
  assert_required(rescale)
  assert_required(run_checks)
  assert_required(force)
  assert_required(threads)
  assert(
    is_conservation_problem(x),
    is_inherits(
      solution,
      c(
        "numeric", "data.frame", "matrix", "sf", "SpatRaster",
        "Spatial", "Raster"
      )
    ),
    assertthat::is.flag(rescale),
    assertthat::is.flag(run_checks),
    assertthat::is.flag(force),
    is_thread_count(threads)
  )
  # extract planning unit solution status
  status <- planning_unit_solution_status(x, solution)
  # calculate replacement costs
  v <- internal_eval_replacement_importance(
    x, status, rescale, run_checks, force, threads
  )
  # return formatted values
  planning_unit_solution_format(x, v, solution, prefix = "rc")
}

internal_eval_replacement_importance <- function(x, status, rescale,
                                                 run_checks, force,
                                                 threads = 1,
                                                 call = fn_caller_env()) {
  # assert valid arguments
  assert(
    is.numeric(status),
    is.matrix(status),
    call = call,
    .internal = TRUE
  )
  # extract indices for solution
  indices <- which(status > 1e-10)
  # assign default solver
  if (inherits(x$solver, "Waiver"))
    x <- add_default_solver(x)
  # overwrite portfolio
  x <- add_shuffle_portfolio(x, 1)
  # compile problem
  opt <- compile.ConservationProblem(x)
  # run presolve check to try to identify potential problems
  if (run_checks) {
    ## run checks
    presolve_res <- internal_presolve_check(opt)
    ## prepare message
    msg <- presolve_res$msg
    if (!isTRUE(force)) {
      msg <- c(
        msg,
        "i" = paste(
          "To ignore checks and attempt optimization anyway,",
          "use {.code solve(force = TRUE)}."
        )
      )
    }
    ## determine if error or warning should be thrown
    if (!isTRUE(force)) {
      f <- assert
    } else {
      f <- verify
    }
    ## throw error or warning if checks failed
    f(isTRUE(presolve_res$pass), call = parent.frame(), msg = msg)
  }
  # solve problem
  x$solver$calculate(opt)
  modelsense <- opt$modelsense()
  rm(opt)
  # lock in decisions in the solution
  x$solver$set_variable_lb(indices, rep.int(1, length(indices)))
  x$solver$set_variable_ub(indices, rep.int(1, length(indices)))
  # generate objective value for solution if unknown
  solution_obj <- x$solver$run()
  if (is.null(solution_obj) || is.null(solution_obj$x))
    cli::cli_abort(
      "{.arg solution} is not feasible solution for the problem {.arg x}",
      call = call
    )
  solution_obj <- solution_obj[[2]]
  # define function for processing
  calculate_alt_solution_obj <- function(y) {
    vapply(
      indices[y],
      FUN.VALUE = numeric(1),
      function(i) {
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
      }
    )
  }
  # iterate over decision variables in solution and store new objective values
  if (isTRUE(threads > 1L)) {
    ## initialize cluster
    cl <- parallel::makeCluster(threads, "PSOCK")
    ## prepare cluster clean up
    on.exit(try(cl <- parallel::stopCluster(cl), silent = TRUE))
    ## move data to workers
    parallel::clusterExport(
      cl,
      c("indices", "x", "calculate_alt_solution_obj"),
      envir = environment()
    )
    ## main processing
    alt_solution_obj <- parallel::parLapply(
      cl,
      parallel::splitIndices(length(indices), threads),
      calculate_alt_solution_obj
    )
  } else {
    ## main processing
    alt_solution_obj <- lapply(
      parallel::splitIndices(length(indices), threads),
      calculate_alt_solution_obj
    )
  }
  # reformat output
  alt_solution_obj <- unlist(
    alt_solution_obj, recursive = TRUE, use.names = FALSE
  )
  # calculate replacement costs
  out <- alt_solution_obj - solution_obj
  if (identical(modelsense, "max")) # rescale values if maximization problem
    out <- out * -1
  # rescale values if specified
  if (rescale) {
    rescale_ind <- is.finite(out) & (abs(out) > 1e-10)
    out[rescale_ind] <- rescale(out[rescale_ind], to = c(0.01, 1))
  }
  # convert to solution status format
  convert_raw_solution_to_solution_status(x, out, indices)
}
