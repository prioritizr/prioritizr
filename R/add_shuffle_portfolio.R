#' @include Portfolio-class.R
NULL

#' Add a shuffle portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' problem by randomly reordering the data prior to
#' solving the problem. Although this function can be useful for generating
#' multiple different solutions for a given problem,
#' it is recommended to use [add_pool_portfolio] if the *Gurobi*
#' software is available.
#'
#' @param x [problem()] object.
#'
#' @param number_solutions `integer` number of attempts to generate
#'   different solutions. Defaults to 10.
#'
#' @param threads `integer` number of threads to use for the generating
#'   the solution portfolio. Defaults to 1.
#'
#' @param remove_duplicates `logical` should duplicate solutions
#'   be removed? Defaults to `TRUE`.
#'
#' @details This strategy for generating a portfolio of solutions often
#'   results in different solutions, depending on optimality gap, but may
#'   return duplicate solutions. In general, this strategy is most effective
#'   when problems are quick to solve and multiple threads are available for
#'   solving each problem separately.
#'
#' @inherit add_cuts_portfolio return
#'
#' @seealso
#' See [portfolios] for an overview of all functions for adding a portfolio.
#'
#' @family portfolios
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem with shuffle portfolio
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_shuffle_portfolio(10, remove_duplicates = FALSE) %>%
#'   add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' # solve problem and generate 10 solutions within 20% of optimality
#' s1 <- solve(p1)
#'
#' # convert portfolio into a multi-layer raster
#' s1 <- terra::rast(s1)
#'
#' # print number of solutions found
#' print(terra::nlyr(s1))
#'
#' # plot solutions in portfolio
#' plot(s1, axes = FALSE)
#'
#' # build multi-zone conservation problem with shuffle portfolio
#' p2 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_shuffle_portfolio(10, remove_duplicates = FALSE) %>%
#'   add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # convert each solution in the portfolio into a single category layer
#' s2 <- terra::rast(lapply(s2, category_layer))
#'
#' # print number of solutions found
#' print(terra::nlyr(s2))
#'
#' # plot solutions in portfolio
#' plot(s2, axes = FALSE)
#' }
#' @name add_shuffle_portfolio
NULL

#' @rdname add_shuffle_portfolio
#' @export
add_shuffle_portfolio <- function(x, number_solutions = 10, threads = 1,
                                  remove_duplicates = TRUE) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(number_solutions)
  assert_required(threads)
  assert_required(remove_duplicates)
  assert(
    is_conservation_problem(x),
    assertthat::is.count(number_solutions),
    all_finite(number_solutions),
    is_thread_count(threads),
    all_finite(threads),
    assertthat::is.flag(remove_duplicates),
    assertthat::noNA(remove_duplicates)
  )
  # add portfolio
  x$add_portfolio(
    R6::R6Class(
      "ShufflePortfolio",
      inherit = Portfolio,
      public = list(
        name = "shuffle portfolio",
        data = list(
          number_solutions = number_solutions,
          threads = threads,
          remove_duplicates = remove_duplicates
        ),
        run = function(x, solver) {
          # determine behavior based on number of solutions
          if (self$get_data("number_solutions") == 1) {
            ## if only one solution is needed,
            ## then simply shuffle the problem and return the solution
            shuffle_key <- sample(seq_len(x$ncol()))
            reorder_key <- x$shuffle_columns(shuffle_key)
            if (!is.null(solver$data$start_solution)) {
              ## convert start solution to numeric vector
              ## and add extra NA values to represent decision variables
              ## that do not correspond to planning units
              n_extra <- x$ncol() - length(solver$data$start_solution)
              solver$data$start_solution <-
                c(c(solver$data$start_solution), rep(NA_real_, n_extra))
              ## shuffle the start solution
              solver$data$start_solution <-
                solver$data$start_solution[shuffle_key]
            }
            s <- solver$solve(x)
            s$x <- s$x[reorder_key]
            if (!is.null(solver$data$start_solution)) {
              solver$data$start_solution <-
                solver$data$start_solution[reorder_key]
            }
            return(list(s))
          }
          ## define worker function generating solutions
          generate_single_solution <- function(i) {
            ## copy optimization problem
            if (exists("x_list")) {
              x2 <- prioritizr::optimization_problem(x_list)
            } else {
              x2 <- x$copy()
            }
            ## shuffle problem
            shuffle_key <- sample(seq_len(x2$ncol()))
            reorder_key <- x2$shuffle_columns(shuffle_key)
            ## prepare stating solution
            if (!is.null(solver$data$start_solution)) {
              ### convert start solution to numeric vector
              ### and add extra NA values to represent decision variables
              ### that do not correspond to planning units
              n_extra <- x2$ncol() - length(solver$data$start_solution)
              solver$data$start_solution <-
                c(c(solver$data$start_solution), rep(NA_real_, n_extra))
              ### shuffle the start solution
              solver$data$start_solution <-
                solver$data$start_solution[shuffle_key]
            }
            ### solve problem
            s <- solver$solve(x2)
            ### prepare solution and re-order it
            s$x <- s$x[reorder_key]
            ## restore start solution to original order for next run
            if (!is.null(solver$data$start_solution)) {
              solver$data$start_solution <-
                solver$data$start_solution[reorder_key]
            }
            ### clean up
            rm(x2)
            ### return solution
            s
          }
          ## if multiple solutions are needed,
          ## then we need to be more complex
          if (self$get_data("threads") > 1L) {
            ### if using paralell processing, then...
            ### convert problem to list so we can copy between workers
            x_list <- as.list(x)
            ### initialize cluster
            cl <- parallel::makeCluster(self$get_data("threads"), "PSOCK")
            ### prepare cluster clean up
            on.exit(try(cl <- parallel::stopCluster(cl), silent = TRUE))
            ### create RNG seeds
            pids <- parallel::clusterEvalQ(cl, Sys.getpid())
            seeds <- sample.int(n = 1e+5, size = self$get_data("threads"))
            names(seeds) <- as.character(unlist(pids))
            ### copy data to cluster
            parallel::clusterExport(
              cl,
              c("solver", "x_list", "seeds"),
              envir = environment()
            )
            ### set up workers
            parallel::clusterEvalQ(cl, {
              ### initialize RNG on worker
              set.seed(seeds[as.character(Sys.getpid())]);
              NULL
            })
            ### main processing
            sol <- parallel::parLapply(
              cl = cl,
              seq_len(self$get_data("number_solutions")),
              generate_single_solution
            )
          } else {
            ### if NOT using paralell processing, then...
            sol <- lapply(
              seq_len(self$get_data("number_solutions")),
              generate_single_solution
            )
          }
          ## if needed, remove duplicated solutions
          if (isTRUE(self$get_data("remove_duplicates"))) {
            unique_pos <- !duplicated(
              vapply(
                lapply(sol, `[[`, 1),
                paste,
                character(1),
                collapse = " "
              )
            )
            sol <- sol[unique_pos]
          }
          ## return result
          sol
        }
      )
    )$new()
  )
}
