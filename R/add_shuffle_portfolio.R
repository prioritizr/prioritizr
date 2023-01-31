#' @include Portfolio-proto.R
NULL

#' Add a shuffle portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' [problem()] by randomly reordering the data prior to
#' solving the problem. This is recommended as a replacement for
#' [add_top_portfolio()] when the *Gurobi* software is not
#' available.
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
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features_zones <- get_sim_zones_features()
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
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
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
  rlang::check_required(x)
  rlang::check_required(number_solutions)
  rlang::check_required(threads)
  rlang::check_required(remove_duplicates)
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
  x$add_portfolio(pproto(
    "ShufflePortfolio",
    Portfolio,
    name = "Shuffle portfolio",
    parameters = parameters(
      integer_parameter("number_solutions", number_solutions, lower_limit = 1L),
      integer_parameter(
        "threads", threads, lower_limit = 1L,
        upper_limit = parallel::detectCores(TRUE)
      ),
      binary_parameter("remove_duplicates", as.integer(remove_duplicates))
    ),
    run = function(self, x, solver) {
      ## attempt initial solution for problem
      initial_sol <- solver$solve(x)
      # if solving the problem failed then return NULL
      if (is.null(initial_sol))
        return(initial_sol)
      if (self$parameters$get("number_solutions") == 1)
        return(list(initial_sol))
      ## generate additional solutions
      # convert OptimizationProblem to list
      x_list <- as.list(x)
      # prepare cluster for parallel processing
      if (self$parameters$get("threads") > 1L) {
        # initialize cluster
        cl <- parallel::makeCluster(self$parameters$get("threads"), "PSOCK")
        # create RNG seeds
        pids <- parallel::clusterEvalQ(cl, Sys.getpid())
        seeds <- sample.int(n = 1e+5, size = self$parameters$get("threads"))
        names(seeds) <- as.character(unlist(pids))
        # move data to workers
        parallel::clusterExport(
          cl, c("solver", "x_list", "seeds"), envir = environment()
        )
        # initalize RNG on workers
        parallel::clusterEvalQ(cl, {
          set.seed(seeds[as.character(Sys.getpid())])
          NULL
        })
        # set default cluster
        doParallel::registerDoParallel(cl)
      }
      sol <- plyr::llply(
        seq_len(self$parameters$get("number_solutions") - 1),
         .parallel = isTRUE(self$parameters$get("threads") > 1L),
         .fun = function(i) {
          # create and shuffle problem
          z <- prioritizr::predefined_optimization_problem(x_list)
          reorder_key <- z$shuffle_columns()
          # solve problem
          s <- solver$solve(z)
          # reorder variables
          s$x <- s$x[reorder_key]
          # return result
          s
        }
      )
      if (self$parameters$get("threads") > 1L) {
        doParallel::stopImplicitCluster()
        cl <- parallel::stopCluster(cl)
      }
      ## compile results
      sol <- append(list(initial_sol), sol)
      if (self$parameters$get("remove_duplicates")) {
        unique_pos <- !duplicated(
          vapply(lapply(sol, `[[`, 1), paste, character(1), collapse = " ")
        )
        sol <- sol[unique_pos]
      }
      return(sol)
    }
  ))
}
