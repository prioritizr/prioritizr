#' @include Portfolio-proto.R
NULL

#' Add a shuffle portfolio
#'
#' Generate a portfolio of solutions by randomly reordering the data prior to #' attempting to solve the problem.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param number_solutions \code{integer} number of attempts to generate
#'   different solutions. Defaults to 10.
#'
#' @param threads \code{integer} number of threads to use for the generating
#'   the solution portfolio. Defaults to 1.
#'
#' @param remove_duplicates \code{logical} should duplicate solutions
#'   be removed? Defaults to \code{TRUE}.
#'
#' @details This strategy for generating a portfolio of solutions often
#'   results in different solutions, depending on optimality gap, but may
#'   return duplicate solutions. In general, this strategy is most effective
#'   when problems are quick to solve and multiple threads are available for
#'   solving each problem separately.
#'
#' @seealso \code{\link{portfolios}}.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_shuffle_portfolio(10, remove_duplicates = FALSE) %>%
#'       add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' \donttest{
#' # solve problem and generate 10 solutions within 20 % of optimality
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(stack(s1), axes = FALSE, box = FALSE)
#' }
#' # build multi-zone conservation problem with raster data
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2),
#'                                   nrow = n_feature(sim_features_zones),
#'                                   ncol = n_zone(sim_features_zones))) %>%
#'       add_binary_decisions() %>%
#'       add_shuffle_portfolio(10, remove_duplicates = FALSE) %>%
#'       add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' \donttest{
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' str(s2, max.level = 1)
#'
#' # plot first solution in portfolio
#' plot(category_layer(s2[[1]]), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_shuffle_portfolio
#'
NULL

#' @export
methods::setClass("ShufflePortfolio", contains = "Portfolio")

#' @rdname add_shuffle_portfolio
#' @export
add_shuffle_portfolio <- function(x, number_solutions = 10L, threads = 1L,
                                  remove_duplicates = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          assertthat::is.count(number_solutions),
                          isTRUE(all(is.finite(number_solutions))),
                          assertthat::is.count(threads),
                          isTRUE(all(is.finite(threads))),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          assertthat::is.flag(remove_duplicates))
  # add solver
  x$add_portfolio(pproto(
    "ShufflePortfolio",
    Portfolio,
    name = "Shuffle portfolio",
    parameters = parameters(
      integer_parameter("number_solutions", number_solutions, lower_limit = 1L),
      integer_parameter("threads", threads, lower_limit = 1L,
                        upper_limit = parallel::detectCores(TRUE)),
      binary_parameter("remove_duplicates", remove_duplicates)),
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
        parallel::clusterExport(cl, c("solver", "x_list", "seeds"),
                                envir = environment())
        # initalize RNG on workers
        parallel::clusterEvalQ(cl, {
          set.seed(seeds[as.character(Sys.getpid())])
          NULL
        })
        # set default cluster
        doParallel::registerDoParallel(cl)
      }
      sol <- plyr::llply(seq_len(self$parameters$get("number_solutions") - 1),
                         .parallel = isTRUE(self$parameters$get("threads") >
                                            1L),
                         .fun = function(i) {
        # create and shuffle problem
        z <- prioritizr::predefined_optimization_problem(x_list)
        reorder_key <- z$shuffle_columns()
        # solve problem
        s <- solver$solve(z)
        # reorder variables
        s$x <- s$x[reorder_key]
        return(s)
      })
      if (self$parameters$get("threads") > 1L) {
        doParallel::stopImplicitCluster()
        cl <- parallel::stopCluster(cl)
      }
      ## compile results
      sol <- append(list(initial_sol), sol)
      if (self$parameters$get("remove_duplicates")) {
        unique_pos <- !duplicated(vapply(lapply(sol, `[[`, 1),
                                         paste, character(1), collapse = " "))
        sol <- sol[unique_pos]
      }
      return(sol)
    }
))}
