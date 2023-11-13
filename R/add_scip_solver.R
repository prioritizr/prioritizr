#' @include Solver-class.R
NULL

#' Add a *SCIP* solver
#'
#' Specify that the [*SCIP*](https://www.scipopt.org/) software
#' should be used to solve a conservation planning problem
#' (Bestuzheva *et al.* 2021). This function can also be used to
#' customize the behavior of the solver.
#' It requires the \pkg{rscip} package to be installed
#' (see below for installation instructions).
#'
#' @inheritParams add_cbc_solver
#'
#' @section Installation:
#' The \pkg{rscip} package is required to use this solver. Since the
#' \pkg{rscip} package is not available on the
#' the Comprehensive R Archive Network (CRAN), it must be installed from
#' [its GitHub repository](https://github.com/jeffreyhanson/rscip). To
#' install the \pkg{rscip} package, please use the following code:
#' ```
#' if (!require(remotes)) install.packages("remotes")
#' remotes::install_github("jeffreyhanson/rscip")
#' ```
#' Note that you may also need to install several dependencies --
#' such as the
#' [Rtools software](https://cran.r-project.org/bin/windows/Rtools/)
#' or system libraries -- prior to installing the \pkg{rcbc} package.
#' For further details on installing this package, please consult the
#' [online package documentation](https://jeffreyhanson.github.io/rscip/).
#'
#' @family solvers
#'
#' @references
#' Bestuzheva K., Besançon M., Chen W-K, Chmiela A., Donkiewicz T., van
#' Doornmalen J., Eifler L., Gaul O., Gamrath G., Gleixner A., Gottwald L.,
#' Graczyk C.,  Halbig K., Hoen A., Hojny C., van der Hulst R., Koch T.,
#' Lübbecke M., Maher S.J., Matter F., Mühmer E., Müller B., Pfetsch M.E.,
#' Rehfeldt D., Schlein S., Schlösser F., Serrano F., Shinano Y., Sofranac B.,
#' Turner M, Vigerske S.,  Wegscheider F., Wellner P., Weninger D., and
#' Witzig J. (2021) The SCIP Optimization Suite 8.0. Available at Optimization
#' Online and as ZIB-Report 21-41. <http://www.optimization-online.org/DB_HTML/2021/12/8728.html>

#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create problem
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_scip_solver(gap = 0, verbose = FALSE)
#'
#' # generate solution
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # create a similar problem with boundary length penalties and
#' # specify the solution from the previous run as a starting solution
#' p2 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_boundary_penalties(10) %>%
#'   add_binary_decisions() %>%
#'   add_scip_solver(gap = 0, start_solution = s1, verbose = FALSE)
#'
#' # generate solution
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution with boundary penalties", axes = FALSE)
#' }
#' @name add_scip_solver
NULL

#' @rdname add_scip_solver
#' @export
add_gurobi_solver <- function(x, gap = 0.1, time_limit = .Machine$integer.max,
                              presolve = TRUE, threads = 1,
                              first_feasible = FALSE,
                              start_solution = NULL, verbose = TRUE) {
  # assert that arguments are valid (except start_solution)
  assert_required(x)
  assert_required(gap)
  assert_required(time_limit)
  assert_required(presolve)
  assert_required(threads)
  assert_required(first_feasible)
  assert_required(start_solution)
  assert_required(verbose)
  assert(
    is_conservation_problem(x),
    assertthat::is.number(gap),
    all_finite(gap),
    gap >= 0,
    assertthat::is.count(time_limit),
    all_finite(time_limit),
    assertthat::is.number(presolve),
    all_finite(presolve),
    is_match_of(presolve, c(-1, 0, 1, 2)),
    is_thread_count(threads),
    assertthat::is.flag(first_feasible),
    assertthat::noNA(first_feasible),
    assertthat::is.flag(verbose),
    is_installed("rscip")
  )
  # extract start solution
  if (!is.null(start_solution)) {
    start_solution <- planning_unit_solution_status(x, start_solution)
  }
  # adjust node file start
  if (identical(node_file_start, Inf)) {
    node_file_start <- -1 # this is used later to indicate Inf
  }
  # add solver
  x$add_solver(
    R6::R6Class(
      "ScipSolver",
      inherit = Solver,
      public = list(
        name = "scip solver",
        data = list(
          gap = gap,
          time_limit = time_limit,
          presolve = presolve,
          threads = threads,
          first_feasible = first_feasible,
          start_solution = start_solution,
          verbose = verbose
        ),
        calculate = function(x, ...) {
          # create problem
          model <- list(
            modelsense = x$modelsense(),
            vtype = x$vtype(),
            obj = x$obj(),
            A = x$A(),
            rhs = x$rhs(),
            sense = x$sense(),
            lb = x$lb(),
            ub = x$ub()
          )
          # create parameters
          p <- list(
            presolve = self$get_data("presolve"),
            gap = self$get_data("gap"),
            time_limit = self$get_data("time_limit"),
            threads = self$get_data("threads"),
            first_feasible <- self$get_data("first_feasible")
          )
          # add starting solution if specified
          start <- self$get_data("start_solution")
          if (!is.null(start) && !is.Waiver(start)) {
            n_extra <- length(model$obj) - length(start)
            model$start <- c(c(start), rep(NA_real_, n_extra))
          }
          # store internal model and parameters
          self$set_internal("model", model)
          self$set_internal("parameters", p)
          # return success
          invisible(TRUE)
        },
        set_variable_ub = function(index, value) {
          self$internal$model$ub[index] <- value
          invisible(TRUE)
        },
        set_variable_lb = function(index, value) {
          self$internal$model$lb[index] <- value
          invisible(TRUE)
        },
        run = function() {
          # access internal model and parameters
          model <- self$get_internal("model")
          p <- self$get_internal("parameters")
          # solve problem
          rt <- system.time({
            x <- rscip::scip_solve(model = model, params = p)
            )
          })
          # fix potential floating point arithmetic issues
          b <- model$vtype == "B"
          if (is.numeric(x$x)) {
            ## round binary variables because default precision is 1e-5
            x$x[b] <- round(x$x[b])
            ## truncate semi-continuous variables
            v <- model$vtype == "S"
            x$x[v] <- pmax(x$x[v], 0)
            x$x[v] <- pmin(x$x[v], 1)
            ## truncate variables to account for rounding issues
            x$x <- pmax(x$x, model$lb)
            x$x <- pmin(x$x, model$ub)
          }
          # return result
          list(
            x = x$x,
            objective = x$objval,
            status = x$status,
            runtime = rt[[3]]
          )
        }
      )
    )$new()
  )
}
