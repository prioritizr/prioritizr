#' @include Solver-class.R
NULL

#' Add a compile solver
#'
#' Specify that no software should be used to solve a conservation
#' planning problem.
#'
#' @param x [problem()] object.
#'
#' @param ... not used.
#'
#' @details
#' This function is used for testing the \pkg{prioritizr} package on
#' systems where no solvers are installed.
#' Although it is not documented as an option for [add_default_solver()],
#' it can be enabled using
#' `Sys.setenv("PRIORITIZR_ENABLE_COMPILE_SOLVER", "TRUE")`.
#'
#' @inheritSection add_gurobi_solver Start solution format
#'
#' @inherit add_gurobi_solver return seealso
#'
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
#'   add_compile_solver()
#'
#' # print problem
#' print(p1)
#' }
#' @noRd
add_compile_solver <- function(x, ...) {
  # assert that arguments are valid
  assert_required(x)
  assert(is_generic_conservation_problem(x))
  # add solver
  x$add_solver(
    R6::R6Class(
      "CompileSolver",
      inherit = Solver,
      public = list(
        name = "compile solver"
      )
    )$new()
  )
}
