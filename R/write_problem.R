#' @include internal.R
NULL

#' Write problem
#'
#' Save the mathematical formulation for a conservation planning problem
#' to a file for mixed integer programming solvers. Note that this function
#' requires the \pkg{Rsymphony} package to be installed.
#'
#' @inheritParams compile
#'
#' @param path `character` file path to save the problem formulation.
#' The argument should contain a `".lp"` or `.mps"` file extension
#' to specify whether the problem formulation will be saved in the
#' [LP](https://www.gurobi.com/documentation/9.5/refman/lp_format.html) or
#' [MPS](https://www.gurobi.com/documentation/9.5/refman/mps_format.html)
#' format (respectively).
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#'
#' # subset data to extract first four planning units
#' sim_pu_polygons <- sim_pu_polygons[1:4, ]
#'
#' # create minimal problem
#' p <-
#'   problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions()
#'
#' # specify file path to save problem formulation
#' path <- file.path(tempdir(), "model.lp")
#' print(path)
#'
#' # save problem to file
#' ## note that the Rsymphony package needs to be installed
#' write_problem(p, path)
#'
#' # print model file
#' cat(readLines(path), sep = "\n")
#' }
#'
#' @export
write_problem <- function(x, path) {
  # assert arguments are valid
  assert_required(x)
  assert_required(path)
  assert(
    is_conservation_problem(x),
    assertthat::is.string(path),
    assertthat::noNA(path),
    is_installed("Rsymphony"),
    assertthat:: is.writeable(dirname(path))
  )
  assert(
    assertthat::has_extension(path, "lp") ||
    assertthat::has_extension(path, "mps"),
    msg = paste(
      "{.arg path} must have a",
      "{.val \".lp\"} or {.val \".mps\"} file extension."
    )
  )

  # add rsymphony solver
  x <- suppressWarnings(add_rsymphony_solver(x))

  # compile problem
  x$solver$calculate(compile(x))
  model <- x$solver$internal$model
  assert(
    is.list(model),
    msg = "failed to compile optimization problem"
  )

  # create parameters
  p <- list(
    first_feasible = TRUE,
    verbosity = -2,
    node_limit = 1,
    write_lp = endsWith(path, ".lp"),
    write_mps = endsWith(path, ".mps")
  )

  # create temporary directory to save result
  d <- file.path(tempdir(), basename(tempfile()))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

  # save problem to disk by generating a solution to the problem
  y <- withr::with_dir(
    d,
    do.call(Rsymphony::Rsymphony_solve_LP, append(model, p))
  )

  # move file to desired location
  assert(
    length(dir(d)) == 1,
    msg = "failed to save problem formulation"
  )
  file.rename(dir(d, full.names = TRUE)[1], path)

  # return success
  invisible(TRUE)
}
