#' @include internal.R
NULL

#' Write problem
#'
#' Save the mathematical formulation for a conservation planning problem
#' to a file for mixed integer programming solvers. Note that this function
#' requires either the \pkg{Rsymphony} or \pkg{gurobi} package to be installed.
#'
#' @inheritParams compile
#'
#' @param path `character` file path to save the problem formulation.
#' The argument should contain a `".lp"` or `.mps"` file extension
#' to specify whether the problem formulation will be saved in the
#' [LP](https://docs.gurobi.com/current/#refman/lp_format.html) or
#' [MPS](https://docs.gurobi.com/current/#refman/mps_format.html)
#' format (respectively). If using the Gurobi solver
#' (i.e., `solver = "gurobi"`), the problem can
#' be saved in a compressed file format (e.g., `".mps.gx"`) to reduce
#' file size (see `?gurobi::gurobi_write()` for details).
#'
#' @param solver `character` name of optimization solver to write the problem
#' to disk. Available options include: `"rsymphony" `, `"gurobi"`, or `NULL`.
#' Note that using the Gurobi solver is much faster, because the Rsymphony
#' solver requires attempting to solve the problem before it can be written.
#' Defaults to `NULL`, such that the best available solver is used.
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
#' ## note that either the gurobi or Rsymphony package needs to be installed
#' write_problem(p, path)
#'
#' # print model file
#' cat(readLines(path), sep = "\n")
#' }
#'
#' @export
write_problem <- function(x, path, solver = NULL) {
  # assert arguments are valid
  assert_required(x)
  assert_required(path)
  assert_required(solver)
  assert(
    is_conservation_problem(x),
    assertthat::is.string(path),
    assertthat::noNA(path),
    assertthat:: is.writeable(dirname(path))
  )
  if (is.null(solver)) {
    solver <- "rsymphony"
    if (requireNamespace("gurobi", quietly = TRUE)) {
      solver <- "gurobi" # nocov
    } else {
      assert(
        requireNamespace("Rsymphony", quietly = TRUE),
        msg = "The {.pkg Rsymphony} or {.pkg gurobi} package must be installed."
      )
    }
  }
  assert(
    assertthat::is.string(solver),
    is_match_of(solver, c("rsymphony", "gurobi"))
  )
  if (identical(solver, "rsymphony")) {
    assert(is_installed("Rsymphony"))
  }
  if (identical(solver, "gurobi")) {
    assert(is_installed("gurobi")) # nocov
  }
  # assert valid file extensions
  valid_ext <- c(".lp", ".mps")
  valid_ext_msg <- paste(
    "{.arg path} must have a",
    "{.val \".lp\"} or {.val \".mps\"} file extension for the Rsymphony",
    "solver."
  )
  if (identical(solver, "gurobi")) {
    valid_ext <- c(
      ".mps", ".rew", ".lp", ".rlp", ".dua", ".dlp", ".ilp",
      ".mps.gz", ".rew.gz", ".lp.gz", ".rlp.gz", ".dua.gz", ".dlp.gz", "ilp.gz",
      ".mps.bz2", ".rew.bz2", ".lp.bz2", ".rlp.bz2", ".dua.bz2", ".dlp.bz2",
      "ilp.bz2",
      ".mps.zip", ".rew.zip", ".lp.zip", ".rlp.zip", ".dua.zip", ".dlp.zip",
      "ilp.zip",
      ".mps.7z", ".rew.7z", ".lp.7z", ".rlp.7z", ".dua.7z", ".dlp.7z", "ilp.7z"
    )
    valid_ext_msg <- c(
      "!" = paste(
        "{.arg path} must have a valid file extension",
        "for the Gurobi solver."
      ),
      "i" = "See {.fn gurobi::gurobi_write} for valid file extensions."
    )
  }
  assert(
    isTRUE(any(vapply(valid_ext, endsWith, logical(1), x = path))),
    msg = valid_ext_msg
  )
  # add solver
  if (identical(solver, "rsymphony")) {
    x <- suppressWarnings(add_rsymphony_solver(x))
  } else {
    x <- suppressWarnings(add_gurobi_solver(x)) # nocov
  }

  # compile problem
  x$solver$calculate(compile(x))
  model <- x$solver$internal$model
  assert(
    is.list(model),
    msg = "failed to compile optimization problem"
  )

  # create model file
  if (identical(solver, "rsymphony")) {
    ## if using Rsymphony solver ...
    ## create parameters
    p <- list(
      first_feasible = TRUE,
      verbosity = -2,
      node_limit = 1,
      write_lp = endsWith(path, ".lp"),
      write_mps = endsWith(path, ".mps")
    )
    ## create temporary directory to save result
    d <- file.path(tempdir(), basename(tempfile()))
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    ## save problem to disk by generating a solution to the problem
    y <- withr::with_dir(
      d,
      do.call(Rsymphony::Rsymphony_solve_LP, append(model, p))
    )
    ## move file to desired location
    assert(
      length(dir(d)) == 1,
      msg = "failed to save problem formulation"
    )
    raw_path <- dir(d, full.names = TRUE)[1]
    file.copy(raw_path, path)
  } else {
    ## if using gurobi solver ...
    gurobi::gurobi_write(model = model, filename = path) # nocov
  }

  # return success
  invisible(TRUE)
}
