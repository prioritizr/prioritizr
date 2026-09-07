#' @include internal.R
NULL

#' Compile a multi-objective optimization problem
#'
#' Compile multiple [`OptimizationProblem-class`] objects for
#' multi-objective optimization.
#'
#' @param x [multi_problem()] or `list` of [`OptimizationProblem-class`]
#' objects.
#'
#' @param ... arguments passed to [compile()].
#'
#' @return
#' A `list` containing a (`$obj`) `numeric` matrix with the coefficients
#' for each of the objectives (i.e., rows correspond to different
#' objectives and columns correspond to different decision variables),
#' (`$modelsense`) `character` vector indicating if each
#' objective should be maximized or minimized
#' (i.e., each element corresponds to a different objective),
#' and a (`$opt`) [`OptimizationProblem-class`] object with all
#' of the constraints present in `x` (note that the objective coefficients
#' in the returned object are all zero).
#'
#' @seealso
#' See [compile()] to create an [`OptimizationProblem-class`] object.
#'
#' @examplesIf prioritizr::do_run_example()
#' # import data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # define a total conservation budget (30% of total cost)
#' budget <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.3
#'
#' # create multi-objective conservation planning problem
#' mp <-
#'   multi_problem(
#'     keystone_obj =
#'       problem(sim_pu_raster, sim_features[[1:3]]) %>%
#'       add_min_shortfall_objective(budget) %>%
#'       add_relative_targets(0.4) %>%
#'       add_binary_decisions(),
#'     iconic_obj =
#'       problem(sim_pu_raster, sim_features[[4:5]]) %>%
#'       add_min_shortfall_objective(budget) %>%
#'       add_relative_targets(0.45) %>%
#'       add_binary_decisions()
#'   )
#'
#' # compile into multi-objective optimization problem
#' mo <- multi_compile(mp)
#'
#' # print multi-objective optimization problem
#' print(mo)
#'
#' @export
multi_compile <- function(x, ...) {
  assert_required(x)
  UseMethod("multi_compile")
}

#' @rdname multi_compile
#' @export
multi_compile.MultiConservationProblem <- function(x, ...) {
  # compile each problem individually, and compile into multi-objective problem
  multi_compile.list(
    stats::setNames(
      lapply(
        x$problems,
        function(x) {
          internal_compile(x, ..., call = rlang::expr(multi_compile()))
        }
      ),
      names(x$problems)
    )
  )
}

#' @rdname multi_compile
#' @export
multi_compile.list <- function(x, ...) {
  # assert arguments are valid
  assert(
    is.list(x),
    all_elements_inherit(x, "OptimizationProblem")
  )

  # compile multi-objective optimization problem
  out <- rcpp_compile_multi_obj_problem(lapply(x, function(z) z$ptr))

  # if needed, set names for objectives
  if (!is.null(names(x))) {
    rownames(out$obj) <- names(x)
  }

  # convert pointer to optimization problem object
  out$opt <- OptimizationProblem$new(ptr = out$opt)

  # return result
  out
}
