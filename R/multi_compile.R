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
#' @return
#' A `list` containing a (`$obj`) `numeric` matrix with the coefficients
#' for each of the objectives (i.e., rows correspond to different
#' objectives and columns correspond to different decision variables),
#' (`$modelsense`) `character` vector indicating if each
#' objective should be maximized or minimized
#' (i.e., each element corresponds to a different objective),
#' and a (`$opt`) [`OptimizationProblem-class`] object with all
#' of the constraints present in `x` (note that the objective coefficients
#' for this object are all zero).
#'
#' @seealso
#' See [compile()] to create an [`OptimizationProblem-class`] object.
#'
#' @examples
#' # TODO
#' @export
multi_compile <- function(x, ...) {
  assert_required(x)
  UseMethod("multi_compile")
}

#' @rdname compile
#' @export
multi_compile.MultiObjConservationProblem <- function(x, ...) {
  # compile each problem individually, and compile into multi-objective problem
  multi_compile.list(
    stats::setNames(
      lapply(x$problems, compile.ConservationProblem),
      names(x$problems)
    )
  )
}

#' @rdname compile
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
