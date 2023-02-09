#' @include internal.R OptimizationProblem-class.R
NULL

#' Optimization problem
#'
#' Create a new optimization problem.
#'
#' @param x A `NULL` or `list` object. See Details for more information.
#'  Defaults to `NULL`.
#'
#' @details
#' The argument to `x` can be a `NULL` or a `list`. If `x` is a `NULL`,
#' then an empty optimization problem is created. Alternately, if a `x` is
#' a `list` then a fully formulated optimization problem is created.
#' Specifically, the `list` should contain the following elements.
#'
#'   \describe{
#'
#'   \item{modelsense}{`character` model sense.}
#'
#'   \item{number_of_features}{`integer` number of features in problem.}
#'
#'   \item{number_of_planning_units}{`integer` number of planning units.}
#'
#'   \item{A_i}{`integer` row indices for problem matrix.}
#'
#'   \item{A_j}{`integer` column indices for problem matrix.}
#'
#'   \item{A_x}{`numeric` values for problem matrix.}
#'
#'   \item{obj}{`numeric` objective function values.}
#'
#'   \item{lb}{`numeric` lower bound for decision values.}
#'
#'   \item{ub}{`numeric` upper bound for decision values.}
#'
#'   \item{rhs}{`numeric` right-hand side values.}
#'
#'   \item{sense}{`numeric` constraint senses.}
#'
#'   \item{vtype}{`character` variable types. These are used to specify
#'     that the decision variables are binary (`"B"`) or continuous
#'     (`"C"`).}
#'
#'   \item{row_ids}{`character` identifiers for the rows in the problem
#'     matrix.}
#'
#'   \item{col_ids}{`character` identifiers for the columns in the problem
#'     matrix.}
#'
#'   }
#'
#' @return An [OptimizationProblem-class] object.
#'
#' @seealso [OptimizationProblem-methods].
#'
#' @examples
#' # create new empty object
#' x1 <- optimization_problem()
#'
#' # print new empty object
#' print(x1)
#'
#' # create list with optimization problem
#' l <- list(
#'   modelsense = "min",
#'   number_of_features = 2,
#'   number_of_planning_units = 3,
#'   number_of_zones = 1,
#'   A_i = c(0L, 1L, 0L, 1L, 0L, 1L),
#'   A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
#'   A_x = c(2, 10, 1, 10, 1, 10),
#'   obj = c(1, 2, 2),
#'   lb = c(0, 1, 0),
#'   ub = c(0, 1, 1),
#'   rhs = c(2, 10),
#'   compressed_formulation = TRUE,
#'   sense = c(">=", ">="),
#'   vtype = c("B", "B", "B"),
#'   row_ids = c("spp_target", "spp_target"),
#'   col_ids = c("pu", "pu", "pu")
#' )
#'
#' # create fully formulated object based on lists
#' x2 <- optimization_problem(l)
#'
#' # print fully formulated object
#' print(x2)
#' @export
optimization_problem <- function(x = NULL) {
  assert(is_inherits(x, c("NULL", "list")))
  if (is.null(x)) {
    ptr <- rcpp_new_optimization_problem()
  } else {
    assert(
      is.list(x),
      assertthat::is.string(x$modelsense),
      is_match_of(x$modelsense, c("min", "max")),
      assertthat::is.count(x$number_of_features),
      all_finite(x$number_of_features),
      assertthat::is.count(x$number_of_planning_units),
      all_finite(x$number_of_planning_units),
      assertthat::is.count(x$number_of_zones),
      all_finite(x$number_of_zones),
      is.numeric(x$obj),
      all_finite(x$obj),
      length(x$obj) >= x$number_of_planning_units,
      is.numeric(x$lb),
      all_finite(x$lb),
      length(x$obj) == length(x$lb),
      is.numeric(x$ub),
      all_finite(x$ub),
      length(x$obj) == length(x$ub),
      all(x$lb <= x$ub),
      is.character(x$vtype),
      assertthat::noNA(is.na(x$vtype)),
      all_match_of(x$vtype, c("B", "S", "C")),
      is.numeric(x$rhs),
      all_finite(x$rhs),
      length(x$rhs) >= length(x$number_of_features),
      is.character(x$sense),
      assertthat::noNA(x$sense),
      length(x$sense) == length(x$rhs),
      all_match_of(x$sense, c("<=", "=", ">=")),
      is.character(x$row_ids),
      assertthat::noNA(x$row_ids),
      length(x$row_ids) == length(x$rhs),
      is.character(x$col_ids),
      assertthat::noNA(x$col_ids),
      length(x$col_ids) == length(x$obj),
      is.integer(x$A_i),
      all_finite(x$A_i),
      min(x$A_i) == 0,
      max(x$A_i) == (length(x$rhs) - 1),
      is.integer(x$A_j),
      all_finite(x$A_j),
      min(x$A_j) >= 0,
      max(x$A_j) <= (length(x$obj) - 1),
      length(x$A_i) == length(x$A_j),
      is.numeric(x$A_x),
      all_finite(x$A_x),
      length(x$A_i) == length(x$A_x)
    )
    ptr <- rcpp_predefined_optimization_problem(x)
  }
  # return object
  OptimizationProblem$new(ptr = ptr)
}
