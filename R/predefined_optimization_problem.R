 #' @include internal.R OptimizationProblem-proto.R
NULL

#' Predefined optimization problem
#'
#' Create a new \code{\link{OptimizationProblem-class}} object.
#'
#' @param x `list` object containing data to construct the problem.
#'
#' @details The argument to `x` must be a list that contains the following
#'   elements:
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
#' @examples
#' # create list with problem data
#' l <- list(modelsense = "min", number_of_features = 2,
#'           number_of_planning_units = 3, number_of_zones = 1,
#'           A_i = c(0L, 1L, 0L, 1L, 0L, 1L), A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
#'           A_x = c(2, 10, 1, 10, 1, 10), obj = c(1, 2, 2), lb = c(0, 1, 0),
#'           ub = c(0, 1, 1), rhs = c(2, 10), compressed_formulation = TRUE,
#'           sense = c(">=", ">="), vtype = c("B", "B", "B"),
#'           row_ids = c("spp_target", "spp_target"),
#'           col_ids = c("pu", "pu", "pu"))
#'
#' # create OptimizationProblem object
#' x <- predefined_optimization_problem(l)
#'
#' # print new object
#' print(x)
#' @name predefined_optimization_problem
#'
#' @export
predefined_optimization_problem <- function(x) {
  assertthat::assert_that(inherits(x, "list"),
  assertthat::is.string(x$modelsense),
  identical(x$modelsense, "min") || identical(x$modelsense, "max"),
  assertthat::is.count(x$number_of_features), is.finite(x$number_of_features),
  assertthat::is.count(x$number_of_planning_units),
  is.finite(x$number_of_planning_units),
  assertthat::is.count(x$number_of_zones),
  is.finite(x$number_of_zones),
  is.numeric(x$obj), all(is.finite(x$obj)),
  length(x$obj) >= x$number_of_planning_units,
  is.numeric(x$lb), all(is.finite(x$lb)), length(x$obj) == length(x$lb),
  is.numeric(x$ub), all(is.finite(x$ub)), length(x$obj) == length(x$ub),
  all(x$lb <= x$ub),
  is.character(x$vtype), all(!is.na(x$vtype)),
  all(x$vtype %in% c("B", "S", "C")),
  is.numeric(x$rhs), all(is.finite(x$rhs)),
  length(x$rhs) >= length(x$number_of_features),
  is.character(x$sense), all(!is.na(x$sense)), length(x$sense) == length(x$rhs),
  all(x$sense %in% c("<=", "=", ">=")),
  is.character(x$row_ids), all(!is.na(x$row_ids)),
  length(x$row_ids) == length(x$rhs),
  is.character(x$col_ids), all(!is.na(x$col_ids)),
  length(x$col_ids) == length(x$obj),
  is.integer(x$A_i), all(is.finite(x$A_i)), min(x$A_i) == 0,
  max(x$A_i) == (length(x$rhs) - 1),
  is.integer(x$A_j), all(is.finite(x$A_j)), min(x$A_j) >= 0,
  max(x$A_j) <= (length(x$obj) - 1), length(x$A_i) == length(x$A_j),
  is.numeric(x$A_x), all(is.finite(x$A_x)), length(x$A_i) == length(x$A_x))
  pproto(NULL, OptimizationProblem,
         ptr = rcpp_predefined_optimization_problem(x))
}
