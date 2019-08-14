#' @include internal.R pproto.R Parameters-proto.R
NULL

#' @export
if (!methods::isClass("OptimizationProblem")) methods::setOldClass("OptimizationProblem")
NULL

#' Optimization problem class
#'
#' The \code{OptimizationProblem} class is used to represent an optimization
#' problem. Data are stored in memory and accessed using an external pointer.
#' \strong{Only experts should interact with this class directly.}
#'
#' @section Fields:
#' \describe{
#' \item{$ptr}{\code{externalptr} object.}
#' }
#'
#' @section Usage:
#' \code{x$print()}
#'
#' \code{x$show()}
#'
#' \code{x$repr()}
#'
#' \code{x$ncol()}
#'
#' \code{x$nrow()}
#'
#' \code{x$ncell()}
#'
#' \code{x$modelsense()}
#'
#' \code{x$vtype()}
#'
#' \code{x$obj()}
#'
#' \code{x$A()}
#'
#' \code{x$rhs()}
#'
#' \code{x$sense()}
#'
#' \code{x$lb()}
#'
#' \code{x$ub()}
#'
#' \code{x$set_ub()}
#'
#' \code{x$number_of_planning_units()}
#'
#' \code{x$number_of_features()}
#'
#' \code{x$number_of_zones()}
#'
#' \code{x$row_ids()}
#'
#' \code{x$col_ids()}
#'
#' \code{x$compressed_formulation()}
#'
#' @section Arguments:
#' \describe{
#' \item{ptr}{\code{externalptr} object.}
#' }
#'
#' @section Details:
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{\code{character} representation of object.}
#'
#' \item{ncol}{\code{integer} number of columns (variables) in model matrix.}
#'
#' \item{nrow}{\code{integer} number of rows (constraints) in model matrix.}
#'
#' \item{ncell}{\code{integer} number of cells in model matrix.}
#'
#' \item{modelsense}{\code{character} model sense.}
#'
#' \item{vtype}{\code{character} vector of variable types.}
#'
#' \item{obj}{\code{numeric} vector of objective function.}
#'
#' \item{A}{\code{\link[Matrix]{dgCMatrix-class}} model matrix }
#'
#' \item{rhs}{\code{numeric} vector of right-hand-side constraints.}
#'
#' \item{sense}{\code{character} vector of constraint senses.}
#'
#' \item{lb}{\code{numeric} vector of lower bounds for each decision variable.}
#'
#' \item{ub}{\code{numeric} vector of upper bounds for each decision variable.}
#'
#' \item{set_ub}{\code{invisible(NULL)} set upper bound for a decision
#'   variable.}
#'
#' \item{number_of_features}{\code{integer} number of features in the problem.}
#'
#' \item{number_of_planning_units}{\code{integer} number of planning units in
#'   the problem.}
#'
#' \item{number_of_zones}{\code{integer} number of zones in the
#'   problem.}
#'
#' \item{col_ids}{\code{character} names describing each decision variable
#'   (column) in the model matrix.}
#'
#' \item{row_ids}{\code{character} names describing each constraint (row) in
#'   in the model matrix.}
#'
#' \item{compressed_formulation}{is the optimization problem formulated
#'   using a compressed version of the rij matrix?}
#'
#' \item{shuffle_columns}{randomly shuffle the columns in the problem. This
#'   should almost never be called manually and only should only be called
#'  after the optimization problem has been fully constructed.}
#'
#' }
#'
#' @name OptimizationProblem-class
#'
#' @aliases OptimizationProblem
NULL

#' @export
OptimizationProblem <- pproto(
  "OptimizationProblem",
  ptr = NULL,
  print = function(self) {
    if (self$ncol() > 0) {
    cv <- table(self$vtype())
    cv <- paste(paste(unname(cv), paste0("(", names(cv), ")")),
          collapse = ", ")
    message("optimization problem",
      "\n  model sense: ", self$modelsense(),
      "\n  dimensions:  ", self$nrow(), ", ", self$ncol(), ", ", self$ncell(),
                          " (nrow, ncol, ncell)",
      "\n  variables:   ", cv)
    } else {
      message("optimization problem (empty)")
    }
  },
  show = function(self) {
    self$print()
  },
  ncol = function(self) {
    rcpp_get_optimization_problem_ncol(self$ptr)
  },
  nrow = function(self) {
    rcpp_get_optimization_problem_nrow(self$ptr)
  },
  ncell = function(self) {
    rcpp_get_optimization_problem_ncell(self$ptr)
  },
  modelsense = function(self) {
    rcpp_get_optimization_problem_modelsense(self$ptr)
  },
  vtype = function(self) {
    rcpp_get_optimization_problem_vtype(self$ptr)
  },
  obj = function(self) {
    rcpp_get_optimization_problem_obj(self$ptr)
  },
  A = function(self) {
    x <- rcpp_get_optimization_problem_A(self$ptr)
    Matrix::sparseMatrix(i = x$i, j = x$j, x = x$x, index1 = FALSE,
                         giveCsparse = TRUE,
                         dims = c(nrow(self), ncol(self)))
  },
  rhs = function(self) {
    rcpp_get_optimization_problem_rhs(self$ptr)
  },
  sense = function(self) {
    rcpp_get_optimization_problem_sense(self$ptr)
  },
  lb = function(self) {
    rcpp_get_optimization_problem_lb(self$ptr)
  },
  ub = function(self) {
    rcpp_get_optimization_problem_ub(self$ptr)
  },
  set_ub = function(self, i, y) {
    assertthat::assert_that(assertthat::is.count(i), assertthat::is.number(y),
                            i <= ncol(self))
    rcpp_set_optimization_problem_ub(self$ptr, i, y)
  },
  number_of_features = function(self) {
    rcpp_get_optimization_problem_number_of_features(self$ptr)
  },
  number_of_planning_units = function(self) {
    rcpp_get_optimization_problem_number_of_planning_units(self$ptr)
  },
  number_of_zones = function(self) {
    rcpp_get_optimization_problem_number_of_zones(self$ptr)
  },
  col_ids = function(self) {
    rcpp_get_optimization_problem_col_ids(self$ptr)
  },
  row_ids = function(self) {
    rcpp_get_optimization_problem_row_ids(self$ptr)
  },
  compressed_formulation = function(self) {
    rcpp_get_optimization_problem_compressed_formulation(self$ptr)
  },
  shuffle_columns = function(self) {
    rcpp_set_optimization_problem_shuffled(self$ptr)
})
