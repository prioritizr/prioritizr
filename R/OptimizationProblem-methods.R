#' @include internal.R OptimizationProblem-proto.R
NULL

#' Optimization problem methods
#'
#' These functions are used to access data from an
#' [`OptimizationProblem-class`] object.
#'
#' @param x [`OptimizationProblem-class`] object.
#'
#' @details The functions return the following data:
#'
#' \describe{
#'
#' \item{nrow}{`integer` number of rows (constraints).}
#'
#' \item{ncol}{`integer` number of columns (decision variables).}
#'
#' \item{ncell}{`integer` number of cells.}
#'
#' \item{modelsense}{`character` describing if the problem is to be
#'   maximized (`"max"`) or minimized (`"min"`).}
#'
#' \item{vtype}{`character` describing the type of each decision variable:
#'   binary (`"B"`), semi-continuous (`"S"`), or continuous
#'   (`"C"`)}
#'
#' \item{obj}{`numeric` vector specifying the objective function.}
#'
#' \item{A}{[`dgCMatrix-class`] matrix object defining the
#'   problem matrix.}
#'
#' \item{rhs}{`numeric` vector with right-hand-side linear constraints}
#'
#' \item{sense}{`character` vector with the senses of the linear
#'   constraints (`"<="`, `">="`, `"="`).}
#'
#' \item{lb}{`numeric` lower bound for each decision variable. Missing data
#'   values (`NA`) indicate no lower bound for a given variable.}
#'
#' \item{ub}{`numeric` upper bounds for each decision variable. Missing
#'   data values (`NA`) indicate no upper bound for a given variable.}
#'
#' \item{number_of_planning_units}{`integer` number of planning units in
#'   the problem.}
#'
#' \item{number_of_features}{`integer` number of features
#'   the problem.}
#'
#' }
#'
#' @return A [`dgCMatrix-class`], `numeric` vector,
#'   `numeric` vector, or scalar `integer` depending on the method
#'   used.
#'
#' @name OptimizationProblem-methods
#'
#' @aliases nrow ncol ncell modelsense vtype obj A rhs sense lb ub col_ids row_ids compressed_formulation ncell,OptimizationProblem-method A,OptimizationProblem-method col_ids,OptimizationProblem-method lb,OptimizationProblem-method modelsense,OptimizationProblem-method ncol,OptimizationProblem-method nrow,OptimizationProblem-method  obj,OptimizationProblem-method rhs,OptimizationProblem-method row_ids,OptimizationProblem-method sense,OptimizationProblem-method ub,OptimizationProblem-method vtype,OptimizationProblem-method  compressed_formulation,OptimizationProblem-method
NULL

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @importFrom raster nrow
#'
#' @exportMethod nrow
#'
#' @usage nrow(x)
NULL

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{nrow}{OptimizationProblem}(x)
methods::setMethod("nrow", "OptimizationProblem", function(x) x$nrow())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @importFrom raster ncol
#'
#' @exportMethod ncol
#'
#' @usage ncol(x)
NULL

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{ncol}{OptimizationProblem}(x)
methods::setMethod("ncol", "OptimizationProblem", function(x) x$ncol())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @importFrom raster ncell
#'
#' @exportMethod ncell
#'
#' @usage ncell(x)
NULL

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{ncell}{OptimizationProblem}(x)
methods::setMethod("ncell", "OptimizationProblem", function(x) x$ncell())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod modelsense
#'
#' @usage modelsense(x)
methods::setGeneric("modelsense", function(x) standardGeneric("modelsense"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{modelsense}{OptimizationProblem}(x)
methods::setMethod("modelsense", "OptimizationProblem",
  function(x) x$modelsense())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod vtype
#'
#' @usage vtype(x)
methods::setGeneric("vtype", function(x) standardGeneric("vtype"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{vtype}{OptimizationProblem}(x)
methods::setMethod("vtype", "OptimizationProblem", function(x) x$vtype())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod obj
#'
#' @usage obj(x)
methods::setGeneric("obj", function(x) standardGeneric("obj"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{obj}{OptimizationProblem}(x)
methods::setMethod("obj", "OptimizationProblem", function(x) x$obj())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod A
#'
#' @usage A(x)
methods::setGeneric("A", function(x) standardGeneric("A"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{A}{OptimizationProblem}(x)
methods::setMethod("A", "OptimizationProblem", function(x) x$A())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod rhs
#'
#' @usage rhs(x)
methods::setGeneric("rhs", function(x) standardGeneric("rhs"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{rhs}{OptimizationProblem}(x)
methods::setMethod("rhs", "OptimizationProblem", function(x) x$rhs())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod sense
#'
#' @usage sense(x)
methods::setGeneric("sense", function(x) standardGeneric("sense"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{sense}{OptimizationProblem}(x)
methods::setMethod("sense", "OptimizationProblem", function(x) x$sense())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod lb
#'
#' @usage lb(x)
methods::setGeneric("lb", function(x) standardGeneric("lb"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{lb}{OptimizationProblem}(x)
methods::setMethod("lb", "OptimizationProblem", function(x) x$lb())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod ub
#'
#' @usage ub(x)
#'
methods::setGeneric("ub", function(x) standardGeneric("ub"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{ub}{OptimizationProblem}(x)
methods::setMethod("ub", "OptimizationProblem", function(x) x$ub())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod col_ids
#'
#' @usage col_ids(x)
methods::setGeneric("col_ids", function(x) standardGeneric("col_ids"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{col_ids}{OptimizationProblem}(x)
methods::setMethod("col_ids", "OptimizationProblem",
  function(x) x$col_ids())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod row_ids
#'
#' @usage row_ids(x)
methods::setGeneric("row_ids", function(x) standardGeneric("row_ids"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{row_ids}{OptimizationProblem}(x)
methods::setMethod("row_ids", "OptimizationProblem", function(x) x$row_ids())

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @exportMethod compressed_formulation
#'
#' @usage compressed_formulation(x)
methods::setGeneric("compressed_formulation",
                    function(x) standardGeneric("compressed_formulation"))

#' @name OptimizationProblem-methods
#'
#' @rdname OptimizationProblem-methods
#'
#' @usage \S4method{compressed_formulation}{OptimizationProblem}(x)
methods::setMethod("compressed_formulation", "OptimizationProblem",
                   function(x) x$compressed_formulation())

#' Convert `OptimizationProblem` to list
#'
#' @param x [`OptimizationProblem-class`] object.
#'
#' @param ... not used.
#'
#' @return `list()` object.
#'
#' @method as.list OptimizationProblem
#'
#' @rdname as.list
#'
#' @export
as.list.OptimizationProblem <- function(x, ...) {
  rcpp_optimization_problem_as_list(x$ptr)
}
