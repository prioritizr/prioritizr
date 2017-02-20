 #' @include internal.R OptimizationProblem-proto.R
NULL

#' Optimization problem
#'
#' Generate a new \code{\link{OptimizationProblem}} object.
#'
#' @return \code{\link{OptimizationProblem}} object.
#'
#' @seealso OptimizationProblem-methods
#'
#' @export
new_optimization_problem <- function() {
  pproto(NULL, OptimizationProblem,
    ptr=rcpp_new_optimization_problem())
}

#' Access data from optimization problem
#' 
#' These functions are used to access data from an
#' \code{\link{OptimizationProblem}} object. See below for details on their
#' usage.
#'
#' @param x \code{\link{OptimizationProblem}} object.
#'
#' @details The functions return the following data:
#'
#' \describe{
#'
#' \item{nrow}{\code{integer} number of rows (constraints).}
#'
#' \item{ncol}{\code{integer} number of columns (decision variables).}
#'
#' \item{ncell}{\code{integer} number of cells.}
#'
#' \item{modelsense}{\code{character} describing if the problem is to be
#'   maximized ('max') or minimized ('min').}
#'
#' \item{vtype}{\code{character} describing the type of each decision variable:
#'   binary ('B'), semi-continuous ('S'), or continuous ('C')}
#'
#' \item{obj}{\code{numeric} vector specifying the objective function.}
#'
#' \item{A}{\code{\link[Matrix]{dgCMatrix-class}} matrix object defining the
#'   problem matrix.}
#'
#' \item{rhs}{\code{numeric} vector with right-hand-side linear constraints}
#'
#' \item{sense}{\code{character} vector with the senses of the linear
#'   constraints ('<=', '>=', '=').}
#'
#' \item{lb}{\code{numeric} lower bounds for decision variables. \code{NA} 
#'   values indicate no lower bound.}
#'
#' \item{ub}{\code{numeric} upper bounds for decision variables. NA values 
#'   indicate no upper bound.}
#'
#' \item{number_of_planning_units}{\code{integer} number of planning units in
#'   the problem.}
#'
#' \item{number_of_features}{\code{integer} number of features
#'   the problem.}
#'
#' }
#'
#' @return \code{\link[Matrix]{dgCMatrix-class}}, \code{numeric} vector,
#'         \code{numeric} vector, or scalar \code{integer}.
#'
#' @name OptimizationProblemMethods
NULL

#' @rdname OptimizationProblemMethods
#'
#' @usage nrow(x)
#'
#' @importFrom raster nrow
#'
#' @exportMethod nrow
methods::setMethod('nrow', 'OptimizationProblem', function(x) x$nrow())

#' @rdname OptimizationProblem-methods
#'
#' @usage ncol(x)
#'
#' @importFrom raster ncol
#'
#' @exportMethod ncol
#'
#' @export
methods::setMethod('ncol', 'OptimizationProblem', function(x) x$ncol())

#' @rdname OptimizationProblem-methods
#'
#' @usage ncell(x)
#'
#' @importFrom raster ncol
#'
#' @exportMethod ncol
#'
#' @export
methods::setMethod('ncell', 'OptimizationProblem', function(x) x$ncell())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod modelsense
#'
#' @export
methods::setGeneric('modelsense', function(x) standardGeneric('modelsense'))


#' @rdname OptimizationProblem-methods
#'
#' @usage modelsense(x)
#'
#' @export
methods::setMethod('modelsense', 'OptimizationProblem',
  function(x) x$modelsense())


#' @rdname OptimizationProblem-methods
#'
#' @exportMethod vtype
#'  
#' @export
methods::setGeneric('vtype', function(x) standardGeneric('vtype'))

#' @rdname OptimizationProblem-methods
#'
#' @usage vtype(x)
#'
#' @export
methods::setMethod('vtype', 'OptimizationProblem', function(x) x$vtype())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod obj
#'
#' @export
methods::setGeneric('obj', function(x) standardGeneric('obj'))

#' @rdname OptimizationProblem-methods
#'
#' @usage obj(x)
#'
#' @export
methods::setMethod('obj', 'OptimizationProblem', function(x) x$obj())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod A
#'
#' @export
methods::setGeneric('A', function(x) standardGeneric('A'))

#' @rdname OptimizationProblem-methods
#'
#' @usage A(x)
#'
#' @export
methods::setMethod('A', 'OptimizationProblem', function(x) x$A())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod rhs
#'
#' @export
methods::setGeneric('rhs', function(x) standardGeneric('rhs'))

#' @rdname OptimizationProblem-methods
#'
#' @usage rhs(x)
#'
#' @export
methods::setMethod('rhs', 'OptimizationProblem', function(x) x$rhs())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod sense
#'
#' @export
methods::setGeneric('sense', function(x) standardGeneric('sense'))

#' @rdname OptimizationProblem-methods
#'
#' @usage sense(x)
#'
#' @export
methods::setMethod('sense', 'OptimizationProblem', function(x) x$sense())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod lb
#'
#' @export
methods::setGeneric('lb', function(x) standardGeneric('lb'))

#' @rdname OptimizationProblem-methods
#'
#' @usage lb(x)
#'
#' @export
methods::setMethod('lb', 'OptimizationProblem', function(x) x$lb())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod ub
#'
#' @export
methods::setGeneric('ub', function(x) standardGeneric('ub'))

#' @rdname OptimizationProblem-methods
#'
#' @usage ub(x)
#'
#' @export
methods::setMethod('ub', 'OptimizationProblem', function(x) x$ub())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod number_of_features
#'
#' @export
methods::setGeneric('number_of_features',
  function(x) standardGeneric('number_of_features'))

#' @rdname OptimizationProblem-methods
#'
#' @usage number_of_features(x)
#'
#' @export
methods::setMethod('number_of_features', 'OptimizationProblem',
  function(x) x$number_of_features())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod number_of_planning_units
#'
#' @export
methods::setGeneric('number_of_planning_units',
  function(x) standardGeneric('number_of_planning_units'))

#' @rdname OptimizationProblem-methods
#'
#' @usage number_of_planning_units(x)
#'
#' @export
methods::setMethod('number_of_planning_units', 'OptimizationProblem',
  function(x) x$number_of_planning_units())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod col_ids
#'
#' @export
methods::setGeneric('col_ids', function(x) standardGeneric('col_ids'))

#' @rdname OptimizationProblem-methods
#'
#' @usage col_ids(x)
#'
#' @export
methods::setMethod('col_ids', 'OptimizationProblem',
  function(x) x$col_ids())

#' @rdname OptimizationProblem-methods
#'
#' @exportMethod row_ids
#'
#' @export
methods::setGeneric('row_ids', function(x) standardGeneric('row_ids'))

#' @rdname OptimizationProblem-methods
#'
#' @usage row_ids(x)
#'
#' @export
methods::setMethod('row_ids', 'OptimizationProblem', function(x) x$row_ids())

