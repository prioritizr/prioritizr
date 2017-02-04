 #' @include internal.R OptimizationProblem-class.R
NULL

#' Access data from optimization problem
#' 
#' These functions are used to access data from an
#' \code{\link{OptimizationProblem}} object. See below for details on their
#' usage.
#'
#' @param x \code{\link{OptimizationProblem}} object.
#'
#' @details The functions return the following data:
#' \describe{
#' \item{nrow}{\code{integer} number of rows (constraints).}
#' \item{ncol}{\code{integer} number of columns (decision variables).}
#' \item{ncell}{\code{integer} number of cells.}
#' \item{modelsense}{\code{character} describing if the problem is to be
#'   maximized ('max') or minimized ('min').}
#' \item{vtype}{\code{character} describing the type of each decision variable:
#'   binary ('B'), semi-continuous ('S'), or continuous ('C')}
#' \item{obj}{\code{numeric} vector specifying the objective function.}
#' \item{A}{\code{\link[Matrix]{dgCMatrix-class}} matrix object defining the problem
#'   matrix.}
#' \item{rhs}{\code{numeric} vector with right-hand-side linear constraints}
#' \item{sense}{\code{character} vector with the senses of the linear constraints
#'   ('<=', '>=', '=').}
#' \item{lb}{\code{numeric} lower bounds for decision variables. NA values indicate
#'   no lower bound.}
#' \item{ub}{\code{numeric} upper bounds for decision variables. NA values indicate
#'   no upper bound.}
#' }
#'
#' @return \code{\link[Matrix]{dgCMatrix-class}}, \code{numeric} vector,
#'         \code{numeric} vector, or scalar \code{integer}.
#'
#' @name OptimizationProblem-methods
NULL

#' @rdname OptimizationProblem-methods
#' @name nrow
#' @usage nrow(x)
#' @export
methods::setMethod('nrow', 'OptimizationProblem', function(x) x$nrow())

#' @rdname OptimizationProblem-methods
#' @name ncol
#' @usage ncol(x)
#' @export
methods::setMethod('ncol', 'OptimizationProblem', function(x) x$ncol())

#' @rdname OptimizationProblem-methods
#' @name ncell
#' @usage ncell(x)
#' @export
methods::setMethod('ncell', 'OptimizationProblem', function(x) x$ncell())

#' @export
methods::setGeneric('modelsense', function(x) standardGeneric('modelsense'))

#' @rdname OptimizationProblem-methods
#' @name modelsense
#' @usage modelsense(x)
#' @export
methods::setMethod('modelsense', 'OptimizationProblem', function(x) x$modelsense())

#' @export
methods::setGeneric('vtype', function(x) standardGeneric('vtype'))

#' @rdname OptimizationProblem-methods
#' @name vtype
#' @usage vtype(x)
#' @export
methods::setMethod('vtype', 'OptimizationProblem', function(x) x$vtype())

#' @export
methods::setGeneric('obj', function(x) standardGeneric('obj'))

#' @rdname OptimizationProblem-methods
#' @name obj
#' @usage obj(x)
#' @export
methods::setMethod('obj', 'OptimizationProblem', function(x) x$obj())

#' @export
methods::setGeneric('A', function(x) standardGeneric('A'))

#' @rdname OptimizationProblem-methods
#' @name A
#' @usage A(x)
#' @export
methods::setMethod('A', 'OptimizationProblem', function(x) x$A())

#' @export
methods::setGeneric('rhs', function(x) standardGeneric('rhs'))

#' @rdname OptimizationProblem-methods
#' @name rhs
#' @usage rhs(x)
#' @export
methods::setMethod('rhs', 'OptimizationProblem', function(x) x$rhs())

#' @export
methods::setGeneric('sense', function(x) standardGeneric('sense'))

#' @rdname OptimizationProblem-methods
#' @name sense
#' @usage sense(x)
#' @export
methods::setMethod('sense', 'OptimizationProblem', function(x) x$sense())

#' @export
methods::setGeneric('lb', function(x) standardGeneric('lb'))

#' @rdname OptimizationProblem-methods
#' @name lb
#' @usage lb(x)
#' @export
methods::setMethod('lb', 'OptimizationProblem', function(x) x$lb())

#' @export
methods::setGeneric('ub', function(x) standardGeneric('ub'))

#' @rdname OptimizationProblem-methods
#' @name ub
#' @usage ub(x)
#' @export
methods::setMethod('ub', 'OptimizationProblem', function(x) x$ub())

#' @export
methods::setGeneric('pu_indices_in_obj', function(x) standardGeneric('pu_indices_in_obj'))

#' @rdname OptimizationProblem-methods
#' @name pu_indices_in_obj
#' @usage pu_indices_in_obj(x)
#' @export
methods::setMethod('pu_indices_in_obj', 'OptimizationProblem', function(x) x$pu_indices_in_obj())

#' @export
methods::setGeneric('column_ids', function(x) standardGeneric('column_ids'))

#' @rdname OptimizationProblem-methods
#' @name column_ids
#' @usage column_ids(x)
#' @export
methods::setMethod('column_ids', 'OptimizationProblem', function(x) x$column_ids())

#' @export
methods::setGeneric('row_ids', function(x) standardGeneric('row_ids'))

#' @rdname OptimizationProblem-methods
#' @name row_ids
#' @usage row_ids(x)
#' @export
methods::setMethod('row_ids', 'OptimizationProblem', function(x) x$row_ids())

