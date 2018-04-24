#' @include internal.R
NULL

#' Show
#'
#' Display information about an object.
#'
#' @param x Any object.
#'
#' @param ... not used.
#'
#' @return None.
#'
#' @seealso \code{\link[methods]{show}}.
#'
#' @name show
#'
#' @aliases show,ConservationProblem-method show,ConservationModifier-method show,OptimizationProblem-method show,Id-method show,Parameter-method show,Solver-method
NULL

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{ConservationModifier}(x)
#'
methods::setMethod("show", "ConservationModifier",
                   function(object) object$show())

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{ConservationProblem}(x)
#'
methods::setMethod("show", "ConservationProblem",
                   function(object) object$show())

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{Id}(x)
#'
methods::setMethod("show", "Id",
                   function(object) print.Id(object))

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{OptimizationProblem}(x)
#'
methods::setMethod("show", "OptimizationProblem",
                   function(object) object$show())

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{Parameter}(x)
#'
methods::setMethod("show", "Parameter",
                   function(object) object$show())

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{Solver}(x)
#'
methods::setMethod("show", "Solver", function(object) object$show())
