#' @include internal.R Parameters-class.R
NULL

#' @export
methods::setOldClass('OptimizationProblem')

#' OptimizationProblem class
#'
#' This class is used to represent an optimization problem. Data are stored
#' in memory using an external pointer. \strong{Only experts interact with this
#' class directly.}
#'
#' @section Fields:
#' \itemize{
#' \item{$ptr}{\code{externalptr} object.}
#' }
#'
#' @section Usage:
#' \preformatted{x <- OptimizationProblem$new()}
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$repr()}
#'
#' \code{x$ncol()}
#' \code{x$nrow()}
#' \code{x$ncell()}
#'
#' \code{x$modelsense()}
#' \code{x$vtype()}
#' \code{x$obj()}
#' \code{x$A()}
#' \code{x$rhs()}
#' \code{x$sense()}
#' \code{x$lb()}
#' \code{x$ub()}
#'
#' \code{x$pu_indices_obj()}
#' \code{x$row_ids()}
#' \code{x$column_ids()}
#'
#' @section Arguments:
#' \describe{
#' \item{ptr}{\code{externalptr} object.}
#' }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{repr}{\code{character} representation of object.}
#' \item{ncol}{\code{integer} number of columns (variables) in model matrix.}
#' \item{nrow}{\code{integer} number of rows (constraints) in model matrix.}
#' \item{ncell}{\code{integer} number of cells in model matrix.}
#' \item{modelsense}{\code{character} model sense.}
#' \item{vtype}{\code{character} vector of variable types.}
#' \item{obj}{\code{numeric} vector of objective function.}
#' \item{A}{\code{\link[Matrix]{dgCMatrix-class}} model matrix }
#' \item{rhs}{\code{numeric} vector of right-hand-side constraints.}
#' \item{sense}{\code{character} vector of constraint senses.}
#' \item{lb}{\code{numeric} vector of lower bounds for each column.}
#' \item{ub}{\code{numeric} vector of upper bounds for each column.}
#' \item{pu_indices_obj}{\code{integer} vector of indices in objective function that correspond
#'   to the status of planning units.}
#' \item{column_ids}{\code{character} constraint ids for each column in the model matrix.}
#' \item{row_ids}{\code{character} constraint ids for each row in the model matrix.}
#' }
#'
#' @name OptimizationProblem
NULL

#' @export
OptimizationProblem <- R6::R6Class('OptimizationProblem',
  private = list(
    ptr = NULL
  ),
  public = list(
    initialize = function() {
      private$ptr <- rcpp_new_optimization_problem()
    },
    print = function() {
      if (self$ncol()>0) {
      cv <- table(self$vtype())
      cv <- paste(paste(unname(cv), paste('(', names(cv), ')')),
            collapse=', ')
      message('OptimizationProblem object',
'\n  model sense: ', self$modelsense(), 
'\n  dimensions:  ', self$nrow(), ', ', self$ncol(), ', ', self$ncell(), 
                    '(nrow, ncol, ncell)',
'\n  variables:    ',cv)
      } else {
        message('OptimizationProblem object (empty)')
      }
    },
    show = function() {
      self$print()
    }, 
    ncol = function() {
      rcpp_get_optimization_problem_ncol(private$ptr)
    },
    nrow = function() {
      rcpp_get_optimization_problem_nrow(private$ptr)
    },
    ncell = function() {
      rcpp_get_optimization_problem_ncell(private$ptr)
    },    
    modelsense = function() {
      rcpp_get_optimization_problem_modelsense(private$ptr)
    },
    vtype = function() {
      rcpp_get_optimization_problem_vtype(private$ptr)
    },
    obj = function() {
      rcpp_get_optimization_problem_obj(private$ptr)
    },
    A = function() {
      rcpp_get_optimization_problem_A(private$ptr)
    },
    rhs = function() {
      rcpp_get_optimization_problem_rhs(private$ptr)
    },
    sense = function() {
      rcpp_get_optimization_problem_sense(private$ptr)
    },
    lb = function() {
      rcpp_get_optimization_problem_lb(private$ptr)
    },
    ub = function() {
      rcpp_get_optimization_problem_ub(private$ptr)
    },
    pu_indices_obj = function() {
    rcpp_get_optimization_problem_pu_indices_obj(private$ptr) 
    },
    column_ids = function() {
      rcpp_get_optimization_problem_column_ids(private$ptr)
    },
    row_ids = function() {
      rcpp_get_optimization_problem_column_ids(private$ptr)
    }
  )
)
