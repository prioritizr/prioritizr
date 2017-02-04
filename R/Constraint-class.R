#' @include internal.R Parameters-class.R ConservationModifier-class.R
NULL

#' @export
methods::setOldClass('Constraint')

#' Constraint class
#'
#' This class is used to represent a constraint that can be added to a 
#' \code{\link{ConservationProblem}} object. \strong{This class represents a
#' a constraint, to actually add constraints to a planning problem:
#' use functions ending with "_constraint". Only experts should use this
#' class directly.}
#'
#' @section Fields:
#' \itemize{
#' \item{$name}{\code{character} name of constraint.}
#' \item{$parameters}{\code{list} object used to customize the constraint.}
#' \item{$data}{\code{list} object with data to constrain problem.}
#' \item{$validate}{\code{function} used to validate that 
#'                                  constraint data are appropriate given a 
#'                                  \code{\link{ConservationProblem}} object.}
#' \item{$apply}{\code{function} used to apply constraint to a 
#'                           \code{\link{OptimizationProblem}} object.}
#' }
#'
#' @section Usage:
#' \preformatted{x <- Constraint$new(name, parameters, data, validate, apply)}
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$repr()}
#'
#' \code{x$validate(cp)}
#' \code{x$apply(op)}
#'
#' \code{x$get_parameter(id)}
#' \code{x$get_all_parameters()}
#' \code{x$set_parameter(id, value)}
#' \code{x$render_parameter(id)}
#' \code{x$render_all_parameters()}
#'
#' @section Arguments:
#' \describe{
#' \item{id}{\code{id} object.}
#' \item{value}{any object.}
#' \item{cp}{\code{\link{ConservationProblem}} object.}
#' \item{op}{\code{\link{OptimizationProblem}} object.}
#' }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{repr}{\code{character} representation of the object.}
#' \item{validate}{validate that data used to formulate the constraints
#' with data in a \code{ConservationProblem} object}
#' \item{apply}{add constraint to a 
#'       \code{\link{OptimizationProblem}} object.}
#' \item{get_parameter}{retrieve the value of a parameter.}
#' \item{get_all_parameters}{generate \code{list} containing all the 
#'   parameters.}
#' \item{set_parameter}{change the value of a parameter to new value.}
#' \item{render_parameter}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#' \item{render_all_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#' }
#'
#' @name Constraint
NULL

#' @export
Constraint <- R6::R6Class('Constraint',
  inherit = ConservationModifier, 
  public = list(
    initialize = function(name, data, parameters, validate, apply) {
      assertthat::assert_that(
        is.character(name), length(name)==1,
        inherits(parameters, 'Parameters'),
        inherits(data, 'list'),
        inherits(validate, 'function'),
        inherits(apply, 'function'))
      self$name <- name
      self$data <- data
      self$parameters <- parameters
      self$validate <- validate
      self$apply <- apply
    },
    print = function() {
      message('Constraint object\n  name: ', self$name)
    },
    render_all_parameters = function() {
      shiny::div(class='Constraint',
                 self$parameters$render_all())
    }    
  )
)
