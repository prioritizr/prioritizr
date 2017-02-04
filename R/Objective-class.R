#' @include internal.R Parameters-class.R ConservationModifier-class.R
NULL

#' @export
methods::setOldClass('Objective')

#' Objective class
#'
#' This class is used to represent an objective that can be added to a
#' \code{\link{ConservationProblem}} object. \strong{This class represents a
#' a recipe to make an objective, to actually add an objective to a planning 
#' problem: use functions ending with "_objective". Only experts should use this
#' class directly.}
#'
#' @section Fields:
#' \itemize{
#' \item{$name}{\code{character} name of constraint.}
#' \item{$modelsense}{\code{character} specifying if objective function is
#'    to be minimized ('min') or maximized ('max').}
#' \item{$parameters}{\code{\ link{Parameters}} object used to customize the
#'    objective.}
#' \item{$data}{\code{list} object with data.}
#' \item{$validate}{\code{function} used to validate that the objective
#'    is compatible with a given \code{\link{ConservationProblem}}
#'    object.}
#' \item{$apply}{\code{function} used to apply the objective
#'    to a \code{\link{OptimizationProblem}} object.}
#' }
#'
#' @section Usage:
#' \preformatted{x <- Objective$new(name, modelsense, parameters, data,
#'                                  validate, apply)}
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
#' \code{x$render_all_parameter()}
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
#' \item{validate}{validate that data used to formulate the objective
#'   are compatible with data in a \code{ConservationProblem} object}
#' \item{apply}{add objective to an
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
#' @name Objective
NULL

#' @export
Objective <- R6::R6Class('Objective',
  inherit=ConservationModifier,
  public = list(
    modelsense=NULL,
    initialize = function(name, modelsense, data, parameters, validate, apply) {
      assertthat::assert_that(
        assertthat::is.string(name), assertthat::is.string(modelsense),
        modelsense %in% c('min', 'max'),
        inherits(parameters, 'list'),
        inherits(data, 'list'),
        inherits(validate, 'function'),
        inherits(apply, 'function'))
      self$name <- name
      self$modelsense <- modelsense
      self$data <- data
      self$parameters <- parameters
      self$validate <- validate
      self$apply <- apply
    },
    print = function() {
      message('Objective object\n  name: ', self$name)
      if (parameters$length() > 0)
        parameters$print()
    },
    render_all_parameters = function() {
      shiny::div(class='Objective',
                 self$parameters$render_all())
    }        
  )
)
