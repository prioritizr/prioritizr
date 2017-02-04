#' @include internal.R Parameters-class.R ConservationModifier-class.R
NULL

#' @export
methods::setOldClass('Target')
  
#' Target class
#'  
#' This class is used to represent the targets used when making a
#' prioritization. \strong{This class represents a recipe, to actually specify 
#' add targets to a planning problem, see the help page on
#' \code{\link{targets}}. Only experts should use this class directly.}
#'
#' @section Fields:
#' \itemize{
#' \item{$name}{\code{character} name of target}
#' \item{$parameters}{\code{\ link{Parameters}} object used to customize the
#'    target.}
#' \item{$data}{\code{list} object with data.}
#' \item{$validate}{\code{function} used to validate that the targets
#'    are compatible with a given \code{\link{ConservationProblem}}
#'    object.}
#' \item{$apply}{\code{function} used to apply the targets
#'    to a \code{\link{OptimizationProblem}} object.}
#' }
#'
#' @section Usage:
#' \preformatted{x <- Target$new(name, parameters, data,
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
#' \item{validate}{validate that data used to formulate the targets
#'   are compatible with data in a \code{ConservationProblem} object.}
#' \item{apply}{add targets to an
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
#' @name Target
NULL

#' @export
Target <- R6::R6Class('Target',
  inherit=ConservationModifier,
  public = list(
    initialize = function(name, data, parameters, validate, apply) {
      assertthat::assert_that(
        assertthat::is.string(name),
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
      message('Target object\n  name: ', self$target)
      if (parameters$length() > 0)
        parameters$print()
    },
    render_all_parameters = function() {
      shiny::div(class='Target',
                 self$parameters$render_all())
    }    
  )
)