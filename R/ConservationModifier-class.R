#' @include internal.R
NULL

#' @export
methods::setOldClass('ConservationModifier')

#' Conservation problem modifier class
#'
#' This super-class is used to represent classes that in turn are used to 
#' modify a \code{\link{ConservationProblem}} object. Specifically, the
#' \code{\link{Constraint}}, \code{\link{Decision}}, \code{\link{Objective}}
#' and \code{\link{Target}} classes are inherit from this class. \strong{
#' Only experts should use this class directly.}
#'
#'
#' @section Fields:
#' \itemize{
#' \item{$name}{\code{character} name of object.}
#' \item{$parameters}{\code{list} object used to customize the modifier.}
#' \item{$data}{\code{list} object with data.}
#' \item{$validate}{\code{function} used to validate that parameters and 
#'                                  data are compatible with a 
#'                                  \code{\link{ConservationProblem}} object.}
#' \item{$apply}{\code{function} used to apply the modifier to a 
#'                           \code{\link{OptimizationProblem}} object.}
#' }
#'
#' @section Usage:
#' \preformatted{x <- ConservationModifier$new(name, parameters, data, validate, apply)}
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
#' \item{print}{print the object}
#' \item{show}{show the object}
#' \item{validate}{validate that data are compatible with a
#'    \code{ConservationProblem} object}
#' \item{apply}{modify a \code{\link{OptimizationProblem}} object.}
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
#' @name ConservationModifier
NULL

#' @export
ConservationModifier <- R6::R6Class('ConservationModifier',
  public = list(
    name = NULL,
    parameters = NULL,
    data = NULL,
    validate = NULL,
    apply = NULL,
    initialize = function(name, data, parameters, validate, apply) {
      self$name <- name
      self$data <- data
      self$parameters <- parameters
      self$validate <- validate
      self$apply <- apply
    },
    print = function() {
      message('ConservationModifier object',
      '\n  name:       ', self$name,
      '\n  parameters: ', repr(self$parameters))
    },
    show = function() {
      self$print()
    },
    repr = function() {
      paste0(self$name, ' [',  repr(self$parameters), ']')
    },
    get_parameter = function(x) {
      self$parameters$get(x)
    },
    set_parameter = function(x, value) {
      self$parameters$set(x, value)
    },
    render_parameter = function(x) {
      self$parameters$render(x)
    },
    get_all_parameters = function() {
      structure(lapply(self$parameters, function(x){x$value}),
                .Names=sapply(self$parameters, function(x){x$name}),
                id=sapply(self$parameters, function(x){x$id}))
    },
    render_all_parameters = function() {
      shiny::div(class='ConstraintModifier',
                 self$parameters$render_all())
    }
  )
)

