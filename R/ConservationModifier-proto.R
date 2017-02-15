#' @include internal.R
NULL

#' @export
methods::setOldClass('ConservationModifier')

#' Conservation problem modifier prototype
#'
#' This super-prototype is used to represent prototypes that in turn are used to 
#' modify a \code{\link{ConservationProblem}} object. Specifically, the
#' \code{\link{Constraint}}, \code{\link{Decision}}, \code{\link{Objective}}
#' and \code{\link{Target}} prototypes inherit from this class. \strong{
#' Only the most expert experts should use this class directly because changes
#' to these class will have profound and far reaching effects.}
#'
#' @section Fields:
#' \itemize{
#' \item{$name}{\code{character} name of object.}
#'
#' \item{$parameters}{\code{list} object used to customize the modifier.}
#'
#' \item{$data}{\code{list} object with data.}
#'
#' \item{$prevalidate}{\code{function} used to validate that parameters and 
#'                      data are compatible with a 
#'                      \code{\link{ConservationProblem}} object.}
#'
#' \item{$synchronize}{\code{function} used to synchronize the parameters and
#'                   and data in the object with the data in a 
#'                   \code{\link{ConservationProblem}}.}
#'
#' \item{$postvalidate}{\code{function} used to validate that parameters and 
#'                      data are still compatible with a 
#'                      \code{\link{ConservationProblem}} object after they 
#'                       have been synchronized.}
#'
#' \item{$output}{\code{function} used to generate an output from the object.}
#'
#' \item{$apply}{\code{function} used to apply the modifier to a 
#'               \code{\link{OptimizationProblem}} object. In most cases, this 
#'               involves calling a function written in \emph{Rcpp} to add
#'               objectives, targets, or constraints to the 
#'               \code{\link{OptimizationProblem}} object.}
#'
#' }
#'
#' @section Usage:
#' \preformatted{x <- ConservationModifier$new(...)}
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$repr()}
#'
#' \code{x$prevalidate(cp)}
#' \code{x$synchronize(cp)}
#' \code{x$postvalidate(cp)}
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
#' @noRd
ConservationModifier <- pproto(
  'ConservationModifier',
    name = 'no name',
    parameters = parameters(),
    data = list(),
    prevalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      invisible(TRUE)
    },
    postvalidate = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      invisible(TRUE)
    },
    synchronize = function(self, x) {
      assertthat::assert_that(inherits(x, 'ConservationProblem'))
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'))
      assertthat::assert_that(inherits(y, 'ConservationProblem'))
      stop('no defined apply method')
    },
    output = function(self) {
      stop('no defined output method')
    },
    print = function(self) {
      message(self$repr())
    },
    show = function(self) {
      self$print()
    },
    repr = function(self) {
      paste0(self$name, repr(self$parameters))
    },
    get_parameter = function(self, x) {
      self$parameters$get(x)
    },
    set_parameter = function(self, x, value) {
      self$parameters$set(x, value)
    },
    render_parameter = function(self, x) {
      self$parameters$render(x)
    },
    get_all_parameters = function(self) {
      structure(lapply(self$parameters, function(x){x$value}),
                .Names=sapply(self$parameters, function(x){x$name}),
                id=sapply(self$parameters, function(x){x$id}))
    },
    render_all_parameters = function(self) {
      shiny::div(class=class(self)[1],
                 self$parameters$render_all())
    })

