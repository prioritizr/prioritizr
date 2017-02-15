#' @include internal.R Constraint-proto.R
NULL

#' @export
methods::setOldClass('Objective')

#' Constraints prototype
#'
#' This prototype represents a collection of \code{\link{Constraint}} objects.
#'
#' @section Fields:
#' \itemize{
#'   \item{$constraints}{\code{list} object containing \code{\link{Constraint}} 
#'    objects.}
#' }
#'
#' @section Usage:
#' \preformatted{x <- Constraint$new()}
#' \code{x$print()}
#' \code{x$show()}
#' \code{x$repr()}
#'
#' \code{x$add}
#'
#' \code{x$get_parameter(id)}
#' \code{x$set_parameter(id, value)}
#' \code{x$render_parameter(id)}
#' \code{x$render_all_parameters()}
#'
#' @section Arguments:
#' \describe{
#' \item{id}{\code{id} object.}
#' \item{value}{any object.}
#' }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{repr}{\code{character} representation of object.}
#'
#' \item{add}{add \code{Constraint} object.}
#'
#' \item{get_parameter}{retrieve the value of a parameter in the object
#'   using an \code{id} object.}
#' \item{set_parameter}{change the value of a parameter in the object 
#'   to a new object.}
#' \item{render_parameter}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#' \item{render_all_parameters}{generate a \emph{shiny} \code{div}
#'   containing all the parameters' widgets.}
#' }
#'
#' @name Constraints
NULL

#' @export
Constraints <- pproto(
  'Constraints',
  constraints = list(),
  repr = function(self) {
    if (length(self$constraints)>0)
      return(paste0('<', paste(sapply(self$constraints, function(x) {x$repr()}),
        collapse='\n'), '>'))
    return('<none>')
  },
  find_parameter = function(id) {
    r <- sapply(self$constraints,
                function(x) {
                  id %in% sapply(x$parameters, 
                                 function(z) {z$id})})
    s <- sum(r)
    if (s==0) {
      stop('no parameter with matching id found')
    } else if (s > 1) {
      stop('multiple parameters with matching id found')
    }
    which(r)
  },  
  add = function(self, x) {
    assertthat::assert_that(inherits(x), 'Constraint')
    self$constraints <- append(self$constraints, x)
  },
  get_parameter = function(self, id) {
    assertthat::assert_that(inherits(id), 'id')
    self$constraints[[self$find_parameter(id)]]$get_parameter(id)
  },
  set_parameter = function(self, id, value) {
    assertthat::assert_that(inherits(id), 'id')
    self$constraints[[self$find_parameter(id)]]$set_parameter(id, value)
  },
  render_parameter = function(self, id, value) {
    assertthat::assert_that(inherits(id), 'id')
    self$constraints[[self$find_parameter(id)]]$render_parameter(id)
  },
  render_all_parameters = function(self) {
    do.call(shiny::div, 
        append(list(class='Constraints'), 
                lapply(self$constraints,
                       function(x) {x$render_all_parameters()})))
  })
