#' @include internal.R waiver.R pproto.R parameters.R
NULL

#' @export
if (!methods::isClass("ConservationModifier")) methods::setOldClass("ConservationModifier")
NULL

#' Conservation problem modifier prototype
#'
#' This super-prototype is used to represent prototypes that in turn are used to
#' modify a \code{\link{ConservationProblem-class}} object. Specifically, the
#' \code{\link{Constraint-class}}, \code{\link{Decision-class}},
#' \code{\link{Objective-class}}, and \code{\link{Target-class}} prototypes
#' inherit from this class. **Only experts should interact with
#' this class directly because changes to these class will have profound and
#' far reaching effects.**
#'
#' @section Fields:
#'
#' \describe{
#'
#' \item{$name}{`character` name of object.}
#'
#' \item{$parameters}{`list` object used to customize the modifier.}
#'
#' \item{$data}{`list` object with data.}
#'
#' \item{$compressed_formulation}{`logical` can this constraint be applied
#'    to the compressed version of the conservation planning problem?. Defaults
#'    to `TRUE`.}
#'
#' }
#'
#' @section Usage:
#'
#' `x$print()`
#'
#' `x$show()`
#'
#' `x$repr()`
#'
#' `x$get_data(name)`
#'
#' `x$set_data(name, value)`
#'
#' `x$calculate(cp)`
#'
#' `x$output()`
#'
#' `x$apply(op,cp)`
#'
#' `x$get_parameter(id)`
#'
#' `x$get_all_parameters()`
#'
#' `x$set_parameter(id, value)`
#'
#' `x$render_parameter(id)`
#'
#' `x$render_all_parameter()`
#'
#' @section Arguments:
#' \describe{
#'
#' \item{name}{`character` name for object}
#'
#' \item{value}{any object}
#'
#' \item{id}{`id` or `name` of parameter}
#'
#' \item{cp}{\code{\link{ConservationProblem-class}} object}
#'
#' \item{op}{\code{\link{OptimizationProblem-class}} object}
#'
#' }
#'
#' @section Details:
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{return `character` representation of the object.}
#'
#' \item{get_data}{return an object stored in the `data` field with
#'   the corresponding `name`. If the object is not present in the
#'   `data` field, a `waiver` object is returned.}
#'
#' \item{set_data}{store an object stored in the `data` field with
#'   the corresponding name. If an object with that name already
#'   exists then the object is overwritten.}
#'
#' \item{calculate}{`function` used to perform preliminary calculations
#'   and store the data so that they can be reused later without
#'   performing the same calculations multiple times. Data can be stored
#'   in the `data` slot of the input `ConservationModifier` or
#'   `ConservationProblem` objects.}
#'
#' \item{output}{`function` used to generate an output from the object.
#'   This method is only used for \code{\link{Target-class}} objects.}
#'
#' \item{apply}{`function` used to apply the modifier to an
#'   \code{\link{OptimizationProblem-class}} object.
#'   This is used by \code{\link{Constraint-class}},
#'   \code{\link{Decision-class}}, and \code{\link{Objective-class}} objects.}
#'
#' \item{get_parameter}{retrieve the value of a parameter.}
#'
#' \item{get_all_parameters}{generate `list` containing all the
#'   parameters.}
#'
#' \item{set_parameter}{change the value of a parameter to new value.}
#'
#' \item{render_parameter}{generate a *shiny* widget to modify the
#'   the value of a parameter (specified by argument `id`).}
#'
#' \item{render_all_parameters}{generate a \code{\link[shiny]{div}}
#'   containing all the parameters" widgets.}
#'
#' }
#'
#' @name ConservationModifier-class
#'
#' @aliases ConservationModifier
NULL

#' @export
ConservationModifier <- pproto(
  "ConservationModifier",
  name = character(0),
  parameters = parameters(),
  data = list(),
  compressed_formulation = TRUE,
  calculate = function(self, y) {
    invisible(TRUE)
  },
  apply = function(self, x, y) {
    assertthat::assert_that(inherits(x, "OptimizationProblem"))
    assertthat::assert_that(inherits(y, "ConservationProblem"))
    stop("no defined apply method")
  },
  output = function(self) {
    stop("no defined output method")
  },
  print = function(self) {
    message(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    paste(self$name, gsub("[]", "", self$parameters$repr(), fixed = TRUE))
  },
  get_data = function(self, x) {
    if (!x %in% names(self$data))
      return(new_waiver())
    return(self$data[[x]])
  },
  set_data = function(self, x, value) {
    self$data[[x]] <- value
    invisible()
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
    structure(lapply(self$parameters, function(x) x$value),
      .Names = vapply(self$parameters, function(x) x$name, character(1)),
      id = vapply(self$parameters, function(x) as.character(x$id),
                  character(1)))
  },
  render_all_parameters = function(self) {
    shiny::div(class = class(self)[1], self$parameters$render_all())
  })
