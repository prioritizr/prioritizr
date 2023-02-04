#' @include internal.R waiver.R pproto.R assertions.R
NULL

#' @export
if (!methods::isClass("ConservationModifier")) methods::setOldClass("ConservationModifier")
NULL

#' Conservation problem modifier prototype
#'
#' This super-prototype is used to represent prototypes that in turn are used to
#' modify a [`ConservationProblem-class`] object. Specifically, the
#' [`Constraint-class`], [`Decision-class`],
#' [`Objective-class`], and [`Target-class`] prototypes
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
#' \item{$data}{`list` object with data.}
#'
#' \item{$internal}{`list` object with internal data.}
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
#' `x$get_internal(name)`
#'
#' `x$set_internal(name, value)`
#'
#' `x$calculate(cp)`
#'
#' `x$output()`
#'
#' `x$apply(op,cp)`
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
#' \item{cp}{[`ConservationProblem-class`] object}
#'
#' \item{op}{[`OptimizationProblem-class`] object}
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
#' \item{get_internal}{return an object stored in the `internal` field with
#'   the corresponding `name`. If the object is not present in the
#'   `data` field, a `waiver` object is returned.}
#'
#' \item{set_internal}{store an object stored in the `internal` field with
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
#'   This method is only used for [`Target-class`] objects.}
#'
#' \item{apply}{`function` used to apply the modifier to an
#'   [`OptimizationProblem-class`] object.
#'   This is used by [`Constraint-class`],
#'   [`Decision-class`], and [`Objective-class`] objects.}
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
  data = list(),
  internal = list(),
  compressed_formulation = TRUE,
  calculate = function(self, y) {
    invisible(TRUE)
  },
  apply = function(self, x, y) {
    cli::cli_abort("No defined $apply method.", .internal = TRUE)
  },
  output = function(self) {
    cli::cli_abort("No defined $output method.", .internal = TRUE)
  },
  print = function(self) {
    cli::cli_text(self$repr())
  },
  show = function(self) {
    self$print()
  },
  repr = function(self, compact = TRUE) {
    repr_data_list(self$name, self$data, compact = compact)
  },
  get_data = function(self, x) {
    self$data[[x]] %||% new_waiver()
  },
  set_data = function(self, x, value) {
    self$data[[x]] <- value
    invisible()
  },
  get_internal = function(self, x) {
    self$internal[[x]] %||% new_waiver()
    self$data[[x]]
  },
  set_internal = function(self, x, value) {
    self$internal[[x]] <- value
    invisible()
  }
)
