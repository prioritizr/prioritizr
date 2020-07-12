#' @include internal.R pproto.R Parameter-proto.R
NULL

#' @export
if (!methods::isClass("ArrayParameter")) methods::setOldClass("ArrayParameter")
NULL

#' Array parameter prototype
#'
#' This prototype is used to represent a parameter has multiple values. Each
#' value is has a label to differentiate values. **Only experts should
#' interact directly with this prototype.**
#'
#' @section Fields:
#'
#' \describe{
#'
#' \item{$id}{`character` identifier for parameter.}
#'
#' \item{$name}{`character` name of parameter.}
#'
#' \item{$value}{`numeric` `vector` of values.}
#'
#' \item{$label}{`character` `vector` of names for each value.}
#'
#' \item{$default}{`numeric` `vector` of default values.}
#'
#' \item{$length}{`integer` number of values.}
#'
#' \item{$class}{`character` class of values.}
#'
#' \item{$lower_limit}{`numeric` `vector` specifying the minimum
#'   permitted values.}
#'
#' \item{$upper_limit}{`numeric` `vector` specifying the maximum
#'   permitted values.}
#'
#' \item{$widget}{`function` used to construct a
#'   [shiny::shiny()] interface for modifying values.}
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
#' `x$validate(tbl)`
#'
#' `x$get()`
#'
#' `x$set(tbl)`
#'
#' `x$reset()`
#'
#' `x$render(...)`
#'
#' @section Arguments:
#' \describe{
#' \item{tbl}{[data.frame()] containing new parameter values with
#'            row names indicating the labels and a column called "values"
#'            containing the new parameter values.}
#' \item{...}{arguments passed to function in `widget` field.}
#' }
#'
#' @section Details:
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{`character` representation of object.}
#'
#' \item{validate}{check if a proposed new set of parameters are valid.}
#'
#' \item{get}{return a [base::data.frame()] containing the
#'   parameter values.}
#'
#' \item{set}{update the parameter values using a
#'   [base::data.frame()].}
#'
#' \item{reset}{update the parameter values to be the default values.}
#'
#' \item{render}{create a [shiny::shiny()] widget to modify
#'   parameter values.}
#'
#' }
#'
#' @seealso [`ScalarParameter-class`], [`Parameter-class`].
#'
#' @name ArrayParameter-class
#'
#' @aliases ArrayParameter
NULL

#' @export
ArrayParameter <- pproto(
  "ArrayParameter",
  Parameter,
  label = character(0),
  upper_limit = numeric(0),
  lower_limit = numeric(0),
  length = 0,
  repr = function(self) {
    paste0(self$name, " (min: ", min(self$value), ", max: ",
      max(self$value), ")")
  },
  validate = function(self, x) {
    assertthat::assert_that(inherits(x, "data.frame"))
    invisible(assertthat::see_if(
      identical(names(x), "value"),
      all(is.finite(x[[1]])),
      ncol(x) == 1,
      nrow(x) == self$length,
      inherits(x[[1]], self$class),
      setequal(self$label, rownames(x)),
      sum(!is.finite(x[[1]])) == 0,
      isTRUE(all(x[[1]] >= self$lower_limit)),
      isTRUE(all(x[[1]] <= self$upper_limit))))
  },
  set = function(self, x) {
    check_that(self$validate(x))
    self$value <- x[[1]][match(rownames(x), self$label)]
  },
  get = function(self) {
    structure(list(value = self$value),
              .Names = "value",
              row.names = self$label,
              class = "data.frame")
  },
  render = function(self, ...) {
    # check that widget dependency installed
    pkg <- strsplit(self$widget, "::")[[1]][[1]]
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(paste0("the \"", pkg, "\" R package must be installed to render",
                  " this parameter"))
    # extract function
    f <- do.call(utils::getFromNamespace,
      as.list(rev(strsplit(self$widget, "::")[[1]])))
    do.call(f, list(outputId = self$id))
  })
