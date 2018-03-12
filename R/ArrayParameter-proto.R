#' @include internal.R pproto.R Parameter-proto.R
NULL

#' @export
methods::setOldClass("ArrayParameter")

#' Array Parameter prototype
#'
#' This prototype is used to represent a parameter has multiple values. Each
#' value is has a label to differentiate values. \strong{Only experts should
#' interact directly with this prototype.}
#'
#' @section Fields:
#'
#' \describe{
#'
#' \item{$id}{\code{character} identifier for parameter.}
#'
#' \item{$name}{\code{character} name of parameter.}
#'
#' \item{$value}{\code{numeric} \code{vector} of values.}
#'
#' \item{$label}{\code{character} \code{vector} of names for each value.}
#'
#' \item{$default}{\code{numeric} \code{vector} of default values.}
#'
#' \item{$length}{\code{integer} number of values.}
#'
#' \item{$class}{\code{character} class of values.}
#'
#' \item{$lower_limit}{\code{numeric} \code{vector} specifying the minimum
#'   permitted values.}
#'
#' \item{$upper_limit}{\code{numeric} \code{vector} specifying the maximum
#'   permitted values.}
#'
#' \item{$widget}{\code{function} used to construct a
#'   \code{\link[shiny]{shiny}} interface for modifying values.}
#' }
#'
#' @section Usage:
#'
#' \code{x$print()}
#'
#' \code{x$show()}
#'
#' \code{x$repr()}
#'
#' \code{x$validate(tbl)}
#'
#' \code{x$get()}
#'
#' \code{x$set(tbl)}
#'
#' \code{x$reset()}
#'
#' \code{x$render(...)}
#'
#' @section Arguments:
#' \describe{
#' \item{tbl}{\code{\link{data.frame}} containing new parameter values with
#'            row names indicating the labels and a column called "values"
#'            containing the new parameter values.}
#' \item{...}{arguments passed to function in \code{widget} field.}
#' }
#'
#' @section Details:
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{\code{character} representation of object.}
#'
#' \item{validate}{check if a proposed new set of parameters are valid.}
#'
#' \item{get}{return a \code{\link[base]{data.frame}} containing the
#'   parameter values.}
#'
#' \item{set}{update the parameter values using a
#'   \code{\link[base]{data.frame}}.}
#'
#' \item{reset}{update the parameter values to be the default values.}
#'
#' \item{render}{create a \code{\link[shiny]{shiny}} widget to modify
#'   parameter values.}
#'
#' }
#'
#' @seealso \code{\link{ScalarParameter-class}}, \code{\link{Parameter-class}}.
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
                  " this parameter."))
    # extract function
    f <- do.call(utils::getFromNamespace,
      as.list(rev(strsplit(self$widget, "::")[[1]])))
    do.call(f, list(outputId = self$id))
  })
