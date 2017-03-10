#' @include internal.R pproto.R Constraint-proto.R Penalty-proto.R
NULL

#' @export
methods::setOldClass("Collection")

#' Collection prototype
#'
#' This prototype represents a collection of
#' \code{\link{ConservationModifier-class}} objects.
#'
#' @section Fields:
#' \describe{
#'   \item{$...}{\code{\link{ConservationModifier-class}} objects stored
#'      in the collection.}
#' }
#'
#' @section Usage:
#' \code{x$print()}
#'
#' \code{x$show()}
#'
#' \code{x$repr()}
#'
#' \code{x$ids()}
#'
#' \code{x$length()}
#'
#' \code{x$add}
#'
#' \code{x$remove(id)}
#'
#' \code{x$get_parameter(id)}
#'
#' \code{x$set_parameter(id, value)}
#'
#' \code{x$render_parameter(id)}
#'
#' \code{x$render_all_parameters()}
#'
#' @section Arguments:
#' \describe{
#'
#' \item{id}{\code{id} object.}
#'
#' \item{value}{any object.}
#'
#' }
#'
#' @section Details:
#'
#' \describe{
#'
#' \item{print}{print the object.}
#'
#' \item{show}{show the object.}
#'
#' \item{repr}{\code{character} representation of object.}
#'
#' \item{ids}{\code{character} ids for objects inside collection.}
#'
#' \item{length}{\code{integer} number of objects inside collection.}
#'
#' \item{add}{add \code{\link{ConservationModifier-class}} object.}
#'
#' \item{remove}{remove an item from the collection.}
#'
#' \item{get_parameter}{retrieve the value of a parameter in the object
#'   using an \code{id} object.}
#'
#' \item{set_parameter}{change the value of a parameter in the object
#'   to a new object.}
#'
#' \item{render_parameter}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#'
#' \item{render_all_parameters}{generate a \code{\link[shiny]{div}}
#'   containing all the parameters" widgets.}
#'
#' }
#'
#' @seealso \code{\link{Constraint-class}}, \code{\link{Penalty-class}}.
#'
#' @name Collection-class
#'
#' @aliases Collection
NULL

#' @export
Collection <- pproto(
  "Collection",
  repr = function(self) {
    if (base::length(self$ids()) > 0)
      return(paste0("<", paste(vapply(self$ids(),
                                      function(z) self[[z]]$repr(),
                                      character(1)),
                         collapse = "\n"), ">"))
    return("<none>")
  },
  find_parameter = function(id) {
    n <- self$ids()
    r <- vapply(n, function(x) {
        id %in% vapply(self[[x]]$parameters, function(z) z$id)
    }, logical(1))
    s <- sum(r)
    if (s == 0) {
      stop("no parameter with matching id found")
    } else if (s > 1) {
      stop("multiple parameters with matching id found")
    }
    n[r]
  },
  find = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x) || is.id(x))
      if (inherits(x, "Id")) {
        return(x)
    } else {
      n <- self$ids()
      x <- match(x, vapply(n, function(j) self[[j]]$name), character(1))
      if (!is.finite(x))
        stop("item with matching name not found")
      if (base::length(x) > 1)
        stop("multiple items with the same name")
      return(n[x])
    }
  },
  ids = function(self) {
    o <- self$ls()
    o[!vapply(o, function(x) inherits(self[[x]], "function"), logical(1))]
  },
  length = function(self) {
    base::length(self$ids())
  },
  add = function(self, x) {
    assertthat::assert_that(inherits(x, "ConservationModifier"))
    self[[new_id()]] <- x
    invisible()
  },
  remove = function(self, x) {
    assertthat::assert_that(is.Id(x))
    rm(list = as.character(x), envir = self)
    invisible(TRUE)
  },
  get_parameter = function(self, id) {
    assertthat::assert_that(inherits(id), "Id")
    self[[self$find_parameter(id)]]$get_parameter(id)
  },
  set_parameter = function(self, id, value) {
    assertthat::assert_that(inherits(id), "Id")
    self[[self$find_parameter(id)]]$set_parameter(id, value)
  },
  render_parameter = function(self, id, value) {
    assertthat::assert_that(inherits(id), "Id")
    self[[self$find_parameter(id)]]$render_parameter(id)
  },
  render_all_parameters = function(self) {
    do.call(shiny::div,
        append(list(class = "Collection"),
                lapply(self$ids(), function(x) {
                  self[[x]]$render_all_parameters()
                })))
  })
