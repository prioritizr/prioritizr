#' @include internal.R
NULL

#' @export
if (!methods::isClass("Method")) methods::setOldClass("Method")
NULL

#' Method class
#'
#' @description
#' This class is used to represent methods for setting targets.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name Method-class
#'
#' @family classes
#'
#' @export
Method <- R6::R6Class(
  "Method",
  public = list(

    #' @field name `character` value with name of method.
    name = character(0),

    #' @field type `character` value denoting the target type.
    type = character(0),

    #' @field fun `function` for calculating targets.
    fun = NULL,

    #' @field args `list` containing arguments.
    args = list(),

    #' @description
    #' Initialize new object.
    #' @param name `character` value with name of method.
    #' @param type `character` value denoting the target type.
    #' Available options include `"relative"` and `"absolute"`.
    #' @param fun `function` for calculating targets.
    #' @param args `list` containing arguments.
    #' @return A new `Method` object.
    initialize = function(name, type, fun, args) {
      self$name <- name
      self$type <- type
      self$args <- args
      self$fun <- fun
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    #' @return Invisible `TRUE`.
    print = function(...) {
      cli::cli_text("Target setting method")
      cli::cli_text("  Name: ", self$name)
      cli::cli_text("  Parameters:")
      cli::cli_bullets(
        setNames(
          paste0(
            names(self$args), ": ",
            vapply(self$args, repr, character(1))
          ),
          rep("*", length(self$args))
        )
      )
      invisible(TRUE)
    },

    #' @description
    #' Calculate targets.
    #' @param x [problem()] object.
    #' @param features `integer` feature indices.
    #' @param call `NULL` or calling environment.
    #' @return A `numeric` vector with target values.
    calculate_targets = function(x, features, call = NULL) {
      do.call(
        self$fun,
        append(list(features = features, call = call), self$args)
      )
    }
  )
)

#' New method
#'
#' Create a new method.
#'
#' @param name `character` value with name of method.
#'
#' @param type `character` value denoting the type of the resulting
#' target values. Available options include `"relative"` or `"absolute"`.
#'
#' @param fun `function` for calculating targets.
#'
#' @param args `list` with additional arguments for calculating the targets.
#' These are passed to `function` during target calculations.
#'
#' @return A [`method-class`] object.
#'
#' @noRd
new_method <- function(name, type, fun, args) {
  # assert valid arguments
  assert(
    assertthat::is.string(name),
    assertthat::noNA(name),
    assertthat::is.string(type),
    assertthat::noNA(type),
    is.function(fun),
    is.list(args),
    all(names(args) %in% names(formals(fun)))
  )
  # return method
  Method$new(name = name, type = type, fun = fun, args = args)
}
