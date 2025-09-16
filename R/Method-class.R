#' @include internal.R
NULL

#' @export
if (!methods::isClass("Method")) methods::setOldClass("Method")
NULL

#' Target setting method class
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

    #' @field frame defused `call for generating error messages.
    frame = NULL,

    #' @description
    #' Initialize new object.
    #' @param name `character` value with name of method.
    #' @param type `character` value denoting the target type.
    #' Available options include `"relative"` and `"absolute"`.
    #' @param fun `function` for calculating targets.
    #' @param args `list` containing arguments.
    #' @param frame defused `call for generating error messages.
    #' @return A new `Method` object.
    initialize = function(name, type, fun, args, frame) {
      self$name <- name
      self$type <- type
      self$args <- args
      self$fun <- fun
      self$frame <- frame
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
      rlang::try_fetch(
        do.call(
          what = self$fun,
          quote = TRUE,
          args = append(
            list(x = x, features = features, call = self$frame),
            self$args
          )
        ),
        error = function(cnd) {
          cli::cli_abort(
            c("i" = "Can't calculate targets."),
            parent = cnd,
            call = call
          )
        }
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
#' @param call Caller environment.
#'
#' @return A [`method-class`] object.
#'
#' @noRd
new_method <- function(name, type, fun, args, call = fn_caller_env()) {
  # assert valid arguments
  assert(
    assertthat::is.string(name),
    assertthat::noNA(name),
    assertthat::is.string(type),
    assertthat::noNA(type),
    is.function(fun),
    is.list(args),
    identical(
      names(args),
      setdiff(names(formals(fun)), c("x", "features", "call"))
    ),
    call = call,
    .internal = TRUE
  )

  # return method
  Method$new(
    name = name,
    type = type,
    fun = fun,
    args = args,
    frame = rlang::frame_call(call)
  )
}
