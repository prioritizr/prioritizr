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
      # calculate targets
      ## note that if call is NULL then we evaluate the expression outside
      ## of the tryfetch so that the error is thrown directly
      if (!is.null(call)) {
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
      } else {
        do.call(
          what = self$fun,
          quote = TRUE,
          args = append(
            list(x = x, features = features, call = self$frame),
            self$args
          )
        )
      }
    },

    #' @description
    #' Calculate targets as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
    #' @param x [problem()] object.
    #' @param features `integer` feature indices.
    #' @param call `NULL` or calling environment.
    #' @return A `numeric` vector with target values expressed in
    #' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
    calculate_targets_km2 = function(x, features, call = NULL) {
      # return targets expressed as km2
      self$calculate_relative_targets(x = x, features = features, call = call) *
      c(x$feature_abundances_km2_in_total_units()[features, ])
    },

    #' @description
    #' Calculate targets as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
    #' @param x [problem()] object.
    #' @param features `integer` feature indices.
    #' @param call `NULL` or calling environment.
    #' @return A `numeric` vector with target values expressed as relative
    #' units.
    calculate_relative_targets = function(x, features, call = NULL) {
      # calculate targets
      out <- self$calculate_targets(x = x, features = features, call = call)
      # if units are absolute, convert to proportion of total units
      if (identical(self$type, "absolute")) {
         out <- out / c(x$feature_abundances_in_total_units()[features, ])
      }
      # return target values
      out
    },

    #' @description
    #' Calculate targets expressed as absolute units.
    #' @param x [problem()] object.
    #' @param features `integer` feature indices.
    #' @param call `NULL` or calling environment.
    #' @return A `numeric` vector with target values expressed as
    #' absolute units.
    calculate_absolute_targets = function(x, features, call = NULL) {
      # calculate targets
      out <- self$calculate_targets(x = x, features = features, call = call)
      # if values are relative, then convert to absolute values
      if (identical(self$type, "relative")) {
         out <- out * c(x$feature_abundances_in_total_units()[features, ])
      }
      # return target values
      out
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
