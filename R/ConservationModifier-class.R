#' @include internal.R waiver.R assertions.R
NULL

#' @export
if (!methods::isClass("ConservationModifier")) methods::setOldClass("ConservationModifier")
NULL

#' Conservation problem modifier class
#'
#' @description
#' This super-class is used to construct [`Objective-class`]
#' [`Penalty-class`], [`Target-class`], [`Constraint-class`],
#' [`Portfolio-class`], [`Solver-class`], and [`Decision-class`] objects.
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name ConservationModifier-class
#'
#' @family classes
ConservationModifier <- R6::R6Class(
  "ConservationModifier",
  public = list(

    #' @field name `character` value.
    name = character(0),

    #' @field data `list` containing data.
    data = list(),

    #' @field internal `list` containing internal computed values.
    internal = list(),

    #' @field compressed_formulation `logical` value indicating if the
    #' object is compatible with a compressed formulation.
    compressed_formulation = TRUE,

    #' @description
    #' Print information about the object.
    #' @return None.
    print = function() {
      cli::cli_text(self$repr())
    },

    #' @description
    #' Print information about the object.
    #' @return None.
    show = function() {
      self$print()
    },

    #' @description
    #' Generate a character representation of the object.
    #' @param compact `logical` value indicating if the output value
    #' should be compact? Defaults to `FALSE`.
    #' @return A `character` value.
    repr = function(compact = TRUE) {
      repr_data_list(self$name, self$data, compact = compact)
    },

    #' @description
    #' Perform computations that need to be completed before applying
    #' the object.
    #' @param x [optimization_problem()] object.
    #' @param y [problem()] object.
    #' @return Invisible `TRUE`.
    calculate = function(x, y) {
      invisible(TRUE)
    },

    #' @description
    #' Get values stored in the `data` field.
    #' @param x `character` name of data.
    #' @return An object. If the `data` field does not contain an object
    #' associated with the argument to `x`, then a [new_waiver()] object is
    #' returned.
    get_data = function(x) {
      if (!x %in% names(self$data)) return(new_waiver())
      self$data[[x]]
    },

    #' @description
    #' Set values stored in the `data` field. Note that this method will
    #' overwrite existing data.
    #' @param x `character` name of data.
    #' @param value Object to store.
    #' @return Invisible `TRUE`.
    set_data = function(x, value) {
      self$data[[x]] <- value
      invisible()
    },

    #' @description
    #' Get values stored in the `internal` field.
    #' @param x `character` name of data.
    #' @return An object. If the `internal` field does not contain an object
    #' associated with the argument to `x`, then a [new_waiver()] object is
    #' returned.
    get_internal = function(x) {
      if (!x %in% names(self$internal)) return(new_waiver())
      self$internal[[x]]
    },

    #' @description
    #' Set values stored in the `internal` field. Note that this method will
    #' overwrite existing data.
    #' @param x `character` name of data.
    #' @param value Object to store.
    #' @return An object. If the `internal` field does not contain an object
    #' associated with the argument to `x`, then a [new_waiver()] object is
    #' returned.
    set_internal = function(x, value) {
      self$internal[[x]] <- value
      invisible()
    }
  )
)
