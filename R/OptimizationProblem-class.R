#' @include internal.R cli.R
NULL

#' @export
if (!methods::isClass("OptimizationProblem")) methods::setOldClass("OptimizationProblem")
NULL

#' Optimization problem class
#'
#' @description
#' This class is used to represent an optimization problem.
#' It stores the information needed to generate a solution using
#' an exact algorithm solver.
#' Most users should use [compile()] to generate new optimization problem
#' objects, and the functions distributed with the package to interact
#' with them (e.g., [base::as.list()]).
#' **Only experts should use the fields and methods for this class directly.**
#'
#' @name OptimizationProblem-class
#'
#' @family classes
#'
#' @export
OptimizationProblem <- R6::R6Class(
  "OptimizationProblem",
  public = list(

    #' @field ptr A `Rcpp::Xptr` external pointer.
    ptr = NULL,

    #' Create a new optimization problem object.
    #' @param ptr `Rcpp::Xptr` external pointer.
    #' @return A new `OptimizationProblem` object.
    initialize = function(ptr) {
      self$ptr <- ptr
    },

    #' @description
    #' Print concise information about the object.
    #' @return Invisible `TRUE`.
    print = function() {
      # compute statistics to display
      if (self$ncol() > 0) {
        cv <- table(self$vtype())
        cv <- paste(
          paste(unname(cv), paste0("(", names(cv), ")")),
          collapse = ", "
        )
        ms <- self$modelsense()
        dims <- paste0(self$nrow(), ", ", self$ncol(), ", ", self$ncell())
      } else {
        cv <- "none"
        ms <- "missing"
        dims <- "0, 0, 0"
      }
      # print text
      div_id <- cli::cli_div(theme = cli_pkg_theme())
      ch <- cli_box_chars()
      cli::cli_text("An optimization problem ({.cls OptimizationProblem})")
      cli_vtext("{ch$b} model sense: ", ms)
      cli_vtext("{ch$b} dimensions:  ", dims, " (rows, columns, cells)")
      cli_vtext("{ch$b} variables:   ", cv)
      cli::cli_end(div_id)
      # return success
      invisible(TRUE)
    },

    #' @description
    #' Print concise information about the object.
    #' @return Invisible `TRUE`.
    show = function() {
      self$print()
    },

    #' @description
    #' Obtain the number of columns in the problem formulation.
    #' @return A `numeric` value.
    ncol = function() {
      rcpp_get_optimization_problem_ncol(self$ptr)

    },

    #' @description
    #' Obtain the number of rows in the problem formulation.
    #' @return A `numeric` value.
    nrow = function() {
      rcpp_get_optimization_problem_nrow(self$ptr)
    },

    #' @description
    #' Obtain the number of cells in the problem formulation.
    #' @return A `numeric` value.
    ncell = function() {
      rcpp_get_optimization_problem_ncell(self$ptr)
    },

    #' @description
    #' Obtain the model sense.
    #' @return A `character` value.
    modelsense = function() {
      rcpp_get_optimization_problem_modelsense(self$ptr)
    },

    #' @description
    #' Obtain the decision variable types.
    #' @return A `character` vector.
    vtype = function() {
      rcpp_get_optimization_problem_vtype(self$ptr)
    },

    #' @description
    #' Obtain the objective function.
    #' @return A `numeric` vector.
    obj = function() {
      rcpp_get_optimization_problem_obj(self$ptr)
    },

    #' @description
    #' Obtain the constraint matrix.
    #' @return A [Matrix::sparseMatrix()] object.
    A = function() {
      x <- rcpp_get_optimization_problem_A(self$ptr)
      Matrix::sparseMatrix(
        i = x$i, j = x$j, x = x$x, index1 = FALSE,
        dims = c(self$nrow(), self$ncol())
      )
    },

    #' @description
    #' Obtain the right-hand-side constraint values.
    #' @return A `numeric` vector.
    rhs = function() {
      rcpp_get_optimization_problem_rhs(self$ptr)
    },

    #' @description
    #' Obtain the constraint senses.
    #' @return A `character` vector.
    sense = function() {
      rcpp_get_optimization_problem_sense(self$ptr)
    },

    #' @description
    #' Obtain the lower bounds for the decision variables.
    #' @return A `numeric` vector.
    lb = function() {
      rcpp_get_optimization_problem_lb(self$ptr)
    },

    #' @description
    #' Obtain the upper bounds for the decision variables.
    #' @return A `numeric` vector.
    ub = function() {
      rcpp_get_optimization_problem_ub(self$ptr)
    },

    #' @description
    #' Obtain the number of features.
    #' @return A `numeric` value.
    number_of_features = function() {
      rcpp_get_optimization_problem_number_of_features(self$ptr)
    },

    #' @description
    #' Obtain the number of planning units.
    #' @return A `numeric` value.
    number_of_planning_units = function() {
      rcpp_get_optimization_problem_number_of_planning_units(self$ptr)
    },

    #' @description
    #' Obtain the number of zones.
    #' @return A `numeric` value.
    number_of_zones = function() {
      rcpp_get_optimization_problem_number_of_zones(self$ptr)
    },

    #' @description
    #' Obtain the identifiers for the columns.
    #' @return A `character` value.
    col_ids = function() {
      rcpp_get_optimization_problem_col_ids(self$ptr)
    },

    #' @description
    #' Obtain the identifiers for the rows.
    #' @return A `character` value.
    row_ids = function() {
      rcpp_get_optimization_problem_row_ids(self$ptr)
    },

    #' @description
    #' Is the problem formulation compressed?
    #' @return A `logical` value.
    compressed_formulation = function() {
      rcpp_get_optimization_problem_compressed_formulation(self$ptr)
    },

    #' @description
    #' Shuffle the order of the columns in the optimization problem.
    #' @param order `integer` vector with new order.
    #' @return An `integer` vector with indices to un-shuffle the problem.
    shuffle_columns = function(order) {
      rcpp_set_optimization_problem_shuffled(self$ptr, order)
    },

    #' @description
    #' Create a copy of the optimization problem.
    #' @return A new `OptimizationProblem` object .
    copy = function() {
      OptimizationProblem$new(ptr = rcpp_copy_optimization_problem(self$ptr))
    }
  )
)
