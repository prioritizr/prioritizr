#' @include internal.R read_marxan_parameters.R read_marxan_data.R
NULL

#' *Marxan* conservation problem
#'
#' Create a conservation planning [problem()] following the
#' mathematical formulations used in *Marxan* (detailed in Beyer
#' *et al.* 2016). Note that these problems are solved using
#' exact algorithms and not simulated annealing (i.e., the *Marxan* software).
#' Please note that the \pkg{vroom} package is required to import *Marxan* data
#' files.
#'
#' @param x `character` file path for a *Marxan* input file (typically
#'   called `"input.dat"`), or `data.frame` containing planning unit
#'   data (typically called `"pu.dat"`). If the argument to `x` is a
#'   `data.frame`, then each row corresponds to a different planning unit,
#'   and it must have the following columns:
#'
#'   \describe{
#'
#'   \item{id}{`integer` unique identifier for each planning unit.
#'     These identifiers are used in the argument to `puvspr`.}
#'
#'   \item{cost}{`numeric` cost of each planning unit.}
#'
#'   \item{status}{`integer` indicating if each planning unit
#'     should not be locked in the solution (0) or if it should be locked in
#'     (2) or locked out (3) of the solution. Although *Marxan* allows
#'     planning units to be selected in the initial solution (using values of
#'     1), these values have no effect here. This column is optional.}
#'
#'   }
#'
#' @param spec `data.frame` containing information on the features.
#'   The argument to `spec` must follow the conventions used by
#'   *Marxan* for the species data file (conventionally called
#'   `"spec.dat"`). Each row corresponds to a different feature and
#'   each column corresponds to different information about the features. It
#'   must contain the columns listed below. Note that the argument to
#'   `spec` must contain at least one column named `"prop"` or
#'   `"amount"`---**but not both columns with both of these
#'   names**---to specify the target for each feature.
#'
#'   \describe{
#'
#'   \item{id}{`integer` unique identifier for each feature
#'     These identifiers are used in the argument to `puvspr`.}
#'
#'   \item{name}{`character` name for each feature.}
#'
#'   \item{prop}{`numeric` relative target for each feature
#'     (optional).}'
#'
#'   \item{amount}{`numeric` absolute target for each
#'     feature (optional).}
#'
#'   }
#'
#' @param puvspr `data.frame` containing information on the amount of
#'   each feature in each planning unit. The argument to
#'   `puvspr` must follow the conventions used in the *Marxan* input
#'   data file (conventionally called `"puvspr.dat"`). It must contain the
#'   following columns:
#'
#'   \describe{
#'
#'   \item{pu}{`integer` planning unit identifier.}
#'
#'   \item{species}{`integer` feature identifier.}
#'
#'   \item{amount}{`numeric` amount of the feature in the
#'        planning unit.}
#'
#'   }
#'
#' @param bound `NULL` object indicating that no boundary data
#'   is required for the conservation planning problem, or a `data.frame`
#'   containing information on the planning units' boundaries. The argument to
#'   `bound` must follow the conventions used in the *Marxan* input
#'   data file (conventionally called `"bound.dat"`). It must contain the
#'   following columns:
#'
#'   \describe{
#'
#'   \item{id1}{`integer` planning unit identifier.}
#'
#'   \item{id2}{`integer` planning unit identifier.}
#'
#'   \item{boundary}{`numeric` length of shared boundary
#'     between the planning units identified in the previous two columns.}
#'
#'   }
#'
#' @param blm `numeric` boundary length modifier. This argument only
#'   has an effect when argument to `x` is a `data.frame`. The
#'   default argument is zero.
#'
#' @param symmetric `logical` does the boundary data (i.e., argument to
#'   `bound`) describe symmetric relationships between planning units?
#'    If the boundary data contain asymmetric connectivity data,
#'    this parameter should be set to `FALSE`.
#'    Defaults to `TRUE`.
#'
#' @param ... not used.
#'
#' @details
#' This function is provided as a convenient interface for solving
#' *Marxan* problems using the \pkg{prioritizr} package. Note that this
#' function does not support all of the functionality provided by the
#' *Marxan* software. In particular, only the following parameters
#' supported: `"INPUTDIR"`, `"SPECNAME`", `"PUNAME"`, `"PUVSPRNAME",
#' `"BOUNDNAME"`, `"BLM"`, and `"ASYMMETRICCONNECTIVITY"`.
#' Additionally, for the species data (i.e., argument to `spec`),
#' only the `"id"`, `"name"`, `"prop"`, and `"amount"` columns are considered.
#'
#' @section Notes:
#' In previous versions, this function could not accommodate asymmetric
#' connectivity data. It has now been updated to handle asymmetric connectivity
#' data.
#'
#' @seealso For more information on the correct format for
#'   for *Marxan* input data, see the
#'   [official *Marxan* website](https://marxansolutions.org), Ball
#'   *et al.* (2009), Serra *et al.* (2020).
#'
#' @return A [problem()] object.
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' *Ecological Modelling*, 228: 14--22.
#'
#' Serra N, Kockel A, Game ET, Grantham H, Possingham HP, and McGowan J (2020)
#' Marxan User Manual: For Marxan version 2.43 and above. The Nature Conservancy
#' (TNC), Arlington, Virginia, United States and Pacific Marine Analysis and
#' Research Association (PacMARA), Victoria, British Columbia, Canada.
#'
#' @examples
#' # create Marxan problem using Marxan input file
#' # (note this example requires the vroom package to be installed)
#' \dontrun{
#' input_file <- system.file("extdata/marxan/input.dat", package = "prioritizr")
#' p1 <-
#'   marxan_problem(input_file) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' head(s1)
#'
#' # create Marxan problem using data.frames that have been loaded into R
#' # (note this example also requires the vroom package to be installed)
#' ## load in planning unit data
#' pu_path <- system.file("extdata/marxan/input/pu.dat", package = "prioritizr")
#' pu_dat <- vroom::vroom(pu_path)
#' head(pu_dat)
#'
#' ## load in feature data
#' spec_path <- system.file(
#'   "extdata/marxan/input/spec.dat", package = "prioritizr"
#' )
#' spec_dat <- vroom::vroom(spec_path)
#' head(spec_dat)
#'
#' ## load in planning unit vs feature data
#' puvspr_path <- system.file(
#'   "extdata/marxan/input/puvspr.dat", package = "prioritizr"
#' )
#' puvspr_dat <- vroom::vroom(puvspr_path)
#' head(puvspr_dat)
#'
#' ## load in the boundary data
#' bound_path <- system.file(
#'   "extdata/marxan/input/bound.dat", package = "prioritizr"
#' )
#' bound_dat <- vroom::vroom(bound_path)
#' head(bound_dat)
#'
#' # create problem without the boundary data
#' p2 <-
#'   marxan_problem(pu_dat, spec_dat, puvspr_dat) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print solution
#' head(s2)
#'
#' # create problem with the boundary data and a boundary length modifier
#' # set to 5
#' p3 <-
#'   marxan_problem(pu_dat, spec_dat, puvspr_dat, bound_dat, blm = 5) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # print solution
#' head(s3)
#' }
#' @export
marxan_problem <- function(x, ...) {
  assert_required(x)
  UseMethod("marxan_problem")
}

#' @rdname marxan_problem
#' @method marxan_problem default
#' @export
marxan_problem.default <- function(x, ...) {
  cli::cli_abort("{.arg x} must be character file path or a data frame.")
}

#' @rdname marxan_problem
#' @method marxan_problem data.frame
#' @export
marxan_problem.data.frame <- function(x, spec, puvspr, bound = NULL,
                                      blm = 0, symmetric = TRUE, ...) {
  # assert arguments are valid
  assert_required(x)
  assert_required(spec)
  assert_required(puvspr)
  assert_required(bound)
  assert_required(blm)
  assert_required(symmetric)
  assert_dots_empty()
  assert(
    ## data frames
    is.data.frame(x),
    is.data.frame(spec),
    is.data.frame(puvspr),
    ## blm
    assertthat::is.number(blm),
    assertthat::noNA(blm),
    ## symmetric
    assertthat::is.flag(symmetric),
    assertthat::noNA(symmetric)
  )

  # validate data
  validate_marxan_pu_data(x)
  validate_marxan_spec_data(spec)
  validate_marxan_puvspr_data(puvspr)
  validate_marxan_puvspr_pu_data(puvspr, x)
  validate_marxan_puvspr_spec_data(puvspr, spec)
  if (!is.null(bound)) {
    assert(is.data.frame(bound))
    validate_marxan_bound_data(bound)
    validate_marxan_bound_pu_data(bound, x)
  }

  # if needed, display warnings
  if (abs(blm) > 1e-15 && is.null(bound)) {
    cli_warning(
      "{.arg bound} is missing, so {.arg blm} has no effect on formulation."
    )
  }
  ## symmetric
  if (!isTRUE(symmetric) && is.null(bound)) {
    cli_warning(
      paste(
        "{.arg bound} is missing, so {.arg symmetric} has no effect on",
        "formulation."
      )
    )
  }

  # return problem
  internal_marxan_problem(
    x = x, spec = spec, puvspr = puvspr, bound = bound,
    blm = blm, symmetric = symmetric
  )
}

#' @rdname marxan_problem
#' @method marxan_problem character
#' @export
marxan_problem.character <- function(x, ...) {
  # assert that arguments are valid
  assert(is_installed("vroom"))

  # define parameter metadata
  param_metadata <- tibble::tribble(
    ~name, ~field, ~class, ~default, ~type, ~mandatory,
    "input_dir", "INPUTDIR", "character", getwd(), "directory", FALSE,
    "pu_name", "PUNAME", "character", NA, "file", TRUE,
    "spec_name", "SPECNAME", "character", NA, "file", TRUE,
    "puvspr_name", "PUVSPRNAME", "character", NA, "file", TRUE,
    "bound_name", "BOUNDNAME", "character", NA, "file", FALSE,
    "blm", "BLM", "double", "0", "number", FALSE,
    "asym_conn", "ASYMMETRICCONNECTIVITY", "double", "0", "number", FALSE
  )

  # import parameters
  param_data <- read_marxan_parameters(x, param_metadata)

  # import data
  error_prefix_handler(
    pu <- read_marxan_pu_data(param_data$pu_name),
    prefix = c(
      "!" = paste(
        "{.field PUNAME} field in {.arg x} must refer",
        "to a valid CSV or TSV file."
      ),
      "i" = paste0(
        "Attempting to read file at {.field PUNAME}",
        cli::symbol$ellipsis
      )
    )
  )
  error_prefix_handler(
    spec <- read_marxan_spec_data(param_data$spec_name),
    prefix = c(
      "!" = paste(
        "{.field SPECNAME} field in {.arg x} must refer",
        "to a valid CSV or TSV file."
      ),
      "i" = paste0(
        "Attempting to read {.field SPECNAME}",
        cli::symbol$ellipsis
      )
    )
  )
  error_prefix_handler(
    puvspr <- read_marxan_puvspr_data(param_data$puvspr_name),
    prefix = c(
      "!" = paste(
        "{.field PUVSPRNAME} field in {.arg x} must refer",
        "to a valid CSV or TSV file."
      ),
      "i" = paste0(
        "Attempting to read {.field PUVSPRNAME}",
        cli::symbol$ellipsis
      )
    )
  )
  if (!is.na(param_data$bound_name[[1]])) {
    error_prefix_handler(
      bound <- read_marxan_bound_data(param_data$bound_name),
    prefix = c(
      "!" = paste(
        "{.field BOUNDNAME} field in {.arg x} must refer",
        "to a valid CSV or TSV file."
      ),
      "i" = paste0(
        "Attemping to read {.field BOUNDNAME}",
        cli::symbol$ellipsis
      )
    )
  )
  } else {
    bound <- NULL
  }

  # validate data
  error_prefix_handler(
    validate_marxan_pu_data(pu),
    prefix = c(
      "!" = "{.field PUNAME} field in {.arg x} must refer to valid data.",
      "i" = paste0(
        "Importing {.field PUNAME} as {.arg pu}",
        cli::symbol$ellipsis
      )
    )
  )
  error_prefix_handler(
    validate_marxan_spec_data(spec),
    prefix = c(
      "!" = "{.field SPECNAME} field in {.arg x} must refer to valid data.",
      "i" = paste0(
        "Importing {.field SPECNAME} as {.arg spec}",
        cli::symbol$ellipsis
      )
    )
  )
  error_prefix_handler(
    validate_marxan_puvspr_data(puvspr),
    prefix = c(
      "!" = "{.field PUVSPRNAME} field in {.arg x} must refer to valid data.",
      "i" = paste0(
        "Importing {.field PUVSPR} as {.arg puvspr}",
        cli::symbol$ellipsis
      )
    )
  )
  error_prefix_handler(
    validate_marxan_puvspr_pu_data(puvspr, pu),
    prefix = c(
      "!" = "{.field PUVSPRNAME} field in {.arg x} must refer to valid data.",
      "i" = paste0(
        "Importing {.field PUVSPRNAME} as {.arg puvspr}",
        cli::symbol$ellipsis
      ),
      "i" = paste0(
        "Importing {.field PUNAME} as {.arg pu}",
        cli::symbol$ellipsis
      )
    )
  )
  error_prefix_handler(
    validate_marxan_puvspr_spec_data(puvspr, spec),
    prefix = c(
      "!" = "{.field PUVSPRNAME} field in {.arg x} must refer to valid data.",
      "i" = paste0(
        "Importing {.field PUVSPRNAME} as {.arg puvspr}",
        cli::symbol$ellipsis
      ),
      "i" = paste0(
        "Importing {.field SPECNAME} as {.arg spec}",
        cli::symbol$ellipsis
      )
    )
  )
  if (!is.null(bound)) {
    error_prefix_handler(
      validate_marxan_bound_data(bound),
      prefix = c(
        "!" = "{.field BOUNDNAME} field in {.arg x} must refer to valid data.",
        "i" = paste0(
          "Importing {.field BOUNDNAME} as {.arg bound}",
          cli::symbol$ellipsis
        )
      )
    )
    error_prefix_handler(
      validate_marxan_bound_pu_data(bound, pu),
      prefix = c(
        "!" = "{.field BOUNDNAME} field in {.arg x} must refer to valid data.",
        "i" = paste0(
          "Importing {.field BOUNDNAME} as {.arg bound}",
          cli::symbol$ellipsis
        ),
        "i" = paste0(
          "Importing {.field PUNAME} as {.arg pu}",
          cli::symbol$ellipsis
        )
      )
    )
  }

  # if needed, display warnings
  ## blm
  if (abs(param_data$blm) > 1e-15 && is.null(bound)) {
    cli_warning(
      paste(
        "{.arg x} is missing {.field BOUNDNAME},",
        "so {.field BLM} has no effect on formulation."
      )
    )
  }
  ## asym_conn
  if ((param_data$asym_conn != 0) && is.null(bound)) {
    cli_warning(
      paste(
        "{.arg x} is missing {.field BOUNDNAME},",
        "so the {.field ASYMMETRICCONNECTIVITY} has no effect on formulation."
      )
    )
  }

  # return problem
  internal_marxan_problem(
    x = pu,
    spec = spec,
    puvspr = puvspr,
    bound = bound,
    blm = param_data$blm,
    symmetric = isTRUE(param_data$asym_conn == 0)
  )
}

internal_marxan_problem <- function(x, spec, puvspr, bound, blm, symmetric) {
  # prepare data
  ## create locked in columns
  if (assertthat::has_name(x, "status")) {
    x$locked_in <- x$status == 2
    x$locked_out <- x$status == 3
  } else {
    x$locked_in <- FALSE
    x$locked_out <- FALSE
  }
  ## if required, add in feature names
  if (!assertthat::has_name(spec, "name")) {
    spec$name <- paste0("feature.", seq_len(nrow(spec)))
  }

  # create problem
  ## initialize problem
  p <- problem(x = x, features = spec, rij = puvspr, cost_column = "cost")
  ## add objective
  p <- add_min_set_objective(p)
  ## add locked in constraints
  if (any(x$locked_in))
    p <- add_locked_in_constraints(p, "locked_in")
  # add locked in constraints
  if (any(x$locked_out))
    p <- add_locked_out_constraints(p, "locked_out")
  ## add targets
  if ("prop" %in% names(spec)) {
    p <- add_relative_targets(p, "prop")
  } else {
    p <- add_absolute_targets(p, "amount")
  }
  ## if boundary data specified, then add boundary penalties
  if (!is.null(bound) && isTRUE(symmetric)) {
    p <- add_boundary_penalties(p, penalty = blm, edge_factor = 1, data = bound)
  } else if (!is.null(bound) && !isTRUE(symmetric)) {
    p <- add_asym_connectivity_penalties(p, penalty = blm, data = bound)
  }

  # return result
  p
}
