#' @include internal.R
NULL

#' Read *Marxan* parameter file
#'
#' Read a *Marxan* parameter file.
#'
#' @param x `character` file name.
#'
#' @param data [data.frame] containing parameter metadata.
#' See Details section below for details.
#'
#' @inheritParams verify
#'
#' @details
#' The argument to `data` should be `data.frame` with the following
#' information. Each row corresponds to a different parameter, and
#' each column contains information about the parameter.
#'
#' \describe{
#'
#' \item{name}{`character` name of the parameter. This value will be
#'  used to refer to the parameter value in the output.}
#'
#' \item{field}{`character` field of the value. This value will be
#'  used to identify the parameter value in the parameter file.}
#'
#' \item{class}{`character` data type of the value. This value
#'  will be used to convert the parameter value to the correct data type.
#'  Available options include `"numeric"` or `"character"`.}
#'
#' \item{default}{`character` default value. This value will be assigned
#'  to the parameter if it is missing from the parameter file.}
#'
#' \item{type}{`character` parameter type. This value will be used to
#'   check the validity of the parameter value. Available options include
#'   `"file"`, `"directory"`, or `"number"`.}
#'
#' \item{mandatory}{`logical` value indicating if the parameter is
#'   mandatory or not. If a mandatory parameter is missing from the parameter
#'  file, then an error will be thrown.}
#'
#' }
#'
#' @return A `list` with the parameter values.
#'
#' @noRd
read_marxan_parameters <- function(x, data, call = fn_caller_env()) {
  # assert valid arguments
  assert(
    assertthat::is.readable(x),
    call = call
  )

  # read parameter files
  l <- readLines(x)

  # import parameters used in prioritization
  data$raw_value <- vapply(
    data$field, marxan_parse_field, character(1), lines = l
  )

  # convert parameters to default values if missing
  data$value <- lapply(
    seq_len(nrow(data)), function(i) {
      v <- data$raw_value[[i]]
      if (is.na(v[[1]])) {
        v <- data$default[[i]]
      }
      v
    }
  )

  # convert parameters to correct class
  data$value <- suppressWarnings(Map(
    methods::as,
    data$value,
    data$class
  ))

  # normalize the names for directories paths
  data$value <- lapply(
    seq_len(nrow(data)), function(i) {
      ## extract values
      v <- data$value[[i]]
      type <- data$type[[i]]
      ## if needed, then sanitize paths
      if (!is.na(v) && identical(type, "directory")) {
        v <- gsub("\\", "/", v, fixed = TRUE)
      }
      ## return result
      v
    }
  )

  # extract input_dir
  input_idx <- which(data$name == "input_dir")
  input_dir <- data$value[[input_idx]]
  raw_input_dir <- data$raw_value[[input_idx]]

  # if input_dir is a relative file path, then normalize it to the
  # directory where it is located
  if (!is.na(input_dir)) {
    if (!any(startsWith(input_dir, c(paste0(LETTERS, ":"), "/")))) {
      x_dir <- dirname(normalizePath(x, winslash = "/", mustWork = FALSE))
      input_dir <- paste0(x_dir, "/", input_dir)
      data$value[[input_idx]] <- input_dir
    }
  }

  # normalize the names for file paths
  data$value <- lapply(
    seq_len(nrow(data)), function(i) {
      ## extract values
      v <- data$value[[i]]
      type <- data$type[[i]]
      ## if needed, then sanitize paths
      if (!is.na(v) && identical(type, "file")) {
        v <- gsub("\\", "/", v, fixed = TRUE)
        if (!any(startsWith(v, c(paste0(LETTERS, ":"), "/")))) {
          v <- paste0(input_dir, "/", v)
        }
      }
      ## return result
      v
    }
  )

  # check for missing parameters
  invisible(lapply(seq_len(nrow(data)), function(i) {
    ## check parameter not missing from file
    if (
      isTRUE(data$mandatory[[i]]) &&
      is.na(data$raw_value[[i]])
    ) {
      cli::cli_abort(
        c(
          "!" = paste0(
            "{.arg x} is missing the {.field ",
            data$field[[i]],
            "} field."
          )
        ),
        call = call
      )
    }
    ## check parameter had no issues being converted to correct class
    if (
      !is.na(data$raw_value[[i]]) &&
      is.na(data$value[[i]])
    ) {
      cli::cli_abort(
        c(
          "!" = paste0(
            "{.arg x} has an invalid value specified for {.field ",
            data$field[[i]],
            "}."
          ),
          "i" = paste0(
            "{.field ",
            data$field[[i]],
            "} must be a ", data$type[[i]], "."
          ),
          "x" = paste0(
            "{.field ",
            data$field[[i]],
            "} has the value {.val ", data$raw_value[[i]], "}."
          )
        ),
        call = call
      )
    }
    ## if is a file path parameter, then verify file exists
    if (
      (isTRUE(data$mandatory[[i]]) || !is.na(data$raw_value[[i]])) &&
      identical(data$type[[i]], "file") &&
      !file.exists(data$value[[i]])
    ) {
      cli::cli_abort(
        c(
          "!" = paste0(
            "{.field ",
            data$field[[i]],
            "} in {.arg x} refers to a path that does not exist."
          ),
          "i" = paste0(
            "{.arg x} is in the directory {.path ", x_dir, "}."
          ),
          "i" = ifelse(is.na(raw_input_dir),
            paste0(
              "{.field INPUTDIR} is missing."
            ),
            paste0(
              "{.field INPUTDIR} refers to {.val ", raw_input_dir, "}."
            )
          ),
          "i" = paste0(
            "{.field ", data$field[[i]], "} refers to {.val ",
            data$raw_value[[i]], "}."
          ),
          "x" = paste0(
            "Path {.file ", data$value[[i]], "} does not exist."
          )
        ),
        call = call
      )
    }
    ## if is INPUTDIR parameter, then verify directory exists
    if (
      identical(data$name[[i]], "input_dir") &&
      !is.na(data$raw_value[[i]]) &&
      !assertthat::is.dir(data$value[[i]])
    ) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.field ", data$field[[i]],
            "} in {.arg x} refers to a directory that does not exist."
          ),
          "i" = paste0(
            "{.arg x} is in the directory {.path ", x_dir, "}."
          ),
          "i" = paste0(
            "{.field ", data$field[[i]], "} refers to {.val ",
            data$raw_value[[i]], "}."
          ),
          "x" = paste0(
            "Directory {.path ", data$value[[i]], "} does not exist."
          )
        ),
        call = call
      )
    }
  }))

  # return result
  out <- data$value
  names(out) <- data$name
  out
}

#' Parse *Marxan* field
#'
#' Parse a parameter value in a *Marxan* parameter file.
#'
#' @param field `character` file name
#'
#' @param lines `character` vector with *Marxan* parameters
#'
#' @return A `character` value with the value
#'
#' @noRd
marxan_parse_field <- function(field, lines) {
  x <- grep(paste0(field, " "), lines, value = TRUE, fixed = TRUE)
  if (length(x) == 0) return(NA_character_)
  x <- x[startsWith(x, field)]
  if (length(x) == 0) return(NA_character_) # nocov
  x <- sub(paste0(field, " "), "", x)
  x[[1]]
}
