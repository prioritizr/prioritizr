#' @include internal.R
NULL

#' Representation
#'
#' Generate a brief `character` value that describes an object.
#'
#' @param x Object.
#'
#' @return A `character` value.
#'
#' @noRd
repr <- function(x) UseMethod("repr")

repr.numeric <- function(x) {
  cli::format_inline("{.val {x}}")
}

.S3method("repr", "numeric", repr.numeric)

repr.logical <- function(x) {
  cli::format_inline("{.val {x}}")
}

.S3method("repr", "logical", repr.logical)

repr.character <- function(x) {
  if (length(x) > 4) {
    out <- cli::format_inline(
      paste(
        "{.val {x[1]}}, {.val {x[2]}}, {.val {x[3]}}, {.val {x[4]}},",
        "{cli::symbol$ellipsis} ({length(x)} total)"
      )
    )
  } else {
    out <- cli::format_inline("{.val {x}}")
  }
  out
}

.S3method("repr", "character", repr.character)

repr.matrix <- function(x) {
  repr.dgCMatrix(as_Matrix(x, "dgCMatrix"))
}

.S3method("repr", "matrix", repr.matrix)

repr.Matrix <- function(x) {
  repr.dgCMatrix(as_Matrix(x, "dgCMatrix"))
}

.S3method("repr", "Matrix", repr.Matrix)

repr.dgCMatrix <- function(x) {
  # extract data
  v <- range(x@x[x@x != 0])
  # compute output
  if (Matrix::isDiagonal(x) && all_binary(x)) {
    out <- "diagonal matrix ({.val {c(0, 1)}} values)"
  } else if (Matrix::isDiagonal(x)) {
    out <- "diagonal matrix (non-zero values between {.val {v}})"
  } else if (all_binary(x)) {
    out <- paste(
      ifelse(Matrix::isSymmetric(x), "symmetric", "asymmetric"),
      "binary values ({.val {c(0, 1)}})"
    )
  } else {
    out <-
    paste(
      ifelse(Matrix::isSymmetric(x), "symmetric", "asymmetric"),
      "continuous values (non-zero values between {.val {v}})"
    )
  }
  # format output
  cli::format_inline(out)
}

.S3method("repr", "dgCMatrix", repr.dgCMatrix)

repr.list <- function(x) {
  cl <- unlist(lapply(x, class), recursive = TRUE, use.names = FALSE)
  cli::format_inline("{.cls list} containing {.cls {cl}} objects.")
}

.S3method("repr", "list", repr.list)

repr.NULL <- function(x) {
  cli::col_blue("NULL")
}

.S3method("repr", "NULL", repr.NULL)

repr.bbox <- function(x) {
  x <- c(x$xmin, x$ymin, x$xmax, x$ymax)
  x <- round(x, 5)
  cli::format_inline(
    paste(
      "{.val {x[[1]]}}, {.val {x[[2]]}}, {.val {x[[3]]}}, {.val {x[[4]]}}",
      "(xmin, ymin, xmax, ymax)"
    )
  )
}

.S3method("repr", "bbox", repr.bbox)

repr.phylo <- function(x) {
  vrng <- range(x$edge.length)
  cli::format_inline("phylogenetic tree (branch lengths between {.val {vrng}})")
}

.S3method("repr", "phylo", repr.phylo)

repr.crs <- function(x) {
  # sf does not export its nice sf:::format.crs method for pretty crs
  # so, we have to use its nice methods
  x <- suppressMessages(suppressWarnings(
    capture.output(print(sf::st_sfc(sf::st_point(c(1,1)), crs = x)))
  ))[[5]]

  # format output
  out <- trimws(strsplit(x, ":", fixed = TRUE)[[1]][[2]])

  # add information on if projected/geodetic
  if (grepl("geodetic", x, ignore.case = TRUE)) {
    out <- paste(out, "(geodetic)")
  } else if (grepl("geodetic", x, ignore.case = TRUE)) {
    out <- paste(out, "(projected)")
  } else {
    out <- paste(out, "(unknown)")
  }

  # return result
  out
}

.S3method("repr", "crs", repr.crs)

repr.ConservationModifier <- function(x) {
  x$repr()
}

.S3method("repr", "ConservationModifier", repr.ConservationModifier)

repr.ConservationProblem <- function(x) {
  x$repr()
}

.S3method("repr", "ConservationProblem", repr.ConservationProblem)

repr_data_list <- function(name, data, compact = TRUE) {
  # define names to suppress if compact  = TRUE
  compact_suppress_names <- c(
    ## constraints, penalties
    "data", "zones",
    ## solver
    "presolve", "threads", "numeric_focus", "node_file_start",
    "start_solution", "verbose",
    ## portfolio
    "remove_duplicates", "threads"
  )

  # find which data to display
  if (isTRUE(compact)) {
    repr_names <- names(data)[!names(data) %in% compact_suppress_names]
  } else {
    repr_names <- names(data)
  }

  # if no data to display, then just show name
  if (length(repr_names) == 0) {
    return(name)
  }

  # parse data values
  repr_values <- vapply(repr_names, FUN.VALUE = character(1), function(x) {
    paste0("{.arg ", x, "} = ", repr(data[[x]]))
  })

  # add ellipses if some data values excluded
  if (!identical(repr_names, names(data))) {
    repr_values <- c(repr_values, "{cli::symbol$ellipsis}")
  }

  # if compact, then convert to one-liner
  if (isTRUE(compact)) {
    out <- paste0(name, " (", paste(repr_values, collapse = ", "), ")")
  } else {
    out <- c(name, repr_values)
  }

  # return result
  out
}
