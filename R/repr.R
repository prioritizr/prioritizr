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

#' @export
repr.numeric <- function(x) {
  cli::format_inline("{.val {x}}")
}

#' @export
repr.logical <- function(x) {
  cli::format_inline("{.val {x}}")
}

#' @export
repr.character <- function(x) {
  # get console width
  w <- ceiling(cli::console_width() * 0.9)
  if (!is.null(getOption("width"))) {
    w <- min(w, getOption("width"))
  }
  # estimate total length of printing entire vector
  print_all_length <-
    ## count total characters for printing vector values
    ## note the +2 is for the quotes surrounding values
    sum(nchar(x) + 2) +
    ## add in the ", "
    ((length(x) - 1) * 2) +
    ## add in the " and (X total)"
    15 + nchar(length(x))
  if (print_all_length < w) {
    out <- cli::format_inline("{.val {x}} ({length(x)} total)")
  } else {
    # determine which characters to show
    nc <- nchar(x) + 2
    nc[-1] <- nc[-1] + 3
    nc <- nc[cumsum(nc) < w]
    nc <- nc[(cumsum(nc) + 10 + nchar(length(x))) < w]
    y <- x[seq_along(nc)]
    out <- cli::format_inline(
      paste(
        paste0("{.val ", y, "}", collapse = ", "),
        ", {cli::symbol$ellipsis} ({length(x)} total)"
      )
    )
  }
}

#' @export
repr.matrix <- function(x) {
  repr.dgCMatrix(as_Matrix(x, "dgCMatrix"))
}

#' @export
repr.Matrix <- function(x) {
  repr.dgCMatrix(as_Matrix(x, "dgCMatrix"))
}

#' @export
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

#' @export
repr.list <- function(x) {
  cl <- unlist(lapply(x, class), recursive = TRUE, use.names = FALSE)
  cli::format_inline("{.cls list} containing {.cls {cl}} objects.")
}

#' @export
repr.NULL <- function(x) {
  cli::col_blue("NULL")
}

#' @export
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

#' @export
repr.phylo <- function(x) {
  vrng <- range(x$edge.length)
  cli::format_inline("phylogenetic tree (branch lengths between {.val {vrng}})")
}

#' @export
repr.crs <- function(x) {
  if (identical(x$wkt, "")) {
    # if the CRS is an empty character value, then just return that
    out <- ""
  } else if (isTRUE(startsWith(x$wkt[[1]], "PROJCRS[\"unknown\""))) {
    # if the CRS is not recognized, then use the terra package to prepare output
    r <- terra::rast(matrix(1), crs = x$wkt)
    ptr_slot <- methods::slotNames(r)[[1]]
    out <- try(
      methods::slot(r, ptr_slot)$get_crs("proj4"),
      silent = TRUE
    )
    if (inherits(out, "try-error")) {
      out <- "Unrecognized Cartesian SRS"
    }
  } else {
    # if the CRS is recognized, then use the sf package to prepare output
    ## sf does not export its nice sf:::format.crs method for pretty crs
    ## so, we have to use its nice methods
    m <- suppressMessages(suppressWarnings(
      utils::capture.output(print(sf::st_sfc(sf::st_point(c(1,1)), crs = x)))
    ))[[5]]
    ## format output
    out <- trimws(strsplit(m, ":", fixed = TRUE)[[1]][[2]])
  }

  # add information on if projected/geodetic
  if (!is.null(x$ud_unit) && inherits(x$ud_unit, "units")) {
    if (identical(base::units(x$ud_unit)$numerator, "m")) {
      out <- paste(out, "(projected)")
    } else {
      out <- paste(out, "(geodetic)")
    }
  } else {
    out <- paste(out, "(unknown)")
  }

  # return result
  out
}

#' @export
repr.ConservationModifier <- function(x) {
  x$repr()
}

#' @export
repr.ConservationProblem <- function(x) {
  x$repr()
}

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

repr_cost <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(rng[1] == rng[2])) {
    out <- "constant values (all equal to {.val {rng[1]}})"
  } else {
    if (all_binary(x)) {
      out <- "binary values ({.val 0} and {.val 1})"
    } else {
      out <- "continuous values (between {.val {rng}})"
    }
  }
  cli::format_inline(out)
}
