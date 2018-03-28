#' Check
#'
#' Check that the output from \code{\link[assert_that]{see_if}}
#' is valid.
#'
#' @param x \code{character} or \code{object}
#'
#' @details This object will return an error if the argument to \code{x}
#'   is \code{FALSE}, and for the error message, will show the error
#'   message attached to the object.
#'
#' @return invisible \code{TRUE}.
#'
#' @noRd
check_that <- function(x) {
  if (!isTRUE(x))
    stop(attr(x, "msg")[1])
  invisible(TRUE)
}

#' Convert sparse matrix to triplet data.frame
#'
#' Convert a sparse matrix to a triplet \code{data.frame}.
#'
#' @param x \code{Matrix} object.
#'
#' @noRd
matrix_to_triplet_dataframe <- function(x) {
  if (inherits(x, c("dsCMatrix")))
    x <- methods::as(x, "dsTMatrix")
  if (inherits(x, c("dgCMatrix", "matrix")))
    x <- methods::as(x, "dgTMatrix")
  data.frame(i = x@i + 1, j = x@j + 1, x = x@x)
}

#' Convert a triplet data.frame to a matrix
#'
#' Convert a triplet data.framr object to a sparse matrix.
#'
#' @param x \code{data.frame} object. The first column contains the row
#'   numbers, the second column contains the column numbers, and the
#'   third column contains the cell values.
#
#' @return \code{\link[Matrix]{dgCMatrix-class}} object.
#'
#' @noRd
triplet_dataframe_to_matrix <- function(x, forceSymmetric = FALSE, ...) {
  assertthat::assert_that(inherits(x, "data.frame"), isTRUE(ncol(x) == 3),
    isTRUE(all(x[[1]] == round(x[[1]]))), isTRUE(all(x[[2]] == round(x[[2]]))))
  # create sparse amtrix
  m <- Matrix::sparseMatrix(i = x[[1]], j = x[[2]], x = x[[3]],
                            giveCsparse = FALSE, ...)
  if (forceSymmetric) {
    # force the matrix to be symmetric
    # we cannot gurantee that the cells that are filled in belong to either
    # the upper or the lower diagonal
    m2 <- matrix(c(m@j + 1, m@i + 1, m@x), ncol = 3)
    m2 <- m2[m2[, 1] != m2[, 2], ]
    m[m2[, 1:2]] <- m2[, 3]
    return(Matrix::forceSymmetric(m))
  } else {
    # return matrix in compressed format
    return(methods::as(m, "dgCMatrix"))
  }
}

#' Parallel extract
#'
#' Extract data from a \code{\link[raster]{Raster-class}} object using
#' a \code{\link[sp]{Spatial-class}} object using parallel processing.
#'
#' @param x \code{\link[raster]{Raster-class}} object.
#'
#' @param y \code{\link[sp]{Spatial-class}} object.
#'
#' @param fun \code{function} to compute values.
#'
#' @param ... additional arguments passed to \code{\link[raster]{extract}}.
#'
#' @details This function is essentially a wrapper for
#'   \code{\link[raster]{extract}}. To enable parallel processing,
#'   use the \code{\link{set_number_of_threads}} function.
#'
#' @return \code{data.frame}, \code{matrix}, or \code{list} object
#'   depending on the arguments.
#'
#' @noRd
parallelized_extract <- function(x, y, fun=mean, ...) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "Raster"), inherits(y, "Spatial"),
      inherits(fun, "function"), raster::compareCRS(x@crs, y@proj4string),
      rgeos::gIntersects(methods::as(raster::extent(x[[1]]), "SpatialPolygons"),
        methods::as(raster::extent(y), "SpatialPolygons")), is.parallel())
  # data processing
  args <- list(...)
  parallel::clusterExport(.pkgenv$cluster, c("x", "y", "fun", "args"),
                          envir = environment())
  m <- plyr::llply(distribute_load(length(y)), .parallel = TRUE,
    function(i) {
      return(do.call(raster::extract,
        append(list(x = x, y = y[i, ], fun = fun), args)))
    })
  parallel::clusterEvalQ(.pkgenv$cluster, {
      rm("x", "y", "fun", "args")
  })
  # combine parallel runs
  if (inherits(m[[1]], c("matrix", "data.frame"))) {
    m <- do.call(rbind, m)
  } else {
    m <- do.call(append, m)
  }
  # return result
  return(m)
}

#' Velox extract
#'
#' This function is a wrapper for \code{\link{velox}{VeloxRaster-extract}}.
#'
#' @param x \code{\link[raster]{Raster-class}} object.
#'
#' @param y \code{\link[sp]{Spatial-class}} object.
#'
#' @param fun \code{function} to compute values.
#'
#' @param df \code{logical} should results be returned as a \code{data.frame}?
#'
#' @param ... not used.
#'
#' @return \code{matrix} or \code{data.frame} depending on arguments.
#'
#' @noRd
velox_extract <- function(x, y, fun, df = FALSE, ...) {
  assertthat::assert_that(inherits(x, "Raster"), inherits(y, "SpatialPolygons"),
    inherits(fun, "function"), assertthat::is.flag(df))
  m <- velox::velox(x)$extract(y, fun)
  if (df) {
    m <- cbind(data.frame(ID = seq_len(length(y))), as.data.frame(m))
    names(m) <- c("ID", names(x))
  }
  return(m)
}

#' Align text
#'
#' Format text by adding a certain number of spaces after new line characters.
#'
#' @param x \code{character} text.
#'
#' @param n \code{integer} number of spaces.
#'
#' @return \code{character}.
#'
#' @examples
#' # make some text
#' original_text <- "animals: horse\npig\nbear"
#'
#' # print text
#' message(original_text)
#'
#' # this look really ugly so we will align it
#' aligned_text <- align_text(original_text, 9)
#'
#' # print aligned text
#' message(aligned_text)
#'
#' @noRd
align_text <- function(x, n) {
  assertthat::assert_that(assertthat::is.string(x), assertthat::is.count(n))
  if (!grepl("\n", x))
    return(x)
  return(gsub("\n", paste0("\n", paste(rep(" ", n), collapse = "")), x,
              fixed = TRUE))
}

#' Default solver name
#'
#' This function returns the name of the default solver. If no sovlers are
#' detected on the system, then a \code{NULL} object is retured.
#'
#' @details This function tests if any of the following packages are installed:
#'   \emph{Rsymphony}, \emph{lpsymphony}, \emph{gurobi}.
#'
#' @return \code{character} indicating the name of the default solver.
#'
#' @noRd
default_solver_name <- function() {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return("gurobi")
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return("Rsymphony")
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return("lpsymphony")
  } else {
    return(NULL)
  }
}

#' Any solvers installed?
#'
#' Test if any solvers are installed.
#'
#' @details This function tests if any of the following packages are installed:
#'   \emph{Rsymphony}, \emph{lpsymphony}, \emph{gurobi}.
#'
#' @return \code{logical} value indicating if any solvers are installed.
#'
#' @noRd
any_solvers_installed <- function() {
  !is.null(default_solver_name())
}

#' Atomic representation
#'
#' Return a pretty character representation of an object with elements and
#  names.
#'
#' @param x \code{object}.
#'
#' @return \code{character} object.
#'
#' @examples
#' repr_atomic(letters)
#' repr_atomic(letters, "characters")
#' @noRd
repr_atomic <- function(x, description = "") {
  n <- length(x)
  if (nchar(description) > 0)
    description <- paste0(" ", description)
  if (length(x) <= 4) {
    x <- x[seq_len(min(length(x), 4))]
  } else {
    x <- c(x[seq_len(min(length(x), 3))], "...")
  }
  paste0(paste(x, collapse = ", "), " (", n, description, ")")
}

#' No extra arguments
#'
#' Check that no additional unused arguments have been supplied to a function
#' through the \code{...}.
#'
#' @param ... arguments that are not used.
#'
#' @return \code{logical} indicating success.
#'
#' @noRd
no_extra_arguments <- function(...) {
  return(length(list(...)) == 0)
}

assertthat::on_failure(no_extra_arguments) <- function(call, env) {
  call_list <- as.list(call)[-1]
  format_args <- function(i) {
    if (names(call_list)[i] == "")
     return(deparse(call_list[[i]]))
    paste0(names(call_list)[i], "=", deparse(call_list[[i]]))
  }
  msg <- paste(vapply(seq_along(call_list), format_args, character(1)),
               collapse = ", ")
  if (length(call_list) > 1) {
    msg <- paste0("unused arguments (", msg, ")")
  } else {
    msg <- paste0("unused argument (", msg, ")")
  }
  msg
}


#' Convert a data.frame to an array
#'
#' Convert a \code{data.frame} that follows \emph{Marxan} formatting to an
#' \code{array} for adding constraints or penalties to a
#' \code{\link{ConservationProblem-class}} object.
#'
#' @param x \code{data.frame} object.
#'
#' @param y \code{\link{ConservationProblem-class}} object.
#'
#' @return \code{array}
#'
#' @noRd
dataframe_to_array <- function(x, y) {
    assertthat::assert_that(
      inherits(y, "ConservationProblem"),
      nrow(x) > 0,
      assertthat::has_name(x, "id1"),
      assertthat::has_name(x, "id2"),
      assertthat::has_name(x, "boundary"),
      assertthat::noNA(x$id1),
      assertthat::noNA(x$id2),
      assertthat::noNA(x$boundary),
      is.numeric(x$id1),
      is.numeric(x$id2), is.numeric(x$boundary),
      all(x$id1 == round(x$id1)),
      all(x$id2 == round(x$id2)))
    if (assertthat::has_name(x, "zone1") ||
        assertthat::has_name(x, "zone2") ||
        y$number_of_zones() > 1) {
        assertthat::assert_that(
          assertthat::has_name(x, "zone1"),
          assertthat::has_name(x, "zone2"),
          assertthat::noNA(x$zone1),
          assertthat::noNA(x$zone1),
          is.character(x$zone1) ||
            is.factor(x$zone1),
          is.character(x$zone2) ||
            is.factor(x$zone2),
          all(as.character(x$zone1) %in% y$zone_names()),
          all(as.character(x$zone2 %in% y$zone_names())))
  }
  # add zone fields if missing
  if (!assertthat::has_name(x, "zone1"))
    x$zone1 <- y$zone_names()
  if (!assertthat::has_name(x, "zone2"))
    x$zone2 <- y$zone_names()
  # convert zone names to indices
  zone1_index <- match(x$zone1, y$zone_names())
  zone2_index <- match(x$zone2, y$zone_names())
  # if planing unit data is a data.frame then standardize ids
  if (inherits(y$data$cost, "data.frame")) {
    x$id1 <- match(x$id1, y$data$cost$id)
    x$id2 <- match(x$id2, y$data$cost$id)
    assertthat::assert_that(assertthat::noNA(x$id1),
      msg = paste("the field data$id1 in argument to",
                  "data contains values for planning units",
                  "not present in the planning unit data"))
    assertthat::assert_that(assertthat::noNA(x$id2),
      msg = paste("the field data$id2 in argument to",
                  "data contains values for planning units",
                  "not present in the planning unit data"))
  } else {
    assertthat::assert_that(
      min(x$id1) > 0,
      min(x$id2) > 0,
      max(x$id1) <= y$number_of_total_units(),
      max(x$id2) <= y$number_of_total_units())
  }
  # create array with data
  connectivity_array <- array(0, dim = c(y$number_of_total_units(),
                                         y$number_of_total_units(),
                                         y$number_of_zones(),
                                         y$number_of_zones()))
  indices <- matrix(c(x$id1, x$id2, zone1_index, zone2_index), ncol = 4)
  connectivity_array[indices] <- x$boundary
  # add data for other diagonal if missing
  pos <- x$id2 < x$id1
  x[pos, c("id1", "id2")] <- x[pos, c("id2", "id1")]
  pos <- zone2_index < zone1_index
  x[pos, c("zone1", "zone2")] <- x[pos, c("zone2", "zone1")]
  if (anyDuplicated(
        paste(x$id1, x$id2,
              x$zone1, x$zone2)) == 0) {
    connectivity_array[indices[, c(2, 1, 4, 3)]] <- x$boundary
  }
  return(connectivity_array)
}
