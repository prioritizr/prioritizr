#' @include internal.R
NULL

#' Convert to Matrix
#'
#' Convert an object to a matrix class provided by the \pkg{Matrix} package.
#'
#' @param object object.
#'
#' @param class `character` name of new classes.
#'
#' @details
#' This function is a wrapper that is designed to provide
#' compatibility with older and newer versions of the \pkg{Matrix} package.
#'
#' @return A `Matrix` object.
#'
#' @examples
#' # create matrix
#' m <- matrix(1:9, ncol = 3)
#'
#' # convert to sparse matrix format
#' m2 <- as_Matrix(m, "dgCMatrix")
#'
#' # display converted matrix
#' print(m2)
#'
#' @export
as_Matrix <- function(object, class) {
  # assert valid argument
  assert(
    assertthat::is.string(class),
    assertthat::noNA(class)
  )
  # if object is already of the same class then just return it
  if (identical(base::class(object)[[1]], class)) {
    return(object)
  }
  # if we just want to convert to generic Matrix class then do that...
  if (identical(class, "Matrix")) {
    return(methods::as(object, class))
  }
  # convert matrix
  # nocov start
  if (utils::packageVersion("Matrix") >= as.package_version("1.4-2")) {
    if (identical(class, "dgCMatrix")) {
      c1 <- "dMatrix"
      c2 <- "generalMatrix"
      c3 <- "CsparseMatrix"
    } else if (identical(class, "dgTMatrix")) {
      c1 <- "dMatrix"
      c2 <- "generalMatrix"
      c3 <- "TsparseMatrix"
    } else if (identical(class, "dsCMatrix")) {
      c1 <- "dMatrix"
      c2 <- "symmetricMatrix"
      c3 <- "CsparseMatrix"
    } else if (identical(class, "dsTMatrix")) {
      c1 <- "dMatrix"
      c2 <- "symmetricMatrix"
      c3 <- "TsparseMatrix"
    } else if (identical(class, "lgCMatrix")) {
      c1 <- "lMatrix"
      c2 <- "generalMatrix"
      c3 <- "CsparseMatrix"
    } else {
      cli::cli_abort("{.arg class} not recognized.", call = NULL)
    }
    out <- methods::as(methods::as(methods::as(object, c1), c2), c3)
  } else {
    out <- methods::as(object, class)
  }
  # nocov end
  # return result
  out
}
