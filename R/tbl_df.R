#' @include internal.R
NULL

#' @export
#' @importClassesFrom tibble tbl_df
NULL

# Dear reader,
# We create nrow/ncol/as.list methods for handling tibble's tbl_df class
# internally because nrow/ncol/as.list are overwritten as a S4 generic by the
# raster package, and so R needs to be forcefully told what to do because
# tibble doesn't export it's S4 class methods.

#' Manipulate tibbles
#'
#' Assorted functions for manipulating [tibble::tibble()] objects.
#'
#' @param x [tibble::tibble()] object.
#'
#' @details The following methods are provided from manipulating
#'   [tibble::tibble()] objects.
#'
#'   \describe{
#'   \item{nrow}{`integer` number of rows.}
#'
#'   \item{ncol}{`integer` number of columns.}
#'
#'   \item{as.list}{convert to a `list`.}
#'
#'   \item{print}{print the object.}
#'
#'   }
#'
#' @name tibble-methods
#'
#' @aliases nrow,tbl_df-method ncol,tbl_df-method as.list,tbl_df-method
#'
#' @examples
#' # load tibble package
#' require(tibble)
#'
#' # make tibble
#' a <- tibble(value = seq_len(5))
#'
#' # number of rows
#' nrow(a)
#'
#' # number of columns
#' ncol(a)
#'
#' # convert to list
#' as.list(a)
NULL

#' @name tibble-methods
#'
#' @rdname tibble-methods
#'
#' @usage \S4method{nrow}{tbl_df}(x)
methods::setMethod("nrow", "tbl_df", function(x) dim(x)[1])

#' @name tibble-methods
#'
#' @rdname tibble-methods
#'
#' @usage \S4method{ncol}{tbl_df}(x)
methods::setMethod("ncol", "tbl_df", function(x) dim(x)[2])

#' @name tibble-methods
#'
#' @rdname tibble-methods
#'
#' @usage \S4method{as.list}{tbl_df}(x)
methods::setMethod("as.list", "tbl_df", function(x) base::as.list(x))
