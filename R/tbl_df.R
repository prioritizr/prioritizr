#' @include internal.R
NULL

#' @export
if (!methods::isClass("tbl_df")) methods::setOldClass("tbl_df")
NULL

# create nrow/ncol/as.list methods for handling tibble's tbl_df class internally
# we do this because nrow/ncol/as.list are overwritten as a S4 generic by the
# raster package, and so R needs to be forcefully told what to do because
# tibble doesn't export it's S4 class definitions. This means that when
# we call nrow/ncol/as.list in prioritizr's source code, R uses prioritizr's
# definition of tbl_df when it is expecting a definition from the tibble
# package. R then kindly/annoyingly informs us this by printing the following
# text:
#
#   "Found more than one class "tbl_df" in cache; using the first, from
#   namespace 'prioritizr'"
#
# To avoid this R from producing this text, we will methods for
# internally handling tbl_df objects inside prioritizr.

#' Manipulate tibbles
#'
#' Assorted functions for manipulating \code{\link[tibble]{tibble}} objects.
#'
#' @param x \code{\link[tibble]{tibble}} object.
#'
#' @details The following methods are provided from manipulating
#'   \code{\link[tibble]{tibble}} objects.
#'
#'   \describe{
#'   \item{nrow}{extract \code{integer} number of rows.}
#'
#'   \item{ncol}{extract \code{integer} number of columns.}
#'
#'   \item{as.list}{convert to a \code{list}.}
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
