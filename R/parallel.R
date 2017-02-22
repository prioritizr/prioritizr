#' @include internal.R
NULL

# Number of threads for data processing
#' 
#' This function set and get the number of threads for processing data when 
#' processing data. Note that this does not influence the number of 
#' threads used when solving a conservation problem.
#' 
#' @param x \code{integer} number of threads to use for processing.
#'
#' @return \describe{
#'   \item{get_number_of_threads}{\code{integer} number of threads.}
#'   \item{set_number_of_threads}{invisible \code{logical} indicating 
#'     success.}
#' }
#'
#' @details To cease processing data in parallel, set the number of 
#'   threads to one.
#'
#' @seealso \code{\link{is.parallel}}.
#'
#' @name parallel
#' @aliases set_number_of_threads get_number_of_threads
NULL

#' @rdname parallel
#' @export
set_number_of_threads <- function(x=1L) {
  assertthat::assert_that(assertthat::is.count(x),
                          x > 0,
                          x <= parallel::detectCores())
  # stop existing cluster if detected
  if (!is.null(.pkgenv$cluster)) {
    parallel::stopCluster(.pkgenv$cluster)
    .pkgenv$cluster <- NULL
    doParallel::stopImplicitCluster()
  }
  # initialize new cluster with number of specified cores
  if (x > 1) {
    .pkgenv$cluster <- parallel::makeCluster(x, type='PSOCK')
    doParallel::registerDoParallel(.pkgenv$cluster)
  }
  invisible(TRUE)
}

#' @rdname parallel
#' @export
get_number_of_threads <- function() {
  if (is.parallel())
    return(length(.pkgenv$cluster))
  1L
}

#' Is parallel?
#' 
#' This function determines if parallel processing capabilities have been set 
#' up.
#'
#' @return \code{logical} indicating if parallel computations will be
#'   performed in parallel.
#'
#' @seealso \code{\link{set_number_of_threads}}.
#' 
#' @export
is.parallel <- function() {
  !is.null(.pkgenv$cluster)
}

#' Distribute load
#'
#' This function helps with distributing a work load among processors
#' for parallel processing by returning a list with index that 
#'
#' @param x \code{integer} number of item to process.
#'
#' @param n \code{integer} number of threads.
#'
#' @details This function returns a \code{list} containing an element for
#'   each thread. Each element contains a \code{integer} vector 
#'   specifying the indices that a thread should process.
#'
#' @return \code{list} object.
#'
#' @seealso \code{\link{get_number_of_threads}}, 
#'   \code{\link{set_number_of_threads}}, \code{\link{is.parallel}}.
#'
#' @export
distribute_load <- function(x, n=get_number_of_threads()) {
  assertthat::assert_that(assertthat::is.count(x),
                          assertthat::is.count(n),
                          isTRUE(x > 0),
                          isTRUE(n > 0))
  if (n==1) {
    i <- list(seq_len(x))
  } else if (x <= n) {
    i <- as.list(seq_len(x))
  } else {
    j <- as.integer(floor(seq(1, n+1, length.out=x+1)))
    i <- list()
    for (k in seq_len(n)) {
        i[[k]] <- which(j==k)
    }
  }
  i
}
