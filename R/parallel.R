#' @include internal.R
NULL

#' Number of threads for data processing
#'
#' Set and get the number of threads used for processing data. Note
#' that this does not influence the number of threads used when solving a
#' conservation problem and this must be set when adding the
#' solver to the problem.
#'
#' @param x \code{integer} number of threads to use for processing.
#'
#' @return
#'
#' \describe{
#'
#'   \item{get_number_of_threads}{\code{integer} number of threads.}
#'
#'   \item{set_number_of_threads}{invisible \code{logical} indicating
#'     success.}
#'
#' }
#'
#' @details To stop processing data in parallel, set the number of
#'   threads to one.
#'
#' @seealso \code{\link{is.parallel}}, \code{\link{solvers}}.
#'
#' @name parallel
#'
#' @examples
#' # set number of threads to 2
#' set_number_of_threads(2)
#
#' # get number of threads
#' get_number_of_threads()
#'
#' # reset number of threads to 1
#' set_number_of_threads(1)
#'
#' # get number of threads
#' get_number_of_threads()
#'
#' @aliases set_number_of_threads get_number_of_threads
NULL

#' @rdname parallel
#' @export
set_number_of_threads <- function(x=1L) {
  assertthat::assert_that(assertthat::is.count(x),
                          x > 0,
                          x <= parallel::detectCores(TRUE))
  # stop existing cluster if detected
  if (!is.null(.pkgenv$cluster)) {
    parallel::stopCluster(.pkgenv$cluster)
    .pkgenv$cluster <- NULL
    doParallel::stopImplicitCluster()
  }
  # initialize new cluster with number of specified cores
  if (x > 1) {
    .pkgenv$cluster <- parallel::makeCluster(x, type = "PSOCK")
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
#' This function determines if parallel processing capabilities have been
#' initialized.
#'
#' @return \code{logical} indicating if parallel computations will be
#'   performed in parallel where possible.
#'
#' @seealso \code{\link{set_number_of_threads}},
#'   \code{\link{get_number_of_threads}}.
#'
#' @examples
#' # set number of threads to 2
#' set_number_of_threads(2)
#
#' # get number of threads
#' get_number_of_threads()
#'
#' # check that parallel processing is active
#' is.parallel()
#'
#' # reset number of threads to 1
#' set_number_of_threads(1)
#'
#' # check that parallel processing has been deactivated
#' is.parallel()
#'
#' @export
is.parallel <- function() {
  !is.null(.pkgenv$cluster)
}

#' Distribute load
#'
#' Utility function for distributing computations among a pool of workers
#' for parallel processing.
#'
#' @param x \code{integer} number of item to process.
#'
#' @param n \code{integer} number of threads.
#'
#' @details This function returns a \code{list} containing an element for
#'   each worker. Each element contains a \code{integer} \code{vector}
#'   specifying the indices that the worker should process.
#'
#' @return \code{list} object.
#'
#' @seealso \code{\link{get_number_of_threads}},
#'   \code{\link{set_number_of_threads}}, \code{\link{is.parallel}}.
#'
#' @examples
#'
#' # imagine that we have 10 jobs that need processing. For simplicity,
#' # our jobs will involve adding 1 to each element in 1:10.
#' values <- 1:10
#'
#' # we could complete this processing using the following vectorized code
#' result <- 1 + 1:10
#' print(result)
#'
#' # however, if our jobs were complex then we would be better off using
#' # functionals
#' result <- lapply(1:10, function(x) x + 1)
#' print(result)
#'
#' # we could do one better, and use the "plyr" package to handle the
#' # processing
#' result <- plyr::llply(1:10, function(x) x + 1)
#' print(result)
#'
#' # we could also use the parallel processing options available through "plyr"
#' # to use more computation resources to complete the jobs (note that since
#' # these jobs are very quick to process this is actually slower).
#' cl <- parallel::makeCluster(2, "PSOCK")
#' doParallel::registerDoParallel(cl)
#' result <- plyr::llply(1:10, function(x) x + 1, .parallel = TRUE)
#' cl <- parallel::stopCluster(cl)
#' print(result)
#'
#' # however this approach iterates over each element individually, we could
#' # use the distribute_load function to split the N jobs up into K super
#' # jobs, and evaluate each super job using vectorized code.
#' x <- 1:10
#' cl <- parallel::makeCluster(2, "PSOCK")
#' parallel::clusterExport(cl, 'x', envir = environment())
#' doParallel::registerDoParallel(cl)
#' l <- distribute_load(length(x), n = 2)
#' result <- plyr::llply(l, function(i) x[i] + 1, .parallel = TRUE)
#' cl <- parallel::stopCluster(cl)
#' print(result)
#'
#' @name distribute_load
#'
#' @export
distribute_load <- function(x, n=get_number_of_threads()) {
  assertthat::assert_that(assertthat::is.count(x),
                          assertthat::is.count(n),
                          isTRUE(x > 0),
                          isTRUE(n > 0))
  if (n == 1) {
    i <- list(seq_len(x))
  } else if (x <= n) {
    i <- as.list(seq_len(x))
  } else {
    j <- as.integer(floor(seq(1, n + 1, length.out = x + 1)))
    i <- list()
    for (k in seq_len(n)) {
        i[[k]] <- which(j == k)
    }
  }
  i
}
