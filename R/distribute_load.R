#' @include internal.R
NULL

#' Distribute load
#'
#' Utility function for distributing computations among a pool of workers
#' for parallel processing.
#'
#' @param x `integer` number of item to process.
#'
#' @param n `integer` number of threads.
#'
#' @details This function returns a `list` containing an element for
#'   each worker. Each element contains a `integer` `vector`
#'   specifying the indices that the worker should process.
#'
#' @return `list` object.
#'
#' @examples
#' \donttest{
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
#' }
#'
#' @name distribute_load
#'
#' @export
distribute_load <- function(x, n = 1) {
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
