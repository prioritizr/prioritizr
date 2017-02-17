#' @include internal.R
NULL

#' Pipe operator
#'
#' This package uses the pipe operator (\code{\%>\%}) to turn operators into
#' a series of imperative procedures.
#'
#' @importFrom magrittr %>%
#' @param lhs,rhs An object and a function.
#' @export
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # generate 100 random numbers and calculate the mean
#' mean(runif(100))
#'
#' # reset the seed
#' set.seed(500)
#'
#' # repeat the procedure of generate 100 random numbers and calculating the 
#' # mean. But this time use the pipe operator instead of nesting calls
#' # inside each other.
#' runif(100) %>% mean()
#'
#' @name %>%
NULL

