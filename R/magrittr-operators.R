#' @include internal.R
NULL

#' Pipe operator
#'
#' This package uses the pipe operator (\code{\%>\%}) to express nested code
#' as a series of imperative procedures.
#'
#' @param lhs,rhs An object and a function.
#'
#' @seealso \code{\link[magrittr]{\%>\%}}, \code{\link{tee}}.
#'
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
#' # repeat the previous procedure but use the pipe operator instead of nesting
#' # function calls inside each other.
#' runif(100) %>% mean()
#'
#' @name %>%
#'
#' @rdname pipe
#'
#' @aliases pipe
#'
#' @importFrom magrittr %>%
#'
#' @export
NULL

#' Tee operator
#'
#' This package uses the "tee" operator (\code{\%T>\%}) to modify objects.
#'
#' @param lhs,rhs An object and a function.
#'
#' @seealso \code{\link[magrittr]{\%T>\%}}, \code{\link{pipe}}.
#'
#' @examples
#' # the tee operator returns the left-hand side of the result and can be
#' # useful when dealing with mutable objects. In this example we want
#' # to use the function "f" to modify the object "e" and capture the
#' # result
#'
#' # create an empty environment
#' e <- new.env()
#'
#' # create a function to modify an environment and return NULL
#' f <- function(x) {x$a <- 5; return(NULL)}
#'
#' # if we use the pipe operator we won't capture the result since "f"()
#' # returns a NULL
#' e2 <- e %>% f()
#' print(e2)
#'
#' # but if we use the tee operator then the result contains a copy of "e"
#' e3 <- e %T>% f()
#' print(e3)
#'
#' @name %T>%
#'
#' @rdname tee
#'
#' @aliases tee
#'
#' @importFrom magrittr %T>%
#'
#' @export
NULL
