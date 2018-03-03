#' @include internal.R Constraint-proto.R intersecting_units.R
NULL

#' Add locked out constraints
#'
#' Add constraints to ensure that certain planning units are not selected
#' (or allocated to a specific zone) in the solution. For example, it may be
#' useful to lock out planning units that have been degraded and are not longer
#' suitable for conserving species. If specific planning units should be locked
#' in to the solution, use \code{\link{add_locked_out_constraints}}. For
#' problems with non-binary planning unit allocations (e.g. proportions), the
#' \code{\link{add_manual_locked_constraints}} function can be used to lock
#' planning unit allocations to a specific value.
#'
#' @usage add_locked_out_constraints(x, locked_out)
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param locked_out Object that determines which planning units that should be
#'   locked out. See the Details section for more information.
#'
#' @inherit add_locked_out_constraints details return seealso
#'
#' @examples
#' # create basic problem
#' p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2)
#'
#' # create problem with added locked out constraints using integers
#' p2 <- p1 %>% add_locked_out_constraints(which(sim_pu_polygons$locked_out))
#'
#' # create problem with added locked out constraints using a field name
#' p3 <- p1 %>% add_locked_out_constraints("locked_out")
#'
#' # create problem with added locked out constraints using raster data
#' p4 <- p1 %>% add_locked_out_constraints(sim_locked_out_raster)
#'
#' # create problem with added locked out constraints using spatial polygons
#' # data
#' locked_out <- sim_pu_polygons[sim_pu_polygons$locked_out == 1, ]
#' p5 <- p1 %>% add_locked_out_constraints(locked_out)
#' \donttest{
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#' s5 <- solve(p5)
#'
#' # plot solutions
#' par(mfrow = c(3, 2), mar = c(0, 0, 4.1, 0))
#' plot(s1, main = "none locked out")
#' plot(s1[s1$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s2, main="locked out (integer input)")
#' plot(s2[s2$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s3, main="locked out (character input)")
#' plot(s3[s3$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s4, main = "locked out (raster input)")
#' plot(s4[s4$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s5, main = "locked out (polygon input)")
#' plot(s5[s5$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#'
#' @name add_locked_out_constraints
#'
#' @exportMethod add_locked_out_constraints
#'
#' @aliases add_locked_out_constraints,ConservationProblem,numeric-method add_locked_out_constraints,ConservationProblem,logical-method add_locked_out_constraints,ConservationProblem,matrix-method add_locked_out_constraints,ConservationProblem,character-method add_locked_out_constraints,ConservationProblem,Raster-method add_locked_out_constraints,ConservationProblem,Spatial-method
#'
#' @export
methods::setGeneric("add_locked_out_constraints",
                    signature = methods::signature("x", "locked_out"),
                    function(x, locked_out)
                      standardGeneric("add_locked_out_constraints"))


#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,numeric}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "numeric"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_out, c("integer", "numeric")),
      x$number_of_zones() == 1,
      isTRUE(all(is.finite(locked_out))),
      isTRUE(all(round(locked_out) == locked_out)),
      isTRUE(max(locked_out) <= x$number_of_total_units()),
      isTRUE(min(locked_out) >= 1))
    # create matrix with locked in constraints
    m <- matrix(FALSE, ncol = 1, nrow = x$number_of_total_units())
    m[locked_out, 1] <- TRUE
    # add constraints
    add_locked_out_constraints(x, m)
})

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,logical}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "logical"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_out, "logical"),
      x$number_of_zones() == 1,
      x$number_of_total_units() == length(x),
      isTRUE(all(is.finite(locked_out))))
    # add constraints
    add_locked_out_constraints(x, matrix(locked_out, ncol = 1))
})

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,matrix}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "matrix"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_out, "matrix"),
      is.logical(locked_out),
      x$number_of_zones() == ncol(locked_out),
      x$number_of_total_units() == nrow(locked_out),
      isTRUE(all(is.finite(locked_out))),
      all(rowSums(locked_out) <= 1))
    # create data.frame with statuses
    ind <- which(locked_out, arr.ind = TRUE)
    y <- data.frame(pu = ind[, 1], zone = x$zone_names()[ind[, 2]], status = 0)
    # add constraints
    add_manual_locked_constraints(x, y)
})

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,character}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "character"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      is.character(locked_out), !anyNA(locked_out),
      inherits(x$data$cost, c("data.frame", "Spatial")),
      x$number_of_zones() == length(locked_out),
      all(locked_out %in% names(x$data$cost)),
      all(vapply(as.data.frame(x$data$cost)[, locked_out, drop = FALSE],
                 inherits, logical(1), "logical")))
    # add constraints
    add_locked_out_constraints(x,
      as.matrix(as.data.frame(x$data$cost)[, locked_out, drop = FALSE]))
})

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,Spatial}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_out, "Spatial"),
      x$number_of_zones() == 1)
    # add constraints
    add_locked_out_constraints(x, intersecting_units(x$data$cost, locked_out))
})

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,Raster}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "Raster"),
  function(x, locked_out) {
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_out, "Raster"),
      x$number_of_zones() == raster::nlayers(locked_out),
      all(max(raster::cellStats(locked_out, "sum")) > 0))
    if (raster::nlayers(locked_out) > 1)
      assertthat::assert_that(raster::cellStats(sum(locked_out), "max") <= 1)
    # create matrix with statuses
    status <- vapply(seq_len(x$number_of_zones()),
                     FUN.VALUE = logical(x$number_of_total_units()),
                     function(i) replace(rep(FALSE, x$number_of_total_units()),
                                         intersecting_units(x$data$cost[[i]],
                                                            locked_out[[i]]),
                                         TRUE))
    # add constraints
    add_locked_out_constraints(x, status)
})
