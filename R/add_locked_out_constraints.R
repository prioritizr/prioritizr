#' @include internal.R Constraint-proto.R intersecting_units.R
NULL

#' Add locked out constraints
#'
#' Add constraints to a conservation planning \code{\link{problem}} to ensure
#' that specific planning units are not selected
#' (or allocated to a specific zone) in the solution. For example, it may be
#' useful to lock out planning units that have been degraded and are not
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
#' @inherit add_contiguity_constraints return seealso
#' @inherit add_locked_in_constraints details
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_polygons, sim_features, sim_locked_out_raster)
#'
#' # create minimal problem
#' p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
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
#' # create problem with added locked out constraints using spatial polygon data
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
#' par(mfrow = c(3,2), mar = c(0, 0, 4.1, 0))
#' plot(s1, main = "none locked out")
#' plot(s1[s1$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s2, main = "locked out (integer input)")
#' plot(s2[s2$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s3, main = "locked out (character input)")
#' plot(s3[s3$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s4, main = "locked out (raster input)")
#' plot(s4[s4$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s5, main = "locked out (polygon input)")
#' plot(s5[s5$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#'
#' # create minimal multi-zone problem with spatial data
#' p6 <- problem(sim_pu_zones_polygons, sim_features_zones,
#'               cost_column = c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(matrix(rpois(15, 1), nrow = 5, ncol = 3)) %>%
#'       add_binary_decisions()
#'
#' # create multi-zone problem with locked out constraints using matrix data
#' locked_matrix <- sim_pu_zones_polygons@data[, c("locked_1", "locked_2",
#'                                                 "locked_3")]
#' locked_matrix <- as.matrix(locked_matrix)
#'
#' p7 <- p6 %>% add_locked_out_constraints(locked_matrix)
#' \donttest{
#' # solve problem
#' s6 <- solve(p6)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s6$solution <- category_vector(s6@data[, c("solution_1_zone_1",
#'                                            "solution_1_zone_2",
#'                                            "solution_1_zone_3")])
#' s6$solution <- factor(s6$solution)
#'
#' # plot solution
#' spplot(s6, zcol = "solution", main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create multi-zone problem with locked out constraints using field names
#' p8 <- p6 %>% add_locked_out_constraints(c("locked_1", "locked_2",
#'                                           "locked_3"))
#' \donttest{
#' # solve problem
#' s8 <- solve(p8)
#'
#' # create new column in s8 representing the zone id that each planning unit
#' # was allocated to in the solution
#' s8$solution <- category_vector(s8@data[, c("solution_1_zone_1",
#'                                            "solution_1_zone_2",
#'                                            "solution_1_zone_3")])
#' s8$solution[s8$solution == 1 & s8$solution_1_zone_1 == 0] <- 0
#' s8$solution <- factor(s8$solution)
#'
#' # plot solution
#' spplot(s8, zcol = "solution", main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create multi-zone problem with raster planning units
#' p9 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(matrix(rpois(15, 1), nrow = 5, ncol = 3)) %>%
#'       add_binary_decisions()
#'
#' # create raster stack with locked out units
#' locked_out_stack <- sim_pu_zones_stack[[1]]
#' locked_out_stack[!is.na(locked_out_stack)] <- 0
#' locked_out_stack <- locked_out_stack[[c(1, 1, 1)]]
#' locked_out_stack[[1]][1] <- 1
#' locked_out_stack[[2]][2] <- 1
#' locked_out_stack[[3]][3] <- 1
#'
#' # plot locked out stack
#' \donttest{
#' plot(locked_out_stack)
#' }
#' # add locked out raster units to problem
#' p9 <- p9 %>% add_locked_out_constraints(locked_out_stack)
#'
#' \donttest{
#' # solve problem
#' s9 <- solve(p9)
#'
#' # plot solution
#' plot(category_layer(s9), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_locked_out_constraints
#'
#' @exportMethod add_locked_out_constraints
#'
#' @aliases add_locked_out_constraints,ConservationProblem,numeric-method add_locked_out_constraints,ConservationProblem,logical-method add_locked_out_constraints,ConservationProblem,matrix-method add_locked_out_constraints,ConservationProblem,character-method add_locked_out_constraints,ConservationProblem,Raster-method add_locked_out_constraints,ConservationProblem,Spatial-method add_locked_out_constraints,ConservationProblem,sf-method
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
      isTRUE(max(locked_out) <= number_of_total_units(x)),
      isTRUE(min(locked_out) >= 1))
    # create matrix with locked out constraints
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
      x$number_of_total_units() == length(locked_out),
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
    y <- data.frame(pu = ind[, 1], zone = x$zone_names()[ind[, 2]], status = 0,
                    stringsAsFactors = FALSE)
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
      inherits(x$data$cost, c("data.frame", "Spatial", "sf")),
      x$number_of_zones() == length(locked_out),
      all(locked_out %in% names(x$data$cost)))
      assertthat::assert_that(
        all(vapply(as.data.frame(x$data$cost)[, locked_out, drop = FALSE],
                   inherits, logical(1), "logical")),
        msg = paste("argument to locked_out refers to a column with data",
                    "that are not logical (i.e TRUE/FALSE)"))
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
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,sf}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "sf"),
  function(x, locked_out) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_out, "sf"),
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
      assertthat::assert_that(raster::cellStats(sum(locked_out, na.rm = TRUE),
                                                "max") <= 1)
    # create matrix with statuses
    if (inherits(x$data$cost, "Raster") && x$number_of_zones() > 1) {
      status <- vapply(seq_len(x$number_of_zones()),
                       FUN.VALUE = logical(x$number_of_total_units()),
                       function(i) replace(rep(FALSE,
                                               x$number_of_total_units()),
                                           intersecting_units(x$data$cost[[i]],
                                                              locked_out[[i]]),
                                           TRUE))
    } else {
      status <- vapply(seq_len(x$number_of_zones()),
                       FUN.VALUE = logical(x$number_of_total_units()),
                       function(i) replace(rep(FALSE,
                                               x$number_of_total_units()),
                                           intersecting_units(x$data$cost,
                                                              locked_out[[i]]),
                                           TRUE))
    }
    # add constraints
    add_locked_out_constraints(x, status)
})
