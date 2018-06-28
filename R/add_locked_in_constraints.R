#' @include internal.R Constraint-proto.R intersecting_units.R
NULL

#' Add locked in constraints
#'
#' Add constraints to a conservation planning \code{\link{problem}} to ensure
#' that specific planning units are selected (or allocated
#' to a specific zone) in the solution. For example, it may be desirable to
#' lock in planning units that are inside existing protected areas so that the
#' solution fills in the gaps in the existing reserve network. If specific
#' planning units should be locked out of a solution, use
#' \code{\link{add_locked_out_constraints}}. For problems with non-binary
#' planning unit allocations (e.g. proportions), the
#' \code{\link{add_manual_locked_constraints}} function can be used to lock
#' planning unit allocations to a specific value.
#'
#' @usage add_locked_in_constraints(x, locked_in)
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param locked_in Object that determines which planning units that should be
#'   locked in. See the Details section for more information.
#'
#' @details The locked planning units can be specified in several different
#'   ways. Generally, the locked data should correspond to the planning units
#'   in the argument to \code{x.} To help make working with
#'   \code{\link{Raster-class}} planning unit data easier,
#'   the locked data should correspond to cell indices in the
#'   \code{\link{Raster-class}} data. For example, \code{integer} arguments
#'   should correspond to cell indices and \code{logical} arguments should have
#'   a value for each cell---regardless of which planning unit cells contain
#'   \code{NA} values.
#'
#'   \describe{
#'
#'   \item{\code{integer}}{\code{vector} of indices pertaining to which
#'     planning units should be locked in the solution. This argument is only
#'     compatible with problems that contain a single zone.}
#'
#'   \item{\code{logical}}{\code{vector} containing \code{TRUE} and/or
#'     \code{FALSE} values that indicate which planning units should be locked
#'     in the solution. This argument is only compatible with problems that
#'     contain a single zone.}
#'
#'   \item{\code{matrix}}{containing \code{logical} \code{TRUE} and/or
#'     \code{FALSE} values which indicate if certain planning units are
#'     should be locked to a specific zone in the solution. Each row
#'     corresponds to a planning unit, each column corresponds to a zone, and
#'     each cell indicates if the planning unit should be locked to a given
#'     zone. Thus each row should only contain at most a single \code{TRUE}
#'     value.}
#'
#'   \item{\code{character}}{field (column) name(s) that indicate if planning
#'     units should be locked in the solution. This type of argument is only
#'     compatible if the planning units in the argument to \code{x} are a
#'     \code{\link[sp]{Spatial-class}} or \code{data.frame} object. The fields
#'     (columns) must have \code{logical}  (i.e. \code{TRUE} or \code{FALSE})
#'     values indicating if the planning unit is to be locked in the solution.
#'     For problems containing multiple zones, this argument should contain
#'     a field (column) name for each management zone.}
#'
#'   \item{\code{\link[raster]{Raster-class}}}{planning units in \code{x}
#'     that intersect with non-zero and non-\code{NA} raster cells are locked
#'     in the solution. For problems that contain multiple zones, the
#'     \code{\link[raster]{Raster-class}} object must contain a layer
#'     for each zone. Note that for multi-band arguments, each pixel must
#'     only contain a non-zero value in a single band.}
#'
#'   }
#'
#' @inherit add_connected_constraints return seealso
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_polygons, sim_features, sim_locked_in_raster)
#'
#' # create minimal problem
#' p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
#'
#' # create problem with added locked in constraints using integers
#' p2 <- p1 %>% add_locked_in_constraints(which(sim_pu_polygons$locked_in))
#'
#' # create problem with added locked in constraints using a field name
#' p3 <- p1 %>% add_locked_in_constraints("locked_in")
#'
#' # create problem with added locked in constraints using raster data
#' p4 <- p1 %>% add_locked_in_constraints(sim_locked_in_raster)
#'
#' # create problem with added locked in constraints using spatial polygon data
#' locked_in <- sim_pu_polygons[sim_pu_polygons$locked_in == 1, ]
#' p5 <- p1 %>% add_locked_in_constraints(locked_in)
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
#' plot(s1, main = "none locked in")
#' plot(s1[s1$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s2, main = "locked in (integer input)")
#' plot(s2[s2$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s3, main = "locked in (character input)")
#' plot(s3[s3$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s4, main = "locked in (raster input)")
#' plot(s4[s4$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s5, main = "locked in (polygon input)")
#' plot(s5[s5$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#'
#' # create minimal multi-zone problem with spatial data
#' p6 <- problem(sim_pu_zones_polygons, sim_features_zones,
#'               cost_column = c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(matrix(rpois(15, 1), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions()
#'
#' # create multi-zone problem with locked in constraints using matrix data
#' locked_matrix <- sim_pu_zones_polygons@data[, c("locked_1", "locked_2",
#'                                                 "locked_3")]
#' locked_matrix <- as.matrix(locked_matrix)
#'
#' p7 <- p6 %>% add_locked_in_constraints(locked_matrix)
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
#' # create multi-zone problem with locked in constraints using field names
#' p8 <- p6 %>% add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
#' \donttest{
#' # solve problem
#' s8 <- solve(p8)
#'
#' # create new column representing the zone id that each planning unit
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
#' # create raster stack with locked in units
#' locked_in_stack <- sim_pu_zones_stack[[1]]
#' locked_in_stack[!is.na(locked_in_stack)] <- 0
#' locked_in_stack <- locked_in_stack[[c(1, 1, 1)]]
#' locked_in_stack[[1]][1] <- 1
#' locked_in_stack[[2]][2] <- 1
#' locked_in_stack[[3]][3] <- 1
#'
#' # plot locked in stack
#' plot(locked_in_stack)
#'
#' # add locked in raster units to problem
#' p9 <- p9 %>% add_locked_in_constraints(locked_in_stack)
#'
#' \donttest{
#' # solve problem
#' s9 <- solve(p9)
#'
#' # plot solution
#' plot(category_layer(s9), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @seealso \code{\link{constraints}}.
#'
#' @name add_locked_in_constraints
#'
#' @exportMethod add_locked_in_constraints
#'
#' @aliases add_locked_in_constraints,ConservationProblem,numeric-method add_locked_in_constraints,ConservationProblem,logical-method add_locked_in_constraints,ConservationProblem,matrix-method add_locked_in_constraints,ConservationProblem,character-method  add_locked_in_constraints,ConservationProblem,Raster-method add_locked_in_constraints,ConservationProblem,Spatial-method

#'
#' @export
methods::setGeneric("add_locked_in_constraints",
                    signature = methods::signature("x", "locked_in"),
                    function(x, locked_in)
                      standardGeneric("add_locked_in_constraints"))

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,numeric}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "numeric"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_in, c("integer", "numeric")),
      x$number_of_zones() == 1,
      isTRUE(all(is.finite(locked_in))),
      isTRUE(all(round(locked_in) == locked_in)),
      isTRUE(max(locked_in) <= number_of_total_units(x)),
      isTRUE(min(locked_in) >= 1))
    # create matrix with locked in constraints
    m <- matrix(FALSE, ncol = 1, nrow = x$number_of_total_units())
    m[locked_in, 1] <- TRUE
    # add constraints
    add_locked_in_constraints(x, m)
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,logical}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "logical"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_in, "logical"),
      x$number_of_zones() == 1,
      x$number_of_total_units() == length(locked_in),
      isTRUE(all(is.finite(locked_in))))
    # add constraints
    add_locked_in_constraints(x, matrix(locked_in, ncol = 1))
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,matrix}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "matrix"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_in, "matrix"),
      is.logical(locked_in),
      x$number_of_zones() == ncol(locked_in),
      x$number_of_total_units() == nrow(locked_in),
      all(is.finite(locked_in)),
      all(rowSums(locked_in) <= 1))
    # create data.frame with statuses
    ind <- which(locked_in, arr.ind = TRUE)
    y <- data.frame(pu = ind[, 1], zone = x$zone_names()[ind[, 2]], status = 1)
    # add constraints
    add_manual_locked_constraints(x, y)
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,character}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "character"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      is.character(locked_in), !anyNA(locked_in),
      inherits(x$data$cost, c("data.frame", "Spatial")),
      x$number_of_zones() == length(locked_in),
      all(locked_in %in% names(x$data$cost)),
      all(vapply(as.data.frame(x$data$cost)[, locked_in, drop = FALSE],
                 inherits, logical(1), "logical")))
    # add constraints
    add_locked_in_constraints(x,
      as.matrix(as.data.frame(x$data$cost)[, locked_in, drop = FALSE]))
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,Spatial}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_in, "Spatial"),
      x$number_of_zones() == 1)
    # add constraints
    add_locked_in_constraints(x, intersecting_units(x$data$cost, locked_in))
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,Raster}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "Raster"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_in, "Raster"),
      x$number_of_zones() == raster::nlayers(locked_in),
      all(max(raster::cellStats(locked_in, "sum")) > 0))
    if (raster::nlayers(locked_in) > 1)
      assertthat::assert_that(raster::cellStats(sum(locked_in), "max") <= 1)
    # create matrix with statuses
    if (inherits(x$data$cost, "Raster") && x$number_of_zones() > 1) {
      status <- vapply(seq_len(x$number_of_zones()),
                       FUN.VALUE = logical(x$number_of_total_units()),
                       function(i) replace(rep(FALSE,
                                               x$number_of_total_units()),
                                           intersecting_units(x$data$cost[[i]],
                                                              locked_in[[i]]),
                                           TRUE))
    } else {
      status <- vapply(seq_len(x$number_of_zones()),
                       FUN.VALUE = logical(x$number_of_total_units()),
                       function(i) replace(rep(FALSE,
                                               x$number_of_total_units()),
                                           intersecting_units(x$data$cost,
                                                              locked_in[[i]]),
                                           TRUE))
    }
    # add constraints
    add_locked_in_constraints(x, status)
})
