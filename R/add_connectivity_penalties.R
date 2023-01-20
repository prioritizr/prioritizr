#' @include internal.R Penalty-proto.R marxan_connectivity_data_to_matrix.R
NULL

#' Add connectivity penalties
#'
#' Add penalties to a conservation planning [problem()] to account for
#' symmetric connectivity between planning units.
#' Symmetric connectivity data describe connectivity information that is not
#' directional. For example, symmetric connectivity data could describe which
#' planning units are adjacent to each other (see [adjacency_matrix()],
#' or which planning units are within threshold distance of each other (see
#' [proximity_matrix()]).
#'
#' @param x [problem()] object.
#'
#' @param penalty `numeric` penalty that is used to scale the importance
#'   of selecting planning units with strong connectivity between them compared
#'   to the main problem objective (e.g., solution cost when the argument to
#'   `x` has a minimum set objective set using
#'   [add_min_set_objective()]). Higher `penalty` values
#'   can be used to obtain solutions with a high degree of connectivity,
#'   and smaller `penalty` values can be used to obtain solutions with a
#'   small degree of connectivity. Note that negative `penalty` values can
#'   be used to obtain solutions that have very little connectivity.
#'
#' @param zones `matrix` or `Matrix` object describing the
#'   level of connectivity between different zones. Each row and column
#'   corresponds to a different zone in the argument to `x`, and cell
#'   values indicate the level of connectivity between each combination
#'   of zones. Cell values along the diagonal of the matrix represent
#'   the level of connectivity between planning units allocated to the
#'   same zone. Cell values must lay between 1 and -1, where negative
#'   values favor solutions with weak connectivity. The default argument to
#'   `zones` is an identity matrix (i.e., a matrix with ones along the
#'   matrix diagonal and zeros elsewhere), so that planning units are
#'   only considered to be connected when they are allocated to the same zone.
#'   This argument is required when working with multiple zones and the
#'   argument to `data` is a `matrix` or `Matrix` object.
#'   If the argument to `data` is an `array` or `data.frame` with data for
#'   multiple zones (e.g., using the `"zone1"` and `"zone2"` column names),
#'   this argument must explicitly be set to `NULL` otherwise an error will be
#'   thrown.
#'
#' @param data `matrix`, `Matrix`, `data.frame`, or
#'   `array` object containing connectivity data. The connectivity values
#'   correspond to the strength of connectivity between
#'   different planning units. Thus connections between planning units
#'   that are associated with higher values are more favorable in the solution.
#'   See the Data format section for more information.
#'
#' @details
#' This function adds penalties to conservation planning problem to penalize
#' solutions that have low connectivity.
#' Specifically, it favors pair-wise connections between planning units
#' that have high connectivity values (based on Önal and Briers 2002).
#'
#' @section Data format:
#' The argument to `data` can be specified using several different formats.
#'
#' \describe{
#'
#' \item{`data` as a `matrix`/`Matrix` object}{where rows and columns represent
#'   different planning units and the value of each cell represents the
#'   strength of connectivity between two different planning units. Cells
#'   that occur along the matrix diagonal are treated as weights which
#'   indicate that planning units are more desirable in the solution.
#'   The argument to `zones` can be used to control
#'   the strength of connectivity between planning units in different zones.
#'   The default argument for `zones` is to treat planning units
#'   allocated to different zones as having zero connectivity.}
#'
#' \item{`data` as a `data.frame` object}{containing the fields (columns)
#'   `"id1"`, `"id2"`, and `"boundary"`. Here, each row
#'   denotes the connectivity between a pair of planning units
#'   (per values in the `"id1"` and `"id2"` columns) following the
#'   *Marxan* format.
#'   If the argument to `x` contains multiple zones, then the columns
#'   `"zone1"` and `"zone2"` can optionally be provided to manually
#'   specify the connectivity values between planning units when they are
#'   allocated to specific zones. If the columns `"zone1"` and
#'   `"zone2"` are present, then the argument to `zones` must be
#'   `NULL`.}
#'
#' \item{`data` as an `array` object}{
#'   containing four-dimensions where cell values
#'   indicate the strength of connectivity between planning units
#'   when they are assigned to specific management zones. The first two
#'   dimensions (i.e., rows and columns) indicate the strength of
#'   connectivity between different planning units and the second two
#'   dimensions indicate the different management zones. Thus
#'   the `data[1, 2, 3, 4]` indicates the strength of
#'   connectivity between planning unit 1 and planning unit 2 when planning
#'   unit 1 is assigned to zone 3 and planning unit 2 is assigned to zone 4.}
#'
#' }
#'
#' @section Mathematical formulation:
#' The connectivity penalties are implemented using the following equations.
#' Let \eqn{I} represent the set of planning units
#' (indexed by \eqn{i} or \eqn{j}), \eqn{Z} represent the set
#' of management zones (indexed by \eqn{z} or \eqn{y}), and \eqn{X_{iz}}{Xiz}
#' represent the decision variable for planning unit \eqn{i} for in zone
#' \eqn{z} (e.g., with binary
#' values one indicating if planning unit is allocated or not). Also, let
#' \eqn{p} represent the argument to `penalty`, \eqn{D} represent the
#' argument to `data`, and \eqn{W} represent the argument
#' to `zones`.
#'
#' If the argument to `data` is supplied as a `matrix` or
#' `Matrix` object, then the penalties are calculated as:
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z} (-p \times X_{iz}
#' \times X_{jy} \times D_{ij} \times W_{zy})}{
#' sum_i^I sum_j^I sum_z^Z sum_y^Z (-p * Xiz * Xjy * Dij * Wzy)
#' }
#'
#' Otherwise, if the argument to `data` is supplied as a
#' `data.frame` or `array` object, then the penalties are
#' calculated as:
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z} (-p \times X_{iz}
#' \times X_{jy} \times D_{ijzy})}{
#' sum_i^I sum_j^I sum_z^Z sum_y^Z (-p * Xiz * Xjy * Dijzy)
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{-p} is
#' replaced with \eqn{p}.
#'
#' @section Notes:
#' In previous versions, this function aimed to handle both symmetric and
#' asymmetric connectivity data. This meant that the mathematical
#' formulation used to account for asymmetric connectivity was different
#' to that implemented by the *Marxan* software
#' (see Beger *et al.* for details). To ensure that asymmetric connectivity is
#' handled in a similar manner to the *Marxan* software, the
#' [add_asym_connectivity_penalties()] function should now be used for
#' asymmetric connectivity data.
#'
#' @inherit add_boundary_penalties return
#'
#' @seealso
#' See [penalties] for an overview of all functions for adding penalties.
#' Additionally, see [add_asym_connectivity_penalties()] to account for
#' asymmetric connectivity between planning units.
#'
#' @family penalties
#'
#' @encoding UTF-8
#'
#' @references
#' Beger M, Linke S, Watts M, Game E, Treml E, Ball I, and Possingham, HP (2010)
#' Incorporating asymmetric connectivity into spatial decision making for
#' conservation, *Conservation Letters*, 3: 359--368.
#'
#' Önal H, and Briers RA (2002) Incorporating spatial criteria in optimum
#' reserve network selection. *Proceedings of the Royal Society of London.*
#' *Series B: Biological Sciences*, 269: 2437--2441.
#'
#' @examples
#' \dontrun{
#' # load package
#' library(Matrix)
#'
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features_zones <- get_sim_zones_features()
#'
#' # define function to rescale values between zero and one so that we
#' # can compare solutions from different connectivity matrices
#' rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
#'   (x - from[1]) / diff(from) * diff(to) + to[1]
#' }
#'
#' # create basic problem
#' p1 <-
#'   problem(sim_pu_polygons, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create a symmetric connectivity matrix where the connectivity between
#' # two planning units corresponds to their shared boundary length
#' b_matrix <- boundary_matrix(sim_pu_polygons)
#'
#' # standardize matrix values to lay between zero and one
#' b_matrix[] <- rescale(b_matrix[])
#'
#' # visualize connectivity matrix
#' image(b_matrix)
#'
#' # create a symmetric connectivity matrix where the connectivity between
#' # two planning units corresponds to their spatial proximity
#' # i.e., planning units that are further apart share less connectivity
#' centroids <- sf::st_coordinates(
#'   suppressWarnings(sf::st_centroid(sim_pu_polygons))
#' )
#' d_matrix <- (1 / (Matrix::Matrix(as.matrix(dist(centroids))) + 1))
#'
#' # standardize matrix values to lay between zero and one
#' d_matrix[] <- rescale(d_matrix[])
#'
#' # remove connections between planning units without connectivity to
#' # reduce run-time
#' d_matrix[d_matrix < 0.7] <- 0
#'
#' # visualize connectivity matrix
#' image(d_matrix)
#'
#' # create a symmetric connectivity matrix where the connectivity
#' # between adjacent two planning units corresponds to their combined
#' # value in a field in the planning unit attribute data
#' # for example, this field could describe the extent of native vegetation in
#' # each planning unit and we could use connectivity penalties to identify
#' # solutions that cluster planning units together that both contain large
#' # amounts of native vegetation
#' c_matrix <- connectivity_matrix(sim_pu_polygons, "cost")
#'
#' # standardize matrix values to lay between zero and one
#' c_matrix[] <- rescale(c_matrix[])
#'
#' # visualize connectivity matrix
#' image(c_matrix)
#'
#' # create penalties
#' penalties <- c(10, 25)
#'
#' # create problems using the different connectivity matrices and penalties
#' p2 <- list(
#'   p1,
#'   p1 %>% add_connectivity_penalties(penalties[1], data = b_matrix),
#'   p1 %>% add_connectivity_penalties(penalties[2], data = b_matrix),
#'   p1 %>% add_connectivity_penalties(penalties[1], data = d_matrix),
#'   p1 %>% add_connectivity_penalties(penalties[2], data = d_matrix),
#'   p1 %>% add_connectivity_penalties(penalties[1], data = c_matrix),
#'   p1 %>% add_connectivity_penalties(penalties[2], data = c_matrix)
#' )
#'
#' # assign names to the problems
#' names(p2) <- c(
#'   "basic problem",
#'   paste0("b_matrix (", penalties,")"),
#'   paste0("d_matrix (", penalties,")"),
#'   paste0("c_matrix (", penalties,")")
#' )
#'
#' # solve problems
#' s2 <- lapply(p2, solve)
#'
#' # create single object with all solutions
#' s2 <- sf::st_sf(
#'   tibble::tibble(
#'     p2_1 = s2[[1]]$solution_1,
#'     p2_2 = s2[[2]]$solution_1,
#'     p2_3 = s2[[3]]$solution_1,
#'     p2_4 = s2[[4]]$solution_1,
#'     p2_5 = s2[[5]]$solution_1,
#'     p2_6 = s2[[6]]$solution_1,
#'     p2_7 = s2[[7]]$solution_1
#'   ),
#'   geometry = sf::st_geometry(s2[[1]])
#' )
#'
#' # plot solutions
#' plot(s2, main = names(p2))
#'
#' # create minimal multi-zone problem and limit solver to one minute
#' # to obtain solutions in a short period of time
#' p3 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.15, nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(time_limit = 60, verbose = FALSE)
#'
#' # create matrix showing which planning units are adjacent to other units
#' a_matrix <- adjacency_matrix(sim_pu_zones_raster)
#'
#' # visualize matrix
#' image(a_matrix)
#'
#' # create a zone matrix where connectivities are only present between
#' # planning units that are allocated to the same zone
#' zm1 <- as(diag(3), "Matrix")
#'
#' # print zone matrix
#' print(zm1)
#'
#' # create a zone matrix where connectivities are strongest between
#' # planning units allocated to different zones
#' zm2 <- matrix(1, ncol = 3, nrow = 3)
#' diag(zm2) <- 0
#' zm2 <- as(zm2, "Matrix")
#'
#' # print zone matrix
#' print(zm2)
#'
#' # create a zone matrix that indicates that connectivities between planning
#' # units assigned to the same zone are much higher than connectivities
#' # assigned to different zones
#' zm3 <- matrix(0.1, ncol = 3, nrow = 3)
#' diag(zm3) <- 1
#' zm3 <- as(zm3, "Matrix")
#'
#' # print zone matrix
#' print(zm3)
#'
#' # create a zone matrix that indicates that connectivities between planning
#' # units allocated to zone 1 are very high, connectivities between planning
#' # units allocated to zones 1 and 2 are moderately high, and connectivities
#' # planning units allocated to other zones are low
#' zm4 <- matrix(0.1, ncol = 3, nrow = 3)
#' zm4[1, 1] <- 1
#' zm4[1, 2] <- 0.5
#' zm4[2, 1] <- 0.5
#' zm4 <- as(zm4, "Matrix")
#'
#' # print zone matrix
#' print(zm4)
#'
#' # create a zone matrix with strong connectivities between planning units
#' # allocated to the same zone, moderate connectivities between planning
#' # unit allocated to zone 1 and zone 2, and negative connectivities between
#' # planning units allocated to zone 3 and the other two zones
#' zm5 <- matrix(-1, ncol = 3, nrow = 3)
#' zm5[1, 2] <- 0.5
#' zm5[2, 1] <- 0.5
#' diag(zm5) <- 1
#' zm5 <- as(zm5, "Matrix")
#'
#' # print zone matrix
#' print(zm5)
#'
#' # create vector of penalties to use creating problems
#' penalties2 <- c(5, 15)
#'
#' # create multi-zone problems using the adjacent connectivity matrix and
#' # different zone matrices
#' p4 <- list(
#'   p3,
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm1, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm1, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm2, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm2, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm3, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm3, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm4, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm4, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm5, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm5, a_matrix)
#' )
#'
#' # solve problems
#' s4 <- lapply(p4, solve)
#' s4 <- lapply(s4, category_layer)
#' s4 <- terra::rast(s4)
#' names(s4) <-  c(
#'   "basic problem",
#'   paste0("zm", rep(seq_len(5), each = 2), " (", rep(penalties2, 2), ")")
#' )
#'
#' # plot solutions
#' plot(s4, axes = FALSE)
#'
#' # create an array to manually specify the connectivities between
#' # each planning unit when they are allocated to each different zone
#' # for real-world problems, these connectivities would be generated using
#' # data - but here these connectivity values are assigned as random
#' # ones or zeros
#' c_array <- array(0, c(rep(ncell(sim_pu_zones_raster[[1]]), 2), 3, 3))
#' for (z1 in seq_len(3))
#'   for (z2 in seq_len(3))
#'     c_array[, , z1, z2] <- round(
#'       runif(ncell(sim_pu_zones_raster[[1]]) ^ 2, 0, 0.505)
#'     )
#'
#' # create a problem with the manually specified connectivity array
#' # note that the zones argument is set to NULL because the connectivity
#' # data is an array
#' p5 <- list(
#'   p3,
#'   p3 %>% add_connectivity_penalties(15, zones = NULL, c_array)
#' )
#'
#' # solve problems
#' s5 <- lapply(p5, solve)
#' s5 <- lapply(s5, category_layer)
#' s5 <- terra::rast(s5)
#' names(s5) <- c("basic problem", "connectivity array")
#'
#' # plot solutions
#' plot(s5, axes = FALSE)
#' }
#' @name add_connectivity_penalties
#'
#' @exportMethod add_connectivity_penalties
#'
#' @aliases add_connectivity_penalties,ConservationProblem,ANY,ANY,Matrix-method add_connectivity_penalties,ConservationProblem,ANY,ANY,matrix-method add_connectivity_penalties,ConservationProblem,ANY,ANY,dgCMatrix-method add_connectivity_penalties,ConservationProblem,ANY,ANY,data.frame-method add_connectivity_penalties,ConservationProblem,ANY,ANY,array-method
NULL

#' @export
methods::setGeneric("add_connectivity_penalties",
  signature = methods::signature("x", "penalty", "zones", "data"),
  function(x, penalty, zones = diag(number_of_zones(x)), data)
    standardGeneric("add_connectivity_penalties"))

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,matrix}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, penalty, zones, data) {
    add_connectivity_penalties(
      x, penalty, zones, as_Matrix(data, "dgCMatrix")
   )
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,Matrix}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "Matrix"),
  function(x, penalty, zones, data) {
     add_connectivity_penalties(x, penalty, zones,
       as_Matrix(data, "dgCMatrix"))
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,data.frame}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assertthat::assert_that(
      is_conservation_problem(x),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is.data.frame(data)
    )
  # add penalties to problem
  add_connectivity_penalties(
    x, penalty, zones,
    marxan_connectivity_data_to_matrix(x, data, symmetric = TRUE)
  )
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,dgCMatrix}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "dgCMatrix"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assertthat::assert_that(
      is_conservation_problem(x),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is_a_matrix(zones),
      nrow(zones) == ncol(zones),
      is_numeric_values(zones),
      all_finite(zones),
      is_numeric_values(data),
      all_finite(data),
      ncol(data) == nrow(data),
      max(zones) <= 1,
      min(zones) >= -1,
      number_of_total_units(x) == ncol(data),
      number_of_zones(x) == ncol(zones)
    )
    # check for symmetry
    if (!Matrix::isSymmetric(data)) {
      stop(
        paste0(
          "argument to data does not contain symmetric connectivity values, ",
          "use add_asym_connectivity_penalties()"
        )
      )
    }
    # coerce zones to matrix
    zones <- as.matrix(zones)
    indices <- x$planning_unit_indices()
    data <- data[indices, indices, drop = FALSE]
    # convert zones & dgCMatrix data to list of sparse matrices
    m <- list()
    for (z1 in seq_len(ncol(zones))) {
      m[[z1]] <- list()
      for (z2 in seq_len(nrow(zones))) {
        m[[z1]][[z2]] <- data * zones[z1, z2]
      }
    }
    # add penalties
    internal_add_connectivity_penalties(x, penalty, m)
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,array}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "array"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assertthat::assert_that(
      is_conservation_problem(x),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is.null(zones),
      is.array(data),
      length(dim(data)) == 4,
      dim(data)[1] == number_of_total_units(x),
      dim(data)[2] == number_of_total_units(x),
      dim(data)[3] == number_of_zones(x),
      dim(data)[4] == number_of_zones(x),
      all_finite(data)
    )
    # generate indices for units that are planning units
    indices <- x$planning_unit_indices()
    # convert array to list of list of sparseMatrix objects
    m <- list()
    for (z1 in seq_len(dim(data)[3])) {
      m[[z1]] <- list()
      for (z2 in seq_len(dim(data)[4])) {
        m[[z1]][[z2]] <- as_Matrix(data[indices, indices, z1, z2], "dgCMatrix")
      }
    }
    # add penalties
    internal_add_connectivity_penalties (x, penalty, m)
})

internal_add_connectivity_penalties <- function(x, penalty, data) {
  # assert valid arguments
  assertthat::assert_that(
    is_conservation_problem(x),
    assertthat::is.number(penalty),
    all_finite(penalty),
    is.list(data)
  )
  # create new penalty object
  x$add_penalty(pproto(
    "ConnectivityPenalty",
    Penalty,
    name = "Connectivity penalties",
    data = list(data = data),
    parameters = parameters(numeric_parameter("penalty", penalty)),
    apply = function(self, x, y) {
      assertthat::assert_that(
        inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem")
      )
      # coerce to symmetric connectivity data
      cm <- self$get_data("data")
      cm <- lapply(cm, function(x) {
        lapply(x, function(y) as_Matrix(Matrix::tril(y), "dgCMatrix"))
      })
      # apply penalties
      rcpp_apply_connectivity_penalties(
        x$ptr, self$parameters$get("penalty"), cm
      )
      invisible(TRUE)
    }
  ))
}
