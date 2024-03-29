#' @include internal.R ConservationProblem-class.R
NULL

#' Evaluate connectivity of solution
#'
#' Calculate the connectivity held within a solution to a conservation
#' planning problem.
#' This summary statistic evaluates the connectivity of a solution using
#' pair-wise connectivity values between combinations of planning units.
#' It is specifically designed for symmetric connectivity data.
#'
#' @inheritParams add_connectivity_penalties
#' @inheritParams eval_cost_summary
#'
#' @details
#' This summary statistic is comparable to the `Connectivity_In` metric
#' reported by the
#' [*Marxan* software](https://marxansolutions.org) (Ball *et al.* 2009).
#' It is calculated using the same equations used to penalize solutions
#' with connectivity data (i.e., [add_connectivity_penalties()]).
#' Specifically, it is calculated as the sum of the pair-wise connectivity
#' values in the argument to `data`, weighted by the value of the planning
#' units in the solution.
#'
#' @inheritSection eval_cost_summary Solution format
#' @inheritSection add_connectivity_penalties Data format
#'
#' @return
#'  A [tibble::tibble()] object describing the connectivity of the
#'  solution.
#'  It contains the following columns:
#'
#'   \describe{
#'
#'   \item{summary}{`character` description of the summary statistic.
#'     The statistic associated with the `"overall"` value
#'     in this column is calculated using the entire solution
#'     (including all management zones if there are multiple zones).
#'     If multiple management zones are present, then summary statistics
#'     are also provided for each zone separately
#'     (indicated using zone names).}
#'
#'   \item{connectivity}{`numeric` connectivity value.
#'     Greater values correspond to solutions associated with greater
#'     connectivity.
#'     Thus conservation planning exercises typically prefer solutions
#'     with greater values.}
#'
#'   }
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' @seealso
#' See [summaries] for an overview of all functions for summarizing solutions.
#' Also, see [add_connectivity_penalties()] to penalize solutions with low
#' connectivity.
#'
#' @family summaries
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # build minimal conservation problem with raster data
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s1 <- solve(p1)
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # simulate a connectivity matrix to describe the relative strength
#' # of connectivity between different planning units
#' # for brevity, we will use cost data here so that pairs
#' # of adjacent planning units with higher cost values will have a
#' # higher connectivity value
#' # (but see ?connectivity_matrix for more information)
#' cm1 <- connectivity_matrix(sim_pu_raster, sim_pu_raster)
#'
#' # calculate connectivity associated with the solution
#' r1 <- eval_connectivity_summary(p1, s1, data = cm1)
#' print(r1)
#'
#' # build multi-zone conservation problem with polygon data
#' p2 <-
#'   problem(
#'     sim_zones_pu_polygons, sim_zones_features,
#'     cost_column = c("cost_1", "cost_2", "cost_3")
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s2$solution <- category_vector(
#'   s2[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' )
#' s2$solution <- factor(s2$solution)
#'
#' # plot solution
#' plot(s2[, "solution"])
#'
#' # simulate connectivity matrix
#' # here, we will add a new column to sim_zones_pu_polygons with
#' # randomly simulated values and create a connectivity matrix
#' # based on the average simulated values of adjacent planning units
#' sim_zones_pu_polygons$con <- runif(nrow(sim_zones_pu_polygons))
#' cm2 <- connectivity_matrix(sim_zones_pu_polygons, "con")
#'
#' # calculate connectivity associated with the solution
#' r2 <- eval_connectivity_summary(
#'   p2, s2[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")],
#'   data = cm2
#' )
#' print(r2)
#'
#' }
#' @name eval_connectivity_summary
#'
#' @exportMethod eval_connectivity_summary
#'
#' @aliases eval_connectivity_summary,ConservationProblem,ANY,ANY,Matrix-method eval_connectivity_summary,ConservationProblem,ANY,ANY,matrix-method eval_connectivity_summary,ConservationProblem,ANY,ANY,dgCMatrix-method eval_connectivity_summary,ConservationProblem,ANY,ANY,data.frame-method eval_connectivity_summary,ConservationProblem,ANY,ANY,array-method
NULL

#' @export
methods::setGeneric("eval_connectivity_summary",
  signature = methods::signature("x", "solution", "zones", "data"),
  function(x, solution, zones = diag(number_of_zones(x)), data) {
    assert_required(x)
    assert_required(solution)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(
        data,
        c("matrix", "Matrix", "dgCMatrix", "data.frame", "array")
      )
    )
    standardGeneric("eval_connectivity_summary")
  }
)

#' @name eval_connectivity_summary
#' @usage \S4method{eval_connectivity_summary}{ConservationProblem,ANY,ANY,matrix}(x, solution, zones, data)
#' @rdname eval_connectivity_summary
methods::setMethod("eval_connectivity_summary",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, solution, zones, data) {
    eval_connectivity_summary(x, solution, zones, as_Matrix(data, "dgCMatrix"))
  }
)

#' @name eval_connectivity_summary
#' @usage \S4method{eval_connectivity_summary}{ConservationProblem,ANY,ANY,Matrix}(x, solution, zones, data)
#' @rdname eval_connectivity_summary
methods::setMethod("eval_connectivity_summary",
  methods::signature("ConservationProblem", "ANY", "ANY", "Matrix"),
  function(x, solution, zones, data) {
    eval_connectivity_summary(x, solution, zones, as_Matrix(data, "dgCMatrix"))
  }
)

#' @name eval_connectivity_summary
#' @usage \S4method{eval_connectivity_summary}{ConservationProblem,ANY,ANY,data.frame}(x, solution, zones, data)
#' @rdname eval_connectivity_summary
methods::setMethod("eval_connectivity_summary",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, solution, zones, data) {
    eval_connectivity_summary(
      x, solution, zones, marxan_connectivity_data_to_matrix(x, data, TRUE)
    )
  }
)

#' @name eval_connectivity_summary
#' @usage \S4method{eval_connectivity_summary}{ConservationProblem,ANY,ANY,dgCMatrix}(x, solution, zones, data)
#' @rdname eval_connectivity_summary
methods::setMethod("eval_connectivity_summary",
  methods::signature("ConservationProblem", "ANY", "ANY", "dgCMatrix"),
  function(x, solution, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_matrix_ish(zones),
      nrow(zones) == ncol(zones),
      is_numeric_values(zones),
      all_finite(zones),
      max(zones) <= 1,
      min(zones) >= -1,
      is_numeric_values(data),
      all_finite(data),
      ncol(data) == nrow(data),
      number_of_total_units(x) == ncol(data),
      number_of_zones(x) == ncol(zones)
    )
    # coerce zones to matrix
    zones <- as.matrix(zones)
    indices <- x$planning_unit_indices()
    data <- data[indices, indices, drop = FALSE]
    # check for symmetry
    if (!Matrix::isSymmetric(data)) {
      cli_warning(
        paste0(
          "{.arg data} contains asymmetric connectivity values; ",
          "it it recommended to use {.fn eval_asym_connectivity_summary}."
        )
      )
    }
    # convert zones & dgCMatrix data to list of sparse matrices
    m <- list()
    for (z1 in seq_len(ncol(zones))) {
      m[[z1]] <- list()
      for (z2 in seq_len(nrow(zones))) {
        m[[z1]][[z2]] <- data * zones[z1, z2]
      }
    }
    # calculate connectivity
    internal_eval_connectivity_summary(
      x, planning_unit_solution_status(x, solution), m, data
    )
  }
)

#' @name eval_connectivity_summary
#' @usage \S4method{eval_connectivity_summary}{ConservationProblem,ANY,ANY,array}(x, solution, zones, data)
#' @rdname eval_connectivity_summary
methods::setMethod("eval_connectivity_summary",
  methods::signature("ConservationProblem", "ANY", "ANY", "array"),
  function(x, solution, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is.null(zones),
      is.array(data),
      all_finite(data),
      length(dim(data)) == 4,
      dim(data)[1] == number_of_total_units(x),
      dim(data)[2] == number_of_total_units(x),
      dim(data)[3] == number_of_zones(x),
      dim(data)[4] == number_of_zones(x)
    )
    # generate indices for units that are planning units
    indices <- x$planning_unit_indices()
    # convert array to list of list of sparseMatrix objects
    m <- list()
    for (z1 in seq_len(dim(data)[3])) {
      m[[z1]] <- list()
      for (z2 in seq_len(dim(data)[4])) {
        m[[z1]][[z2]] <- as_Matrix(
          data[indices, indices, z1, z2],
          "dgCMatrix"
        )
      }
    }
    # calculate connectivity
    internal_eval_connectivity_summary(
      x, planning_unit_solution_status(x, solution), m, NULL
    )
  }
)

internal_eval_connectivity_summary <- function(x, solution, zone_scaled_data,
                                               data) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    is.matrix(solution),
    is.list(zone_scaled_data),
    is_inherits(data, c("dgCMatrix", "NULL"))
  )
  # manually coerce NA values in solution to 0
  solution[!is.finite(solution)] <- 0
  # calculate overall connectivity
  v <- rcpp_connectivity(zone_scaled_data, solution)
  # main calculations
  if (number_of_zones(x) == 1) {
    ## store result for single zone
    out <- tibble::tibble(summary = "overall", connectivity = v)
  } else {
    ## calculate connectivity for each zone separately
    zv <- vapply(seq_len(ncol(solution)), FUN.VALUE = numeric(1), function(z) {
      ## prepare data the z'th zone
      if (is.null(data)) {
        zd <- as_Matrix(zone_scaled_data[[z]][[z]], "dgCMatrix")
      } else {
        zd <- data
      }
      ## calculate connectivity
      rcpp_connectivity(list(list(zd)), solution[, z, drop = FALSE])
    })
    ## store results for multiple zones
    out <- tibble::tibble(
      summary = c("overall", zone_names(x)),
      connectivity = c(v, zv)
    )
  }
  # return result
  out
}
