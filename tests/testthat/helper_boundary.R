r_boundary_given_matrix <- function(solution, edge_factor, zones,
                                    boundary_matrix) {
  # convert sf solution to Spatial
  if (inherits(solution, "sf")) solution <- sf::as_Spatial(solution)
  # convert Spatial solution to matrix
  if (inherits(solution, "Spatial")) solution <- as.matrix(solution@data)
  # convert data.frame solution to matrix
  if (inherits(solution, "data.frame")) solution <- as.matrix(solution)
  # coerce solution to matrix if not a matrix
  if (!is.matrix(solution)) solution <- matrix(solution, ncol = 1)
  # assert arguments are valid
  assertthat::assert_that(
    ncol(solution) == length(edge_factor),
    ncol(solution) == ncol(zones),
    ncol(solution) == nrow(zones),
    nrow(solution) == ncol(boundary_matrix),
    nrow(solution) == nrow(boundary_matrix)
  )
  # initialization
  c1 <- 0
  c2 <- 0
  c3 <- 0
  # adjust inputs according to planning units
  idx <- which(rowSums(is.finite(solution)) > 0)
  boundary_matrix <- boundary_matrix[idx, idx]
  solution <- solution[idx, , drop = FALSE]
  solution[is.na(solution)] <- 0
  # prepare data for processing
  total_boundary <- Matrix::diag(boundary_matrix)
  exposed_boundary <-
    Matrix::diag(boundary_matrix) -
    (Matrix::rowSums(boundary_matrix) - Matrix::diag(boundary_matrix))
  # main processing
  ## calculate total boundary
  for (z in seq_len(ncol(zones))) {
    for (i in seq_len(nrow(solution))) {
      c1 <-
        c1 + (solution[i, z] * zones[z, z] * (
        (total_boundary[i] - exposed_boundary[i]) +
        (exposed_boundary[i] * edge_factor[z])
      ))
    }
  }

  ## subtract shared boundary lengths
  for (z1 in seq_len(ncol(zones))) {
    for (z2 in seq(z1, nrow(zones))) {
      m <- boundary_matrix * zones[z1, z2]
      Matrix::diag(m) <- 0
      for (i in seq_len(nrow(solution))) {
        for (j in seq(i, nrow(solution))) {
          if (z1 == z2) {
            c2 <- c2 - (2 * m[i, j] * solution[i, z1] * solution[j, z2])
          } else {
            c3 <- c3 - (2 * m[i, j] * solution[i, z1] * solution[j, z2])
            c3 <- c3 - (2 * m[i, j] * solution[i, z2] * solution[j, z1])
          }
        }
      }
    }
  }

  # return result
  sum(c1, c2, c3)
}

r_boundary_given_geometry <- function(solution, sp) {
  # convert sf solution to matrix
  if (inherits(solution, "sf")) solution <- sf::as_Spatial(solution)
  # convert Spatial solution to matrix
  if (inherits(solution, "Spatial")) solution <- as.matrix(solution@data)
  # convert data.frame solution to matrix
  if (inherits(solution, "data.frame")) solution <- as.matrix(solution)
  # coerce solution to matrix if not a matrix
  if (!is.matrix(solution)) solution <- matrix(solution, ncol = 1)
  # assert that solution contains binary values
  assertthat::assert_that(all_binary(solution))
  # filter planning units
  solution <- rowSums(solution, na.rm = TRUE) > 1e-15
  # return boundary
  sp2 <- sf::st_union(sf::st_as_sf(sp[which(solution), ]))
  sp2 <- sf::st_cast(sp2, "MULTILINESTRING")
  sum(sf::st_length(sp2))
}
