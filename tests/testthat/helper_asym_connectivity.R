r_asym_connectivity_given_matrix <- function(solution, zones,
                                             connectivity_matrix) {
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
    ncol(solution) == ncol(zones),
    ncol(solution) == nrow(zones),
    nrow(solution) == ncol(connectivity_matrix),
    nrow(solution) == nrow(connectivity_matrix)
  )
  # initialization
  out <- 0
  # adjust inputs according to planning units
  idx <- which(rowSums(is.finite(solution)) > 0)
  connectivity_matrix <- connectivity_matrix[idx, idx]
  solution <- solution[idx, , drop = FALSE]
  solution[is.na(solution)] <- 0
  # main processing
  for (z1 in seq_len(ncol(zones))) {
    for (z2 in seq_len(nrow(zones))) {
      m <- connectivity_matrix * zones[z1, z2]
      for (i in seq_len(nrow(solution))) {
        for (j in seq_len(nrow(solution))) {
          if ((i == j) && (z1 == z2)) {
            out <- out + unname(m[i, j] * solution[i, z1])
          } else if (i != j) {
            out <- out + unname(m[i, j] * solution[i, z1] * (solution[j, z2]))
          }
        }
      }
    }
  }
  # return result
  out
}

as_connectivity_array <- function(zones, data) {
  # assert arguments are valid
  assertthat::assert_that(
    is.matrix(zones),
    nrow(zones) == ncol(zones),
    inherits(data, c("matrix", "Matrix")),
    nrow(data) == ncol(data))
  # init
  n_z <- nrow(zones)
  n_pu  <- nrow(data)
  # generate array
  out <- array(0, c(n_pu, n_pu, n_z, n_z))
  for (z1 in seq_len(n_z)) {
    for (z2 in seq_len(n_z)) {
      out[, , z1, z2] <- as.matrix(data * zones[z1, z2])
    }
  }
  # return result
  out
}
