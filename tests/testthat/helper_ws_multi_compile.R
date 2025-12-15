helper_ws_multi_compile <- function(multi_obj_list) {
  # prep problems
  for (prob in 1:length(multi_obj_list)) {
    o <- compile(multi_obj_list[[prob]])
    l <- as.list(o)
    multi_obj_list[[prob]] <- l
  }
  
  opt <- multi_obj_list
  n   <- length(opt)
  
  # counters
  n_pu    <- opt[[1]]$number_of_planning_units
  n_zone  <- opt[[1]]$number_of_zones
  n_status <- n_pu * n_zone
  
  # init stuff
  opt_n_ncol     <- vapply(opt, function(x) length(x$obj), integer(1))
  opt_n_nrow     <- vapply(opt, function(x) length(x$rhs), integer(1))
  opt_n_A        <- vapply(opt, function(x) length(x$A_i), integer(1))
  opt_n_features <- vapply(opt, function(x) x$number_of_features, double(1))
  
  # offsets
  opt_row_offset <- c(0, cumsum(opt_n_nrow[-n]))
  opt_col_offset <- c(0, cumsum(opt_n_ncol[-n] - n_status))
  opt_A_offset   <- c(0, cumsum(opt_n_A[-n]))
  
  # multi-obj dims
  mopt_ncol <- sum(opt_n_ncol) - (n - 1) * n_status
  mopt_nrow <- sum(opt_n_nrow)
  mopt_n_A  <- sum(opt_n_A)
  
  # obj
  obj <- matrix(0, nrow = n, ncol = mopt_ncol)
  
  for (i in seq_len(n)) {
    ## planning unit status variables
    obj[i, seq_len(n_status)] <- obj[i, seq_len(n_status)] +
      opt[[i]]$obj[seq_len(n_status)]
    
    ## extra variables
    if (opt_n_ncol[i] > n_status) {
      idx <- (n_status + 1):opt_n_ncol[i]
      obj[i, idx + opt_col_offset[i]] <- opt[[i]]$obj[idx]
    }
  }
  
  # modelsense
  modelsense <- vapply(opt, `[[`, character(1), "modelsense")

  # lb, ub
  lb <- numeric(mopt_ncol)
  ub <- numeric(mopt_ncol)
  vtype <- character(mopt_ncol)
  col_ids <- character(mopt_ncol)
  
  lb[seq_along(opt[[1]]$lb)] <- opt[[1]]$lb
  ub[seq_along(opt[[1]]$ub)] <- opt[[1]]$ub
  vtype[seq_along(opt[[1]]$vtype)] <- opt[[1]]$vtype
  col_ids[seq_along(opt[[1]]$col_ids)] <- opt[[1]]$col_ids
  
  for (i in 2:n) {
    if (opt_n_ncol[i] > n_status) {
      idx <- (n_status + 1):opt_n_ncol[i]
      lb[idx + opt_col_offset[i]] <- opt[[i]]$lb[idx]
      ub[idx + opt_col_offset[i]] <- opt[[i]]$ub[idx]
      vtype[idx + opt_col_offset[i]]   <- opt[[i]]$vtype[idx]
      col_ids[idx + opt_col_offset[i]] <- opt[[i]]$col_ids[idx]
    }
  }
  
  # sense, rhs
  rhs <- unlist(lapply(opt, `[[`, "rhs"))
  sense <- unlist(lapply(opt, `[[`, "sense"))
  row_ids <- unlist(lapply(opt, `[[`, "row_ids"))
  
  # A
  A_i <- integer(mopt_n_A)
  A_j <- integer(mopt_n_A)
  A_x <- numeric(mopt_n_A)
  
  for (i in seq_len(n)) {
    
    if (opt_n_A[i] == 0L) next
    
    a_idx <- seq_len(opt_n_A[i]) + opt_A_offset[i]
    
    ## row indices
    A_i[a_idx] <- opt[[i]]$A_i + opt_row_offset[i] + 1
    
    ## column indices
    A_j[a_idx] <- opt[[i]]$A_j +
      opt_col_offset[i] * (opt[[i]]$A_j >= n_status) + 1
    
    #vals
    A_x[a_idx] <- opt[[i]]$A_x
  }
  
  A = Matrix::sparseMatrix(
    i = A_i,
    j = A_j,
    x = A_x,
    dims = c(length(row_ids), length(col_ids))
  )
  
  ## make new model
  list(
    modelsense = modelsense,
    obj = obj,
    lb = lb,
    ub = ub,
    vtype = vtype,
    A = A,
    rhs = rhs,
    sense = sense)
  
}
