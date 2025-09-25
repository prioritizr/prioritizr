add_hierarchical_approach <- function(multi_obj_list, degradation_vec) {
  assertthat::assert_that(is.list(multi_obj_list))
  assertthat::assert_that(length(multi_obj_list) - 1 == length(degradation_vec))
  
  # compile each problem
  compiled_list <- lapply(multi_obj_list, compile)
  
  
  # Get PU and zone counts from the first problem
  n_pu <- compiled_list[[1]]$number_of_planning_units()
  n_zone <- compiled_list[[1]]$number_of_zones()
  
  # Check consistency across all problems: do this here and not in c++
  for (i in 1:length(compiled_list)) {
    if (compiled_list[[i]]$number_of_planning_units() != n_pu) {
      stop(sprintf("Mismatch in number_of_planning_units at problem %d", i))
    }
    if (compiled_list[[i]]$number_of_zones() != n_zone) {
      stop(sprintf("Mismatch in number_of_zones at problem %d", i))
    }
  }
  
  message("All problems have consistent planning units and zones: ", n_pu, " PUs, ", n_zone, " zones.")
  
  # extract pointers 
  problems_ptrs <- lapply(compiled_list, function(p) p$ptr)
  
  n_probs <- length(problems_ptrs)
  
  #init stuff
  results <- vector("list", length(compiled_list))
  pu_len <- NULL
  
  for (k in 1:length(compiled_list)) {
    current_prob <- problems_ptrs[[k]]
    
    if (k > 1) {
      #load right stuff
      prev_prob <- problems_ptrs[[k-1]]
      prev_sol <- results[[k-1]]$solution
      d <- degradation_vec[k-1]
      
      current_out <- rcpp_apply_hierachical_approach(
        current_ptr = current_prob,
        prev_ptr = prev_prob,
        prev_solution = prev_sol,
        degradation = d
      )
    } else {
      current_out <- current_prob
    }
    
    # Solve manually for now
    p_solve <- OptimizationProblem$new(ptr = current_out)
    
    model_p <- list(
      A = p_solve$A(),
      obj = p_solve$obj(),
      modelsense = p_solve$modelsense(),
      rhs = p_solve$rhs(),
      sense = p_solve$sense(),
      vtype = p_solve$vtype(),
      lb = p_solve$lb(),
      ub = p_solve$ub()
    )

    params <- list(OutputFlag = 0)
    result <- gurobi(model_p, params)
    
    results[[k]] <- list(
      solution = result$x,
      model = model_p
    )
  }
  
  return(results)
  
  
}