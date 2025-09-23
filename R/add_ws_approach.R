add_ws_approach <- function(multi_obj_list, obj_weights, rescale_weights = FALSE) {
  assertthat::assert_that(is.list(multi_obj_list))
  assertthat::assert_that(length(multi_obj_list) == length(obj_weights))
  
  # compile each problem
  compiled_list <- lapply(multi_obj_list, compile)
  
  # extract pointers 
  problems_ptrs <- lapply(compiled_list, function(p) p$ptr)
  
  # apply ws approach on my probs
  ws_out <- rcpp_apply_ws_approach(problems_ptrs, obj_weights, rescale_weights)
  
  ws_p1 <- OptimizationProblem$new(ptr = ws_out)
  #test8 <- as.list(ws_p1)
  
}