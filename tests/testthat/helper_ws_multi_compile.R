helper_ws_multi_compile <- function(multi_obj_list) {
  # prep problems
  for (prob in 1:length(multi_obj_list)) {
    o <- compile(multi_obj_list[[prob]])
    l <- as.list(o)
    multi_obj_list[[prob]] <- l
  }
  
  #objective and modelsense
  new_obj <- multi_obj_list # extract objectives + append
  new_modelsense <-  multi_obj_list # extract modelsenses
  
  # lb, ub, vtype
  
  # sense, rhs
  
  # A
  
  ## make new ws model
  list(
    modelsense = new_modelsense,
    obj = new_obj,
    lb = new_lb,
    ub = new_ub,
    vtype = new_vtype,
    A = new_A,
    rhs = new_rhs,
    sense = new_sense)
  
}

add_ws_approach <- function(multi_obj_list, 
                            obj_weights, 
                            gap = 0.01,
                            rescale = FALSE) {
  # multi_obj_list = multi-objective problem object with multiple models in multi_obj_list$problems (list)
  # obj_weights = numeric vector of weights for each problem's objective
  
  if (length(multi_obj_list) != length(obj_weights)) {
    stop("Number of objective weights must match number of problems")
  }
  
  # normalize the objective weights to sum to 1
  obj_weights <- obj_weights / sum(obj_weights)
  
  # before we do anything else, let's normalise and weight our objectives:
  # get sign for each objective
  signs <- vector("numeric", length(multi_obj_list))
  for (i in seq_along(multi_obj_list)) {
    p <- multi_obj_list[[i]]
    if (p$modelsense == "min") {
      signs[i] <- 1
    } else if (p$modelsense == "max") {
      signs[i] <- -1
    } else {
      stop("Unknown modelsense")
    }
  }
  
  for (i in seq_along(multi_obj_list)) {
    obj <- multi_obj_list[[i]]$obj
    
    if (rescale) {
      # normalize entire objective btw 0 and 1, otherwise not really comparable? use this: x-min/max-min
      rng <- range(obj)
      if (diff(rng) == 0) {
        obj <- rep(0, length(obj))
      } else {
        obj <- (obj - rng[1]) / diff(rng)
      }
      
    }
    # multiply by weight and sign
    obj_weighted <- obj * obj_weights[i] * signs[i]
    # overwrite original objective
    multi_obj_list[[i]]$obj <- obj_weighted
  }
  
  # before combining the objectives, we need to make sure all objectives have the same length
  # and the right variables at the right location:
  # e.g. obj one has cost vals (1-400 and penalties)
  # obj 2 is min shortfall, so all 0s for PUs and then 1s for # features representing shortfall vars
  # obj 3 is max utility, so has summed rep for each PU (1-400)
  
  ## we now need to pad the different obj
  ## split all objectives in planning unit part and non PU part
  # get last pu name
  last_pu <- tail(multi_obj_list[[1]]$pus, 1)
  
  # Find position of that column
  insert_after <- which(colnames(multi_obj_list[[1]]$A) == last_pu)
  rm(last_pu)
  
  # get parts of just pus and then extras
  split_multi_objs <- vector("list", length(multi_obj_list))
  for (i in seq_along(multi_obj_list)) {
    split_multi_objs[[i]] <- split_pu_extra(multi_obj_list[[i]]$obj, insert_after)
  }
  names(split_multi_objs) <- paste0("p", seq_along(split_multi_objs))
  
  # combined PU objective
  combined_pu_obj <- split_multi_objs[[1]]$pu
  if (length(multi_obj_list) > 1) {
    for (i in 2:length(split_multi_objs)) {
      combined_pu_obj <- combined_pu_obj + split_multi_objs[[i]]$pu
    }
  }
  
  # combine extra objective (go in order of problems)
  extras_obj <- c()
  for (i in seq_along(split_multi_objs)) {
    if (!is.null(split_multi_objs[[i]]$extra)) {
      extras_obj <- c(extras_obj, split_multi_objs[[i]]$extra)
    }
  }
  
  # then bind combined and extras
  new_obj <- c(combined_pu_obj, extras_obj)
  rm(split_multi_objs, combined_pu_obj, extras_obj)
  
  # now need to think about:
  ## A
  split_multi_A <- vector("list", length(multi_obj_list))
  for (i in seq_along(multi_obj_list)) {
    split_multi_A[[i]] <- split_pu_extra(multi_obj_list[[i]]$A, insert_after)
  }
  names(split_multi_A) <- paste0("p", seq_along(split_multi_A))
  
  # combined PU A
  combined_pu_A <- do.call(rbind, lapply(split_multi_A, function(x) x$pu))
  
  # now extra
  combined_extra_A <- combine_extras_A(split_multi_A)
  
  # final A
  new_A <- base::cbind(combined_pu_A, combined_extra_A)
  
  ## rhs, sense
  new_rhs <- unlist(lapply(multi_obj_list, function(x) x$rhs))
  new_sense <- unlist(lapply(multi_obj_list, function(x) x$sense))
  
  ## lb, ub, vtype, pus, all_vars
  new_bounds <- check_and_combine_other_old(multi_obj_list, new_obj, insert_after)
  
  new_lb <- new_bounds$lb
  new_ub <- new_bounds$ub
  new_vtype <- new_bounds$vtype
  new_pus <- new_bounds$pus
  new_all_vars <- new_bounds$all_vars
  
  ## other stuff like name, type, vars, modelsense (always set to min)
  new_modelsense <- "min"
  
  new_s_vars <- c()
  for (i in seq_along(multi_obj_list)) {
    new_s_vars <- c(new_s_vars, multi_obj_list[[i]]$s_vars)
  }
  
  ## make new ws model
  list(
    modelsense = new_modelsense,
    obj = new_obj,
    lb = new_lb,
    ub = new_ub,
    vtype = new_vtype,
    A = new_A,
    rhs = new_rhs,
    sense = new_sense,
    name = "weighted sum",
    type = "ws",
    which_obj = unlist(lapply(multi_obj_list, function(x) x$name)),
    pus = new_pus,
    s_vars = new_s_vars,
    all_vars = new_all_vars#,
    #feature_names = feature_names # not sure how do best go about it because we can have duplicate features from the different models
  )
}