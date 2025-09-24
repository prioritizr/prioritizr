#include "package.h"
#include "optimization_problem.h"
#include "moo-function.h" 
#include <numeric>
#include <string>
#include <iostream>

// [[Rcpp::export]]
SEXP rcpp_apply_ws_approach(const Rcpp::List problems_ptrs,
                            Rcpp::NumericVector obj_weights,
                            bool rescale_weights = false) {
  
  const std::size_t n_probs = problems_ptrs.size();
  if (n_probs == 0) Rcpp::stop("problems_ptrs is empty.");
  if (n_probs != static_cast<std::size_t>(obj_weights.size()))
    Rcpp::stop("Number of weights must match number of problems.");
  
  // consistency check across all problems 
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> p0 =
    Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[0]);
  const std::size_t n_pu   = p0->_number_of_planning_units;
  const std::size_t n_zone = p0->_number_of_zones;
  
  for (std::size_t i = 1; i < n_probs; ++i) {
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> pi =
      Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[i]);
    
    if (pi->_number_of_planning_units != n_pu)
      Rcpp::stop("Mismatch in number_of_planning_units between problems.");
    if (pi->_number_of_zones != n_zone)
      Rcpp::stop("Mismatch in number_of_zones between problems.");
  }
  
  // create empty prioirtizr optimization problem to fill stuff into
  OPTIMIZATIONPROBLEM* x = new OPTIMIZATIONPROBLEM(
    std::string(""),                  // modelsense empty
    0,                                // number_of_features empty
    n_pu,                             // number_of_planning_units from first problem
    n_zone,                           // number_of_zones from first problem
    std::vector<std::size_t>(),       // A_i empty
    std::vector<std::size_t>(),       // A_j empty
    std::vector<double>(),            // A_x empty
    std::vector<double>(),            // obj empty
    std::vector<double>(),            // lb empty
    std::vector<double>(),            // ub empty
    std::vector<double>(),            // rhs empty
    std::vector<std::string>(),       // sense empty
    std::vector<std::string>(),       // vtype empty
    std::vector<std::string>(),       // row_ids empty
    std::vector<std::string>(),       // col_ids empty
    true                              // compressed_formulation
  );
  
  // Now start actual calcs
  
  // Normalize weights
  double total_w = std::accumulate(obj_weights.begin(), obj_weights.end(), 0.0);
  if (!R_finite(total_w) || total_w <= 0.0)
    Rcpp::stop("Objective weights must sum to a positive finite value.");
  for (std::size_t i = 0; i < static_cast<std::size_t>(obj_weights.size()); ++i)
    obj_weights[i] /= total_w;
  
  // Signs vector: min = +1, max = -1
  std::vector<int> signs(n_probs); // +1 = min, -1 = max
  for (std::size_t i = 0; i < n_probs; ++i) {
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> p =
      Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[i]);
    const std::string ms = p->_modelsense;
    
    if (ms == "min") signs[i] = +1;
    else if (ms == "max") signs[i] = -1;
    else Rcpp::stop("Unknown modelsense in problem ", i + 1);
  }
  
  // after determining signs[i], we now want to rescale and weight objectives
  std::vector< std::vector<double> > obj_list(n_probs);
  
  for (std::size_t i = 0; i < n_probs; ++i) {
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> p = 
      Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[i]);
    
    // extract objectives
    std::vector<double> obj = p->_obj;
    Rcpp::Rcout << "Problem " << i+1 
    << " original objective length: " << obj.size() << std::endl;
    
    Rcpp::Rcout << "Problem " << i+1 << " original objective values (first 5): ";
    for (std::size_t j = 0; j < obj.size(); ++j) {
      Rcpp::Rcout << obj[j] << " ";
    }
    Rcpp::Rcout << std::endl;
    
    if (rescale_weights) {
      double min_val = *std::min_element(obj.begin(), obj.end());
      double max_val = *std::max_element(obj.begin(), obj.end());
      double range = max_val - min_val;
      
      if (range == 0) {
        std::fill(obj.begin(), obj.end(), 0.0);
        Rcpp::Rcout << "Problem " << i+1 << " objective constant, rescaled to all zeros" << std::endl;
      } else {
        for (auto &val : obj) {
          val = (val - min_val) / range;
        }
        Rcpp::Rcout << "Problem " << i+1 
        << " objective rescaled to [0,1]" << std::endl;
      }
    }
    
    Rcpp::Rcout << "Problem " << i+1 << " still original objective values (first 5): ";
    for (std::size_t j = 0; j < obj.size(); ++j) {
      Rcpp::Rcout << obj[j] << " ";
    }
    Rcpp::Rcout << std::endl;
    
    // multiply by weight and sign
    for (std::size_t j = 0; j < obj.size(); ++j) {
      obj[j] = obj[j] * obj_weights[i] * signs[i];
    }
    Rcpp::Rcout << "Problem " << i+1 
    << " weighted by " << obj_weights[i] 
    << " and sign " << signs[i] << std::endl;
    
    // DEBUG: print the weighted objective
    Rcpp::Rcout << "Problem " << i+1 << " weighted objective:" << std::endl;
    for (std::size_t j = 0; j < obj.size(); ++j) {
      Rcpp::Rcout << obj[j] << " ";
    }
    Rcpp::Rcout << std::endl;
    
    // overwrite original objective coefficients in problem for later use
    p->_obj = obj;
    
    // save individual weighted objective for later use outside
    obj_list[i] = obj;
  }
  
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> first_problem =
    Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[0]);
  std::vector<std::string> col_ids = first_problem->_col_ids;
  
  // get location of my last pu (can delete once I changed triplet bit)
  // std::size_t insert_after = 0;
  // for (std::size_t j = 0; j < col_ids.size(); ++j) {
  //   if (col_ids[j] == "pu") {
  //     insert_after = j; 
  //   }
  // }
  
  //   std::size_t num_pus = insert_after + 1; 
  //   Rcpp::Rcout << "Position of last PU variable in problem 1: "
  //               << num_pus << std::endl;
  //   
    // // Split objectives into PU vs extra (vector-only)
  // std::vector<std::vector<double>> pu_parts(n_probs);
  // std::vector<std::vector<double>> extra_parts(n_probs);
  // 
    // for (std::size_t i = 0; i < n_probs; ++i) {
      //   auto split = split_pu_extra_vec(obj_list[i], num_pus);
      //   pu_parts[i] = split.first;
      //   extra_parts[i] = split.second;
      //   
        // }
  // 
    // // Combine PU and extra objectives across problems
  // 
    // // Sum PU parts
  // std::vector<double> combined_pu_obj = pu_parts[0];
  // for (std::size_t i = 1; i < n_probs; ++i) {
    //   if (pu_parts[i].size() != combined_pu_obj.size())
      //     Rcpp::stop("Mismatch in PU lengths between problems.");
    //   for (std::size_t j = 0; j < combined_pu_obj.size(); ++j)
      //     combined_pu_obj[j] += pu_parts[i][j];
      // }
  // 
    // // Concatenate extra parts
  // std::vector<double> extras_obj;
  // for (std::size_t i = 0; i < n_probs; ++i) {
    //   extras_obj.insert(extras_obj.end(),
                           //                     extra_parts[i].begin(),
                           //                     extra_parts[i].end());
    // }
  // 
    // // Combine PU + extras into new objective
  // x->_obj.reserve(combined_pu_obj.size() + extras_obj.size());
  // x->_obj.insert(x->_obj.end(), combined_pu_obj.begin(), combined_pu_obj.end());
  // x->_obj.insert(x->_obj.end(), extras_obj.begin(), extras_obj.end());
  
  // Rcpp::XPtr<OPTIMIZATIONPROBLEM> first_problem =
    //   Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[0]);
  //   
    // determine PU length the same way as lb/ub/vtype
  std::size_t pu_len = n_pu * n_zone;
  
  Rcpp::Rcout << "PU length from first problem: "
  << pu_len << std::endl;
  
  // initialize combined PU objectives with first problem’s PU part
  x->_obj = first_problem->_obj;
  if (x->_obj.size() > pu_len)
    x->_obj.resize(pu_len); // keep only PU part for now
  
  // accumulate PU parts and concatenate extras directly
  for (std::size_t i = 1; i < n_probs; ++i) {
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> p =
      Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[i]);
    
    // check length
    if (p->_obj.size() < pu_len)
      Rcpp::stop("Mismatch in PU length.");
    
    // add PU coefficients
    for (std::size_t j = 0; j < pu_len; ++j) {
      x->_obj[j] += p->_obj[j];
    }
    
    // append extras
    if (p->_obj.size() > pu_len) {
      x->_obj.insert(
        x->_obj.end(),
        p->_obj.begin() + pu_len,
        p->_obj.end()
      );
    }
  }
  
  // also append extras from first_problem
  if (first_problem->_obj.size() > pu_len) {
    x->_obj.insert(
      x->_obj.end(),
      first_problem->_obj.begin() + pu_len,
      first_problem->_obj.end()
    );
  }
  
  
  // Now think about A
  // Split sparse A matrices
  // std::vector<TripletSplit> split_multi_A(n_probs);
  //
  // for (std::size_t i = 0; i < n_probs; ++i) {
  //   Rcpp::XPtr<OPTIMIZATIONPROBLEM> p =
  //     Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[i]);
  //
  //   Triplet t;
  //   t.i.resize(p->_A_i.size());
  //   std::transform(p->_A_i.begin(), p->_A_i.end(), t.i.begin(),
  //                  [](std::size_t v){ return static_cast<int>(v); });
  //   t.j.resize(p->_A_j.size());
  //   std::transform(p->_A_j.begin(), p->_A_j.end(), t.j.begin(),
  //                  [](std::size_t v){ return static_cast<int>(v); });
  //   t.x = p->_A_x;
  //   t.dims = std::make_pair((int)p->_row_ids.size(), (int)p->_col_ids.size());
  //
  //   split_multi_A[i] = split_pu_extra_triplet(t, insert_after);
  // }
  //
  // // Combine PU triplets
  // std::vector<Triplet> pu_triplets(n_probs);
  // for (std::size_t i = 0; i < n_probs; ++i) {
  //   pu_triplets[i] = split_multi_A[i].pu_trip;
  // }
  // Triplet combined_pu_A = rbind_triplets(pu_triplets);
  //
  // Rcpp::Rcout << "Combined PU triplet has "
  // << combined_pu_A.i.size() << " non-zeros, "
  // << "dims=(" << combined_pu_A.dims.first
  // << "," << combined_pu_A.dims.second << ")" << std::endl;
  //
  // // Combine extras
  // Triplet combined_extra_A = combine_extras_A_trip(split_multi_A);
  //
  // // cbind both combined_A for our new A
  // Triplet new_A = cbind_triplets({combined_pu_A, combined_extra_A});
  //
  // // fill in our dummy problem
  // x->_A_i.resize(new_A.i.size());
  // std::transform(new_A.i.begin(), new_A.i.end(),
  //                x->_A_i.begin(),
  //                [](int v){ return static_cast<std::size_t>(v); });
  //
  // x->_A_j.resize(new_A.j.size());
  // std::transform(new_A.j.begin(), new_A.j.end(),
  //                x->_A_j.begin(),
  //                [](int v){ return static_cast<std::size_t>(v); });
  //
  // // Copy values directly
  // x->_A_x = new_A.x;
  //
  // // Also update row_ids and col_ids if you want a human-readable problem
  // // For now, fill with dummy names if nothing else available
  // x->_row_ids.resize(new_A.dims.first);
  // for (int r = 0; r < new_A.dims.first; ++r) {
  //   x->_row_ids[r] = "row_" + std::to_string(r + 1);
  // }
  //
  // x->_col_ids.resize(new_A.dims.second);
  // for (int c = 0; c < new_A.dims.second; ++c) {
  //   x->_col_ids[c] = "col_" + std::to_string(c + 1);
  // }

  int row_offset = 0;         // offset to shift row indices when stacking rows from multiple problems
  int extra_col_offset = 0;   // offset to shift extra column indices so they don’t overlap
  
  for (std::size_t pidx = 0; pidx < n_probs; ++pidx) {     
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> p =
      Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[pidx]);  
    
    int n_extra_cols = static_cast<int>(p->_col_ids.size()) - pu_len;  // cal # of extra cols beyond PU cols
    
    for (std::size_t k = 0; k < p->_A_i.size(); ++k) {  
      int gi = static_cast<int>(p->_A_i[k]) + row_offset; // shift  row index by current row_offset for stacking
      int gj = static_cast<int>(p->_A_j[k]);              // get original col index
      double val = p->_A_x[k];                            // get  corresponding x val
      
      if (gj < pu_len) {  // if PU col
        x->_A_i.push_back(gi);   // add shifted row index to combined matrix
        x->_A_j.push_back(gj);   // add PU col index without changes
        x->_A_x.push_back(val);  // add val
      } else {               // else, its extra col
        x->_A_i.push_back(gi);  // row index still shifted by row_offset
        x->_A_j.push_back(pu_len + extra_col_offset + (gj - pu_len)); // shift extra col by current offset to avoid overlap
        x->_A_x.push_back(val); // add x
      }
    }
    
    row_offset += static_cast<int>(p->_row_ids.size()); // increase row_offset by number of rows in prob
    extra_col_offset += n_extra_cols;                  // increase extra column offset for next prob
  }
  
  // set row ids for combined prob
  x->_row_ids.resize(row_offset);                    // resize row_ids vector to total number of rows
  for (int r = 0; r < row_offset; ++r)               
    x->_row_ids[r] = "row_" + std::to_string(r + 1); 
  
  // set column ids for the combined problem
  x->_col_ids.resize(pu_len + extra_col_offset);   // resize col ids vector to total columns (PU + extras)
  for (int c = 0; c < pu_len + extra_col_offset; ++c) 
    x->_col_ids[c] = "col_" + std::to_string(c + 1);  
  
  // debug print the final dimensions of the combined A matrix
  Rcpp::Rcout << "Final new_A dims: " << row_offset
              << ", " << pu_len + extra_col_offset
              << ", nnz=" << x->_A_x.size() << std::endl;
  
  // now rhs and sense
  for (std::size_t i = 0; i < n_probs; ++i) {
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> p =
      Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[i]);
    
    // Append rhs
    x->_rhs.insert(x->_rhs.end(), p->_rhs.begin(), p->_rhs.end());
    
    // Append sense
    x->_sense.insert(x->_sense.end(), p->_sense.begin(), p->_sense.end());
  }
  
  // Debugging
  Rcpp::Rcout << "Combined rhs length: " << x->_rhs.size() << std::endl;
  Rcpp::Rcout << "Combined sense length: " << x->_sense.size() << std::endl;
  
  //lb, ub, vtype, pus, all_vars
  std::vector<OPTIMIZATIONPROBLEM*> problem_ptrs(n_probs);
  for (std::size_t i = 0; i < n_probs; ++i) {
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> p =
      Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[i]);
    problem_ptrs[i] = p.get();
  }
  
  x->_lb = problem_ptrs[0]->_lb;
  x->_ub = problem_ptrs[0]->_ub;
  x->_vtype = problem_ptrs[0]->_vtype;
  x->_col_ids = problem_ptrs[0]->_col_ids;
  
  for (size_t i = 1; i < n_probs; ++i) {
    OPTIMIZATIONPROBLEM* p = problem_ptrs[i];
    
    // Determine PU length
    //size_t pu_len = n_pu * n_zone;;
    
    // check consistency of PU part
    for (size_t j = 0; j < pu_len; ++j) {
      if (p->_lb[j] != x->_lb[j]) Rcpp::stop("Inconsistent lb PU across problems");
      if (p->_ub[j] != x->_ub[j]) Rcpp::stop("Inconsistent ub PU across problems");
      if (p->_vtype[j] != x->_vtype[j]) Rcpp::stop("Inconsistent vtype PU across problems");
    }
    
    // Append extras directly into dummy problem
    x->_lb.insert(x->_lb.end(), p->_lb.begin() + pu_len, p->_lb.end());
    x->_ub.insert(x->_ub.end(), p->_ub.begin() + pu_len, p->_ub.end());
    x->_vtype.insert(x->_vtype.end(), p->_vtype.begin() + pu_len, p->_vtype.end());
    x->_col_ids.insert(x->_col_ids.end(), p->_col_ids.begin() + pu_len, p->_col_ids.end());
  }
  
  // other stuff like type, vars, modelsense (always set to min)
  x->_modelsense="min";
  // Combine row_ids across problems
  x->_row_ids.clear();
  std::size_t new_number_of_features = 0;
  for (std::size_t i = 0; i < n_probs; ++i) {
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> p = 
      Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(problems_ptrs[i]);
    
    // Append all row_ids from this problem
    x->_row_ids.insert(x->_row_ids.end(),
                       p->_row_ids.begin(),
                       p->_row_ids.end());
    
    // Sum number_of_features
    new_number_of_features += p->_number_of_features;
  }
  
  // Store total number of features in dummy problem
  x->_number_of_features = new_number_of_features;
  
  // Debug
  Rcpp::Rcout << "Combined number of features: " << x->_number_of_features << std::endl;
  Rcpp::Rcout << "Combined row_ids length: " << x->_row_ids.size() << std::endl;

  // Returns
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::XPtr<OPTIMIZATIONPROBLEM>(x,
                                                                        true);
  return(ptr);
  
}