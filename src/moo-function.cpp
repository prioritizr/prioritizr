#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include "moo-function.h"
#include <unordered_map>
#include <iostream>

// ========================================================
  // Helper: Split PU vs Extra (vector case)
// ========================================================
  std::pair<std::vector<double>, std::vector<double>>
  split_pu_extra_vec(const std::vector<double>& x, std::size_t insert_after) {
    std::size_t n = x.size();
    
    // PU part: first `insert_after` elements
    std::vector<double> pu(x.begin(), x.begin() + std::min(insert_after, n));
    
    // Extra part: remaining elements
    std::vector<double> extra;
    if (insert_after < n) {
      extra.assign(x.begin() + insert_after, x.end());
    }
    
    return {pu, extra};
  }

// ========================================================
  // Helper 2: Split sparse triplet into PU vs Extra
// ========================================================
  TripletSplit split_pu_extra_triplet(const Triplet& triplet,
                                      std::size_t insert_after) {
    const std::size_t n_rows = triplet.dims.first;
    const std::size_t n_cols = triplet.dims.second;
    
    // Identify PU vs Extra columns
    std::vector<int> pu_cols, extra_cols;
    for (std::size_t k = 0; k < n_cols; ++k) {
      if (k <= insert_after) pu_cols.push_back((int)k + 1); // 1-based
      else extra_cols.push_back((int)k + 1);
    }
    
    auto subset_triplet = [&](const std::vector<int>& cols) {
      Triplet out;
      out.dims = std::make_pair((int)n_rows, (int)cols.size());
      
      // Map global col → local col
      std::unordered_map<int, int> col_map;
      for (std::size_t idx = 0; idx < cols.size(); ++idx)
        col_map[cols[idx]] = (int)idx + 1;
      
      for (std::size_t k = 0; k < triplet.j.size(); ++k) {
        auto it = col_map.find(triplet.j[k]);
        if (it != col_map.end()) {
          out.i.push_back(triplet.i[k]);
          out.j.push_back(it->second);
          out.x.push_back(triplet.x[k]);
        }
      }
      return out;
    };
    
    TripletSplit result;
    result.pu_trip = subset_triplet(pu_cols);
    result.extra_trip = subset_triplet(extra_cols);
    return result;
  }

Triplet rbind_triplets(const std::vector<Triplet>& triplet_list) {
  Triplet out;
  int row_offset = 0;
  int max_ncol = 0;
  
  for (const auto& tr : triplet_list) {
    // Copy with row offset
    for (std::size_t k = 0; k < tr.i.size(); ++k) {
      out.i.push_back(tr.i[k] + row_offset);
      out.j.push_back(tr.j[k]);
      out.x.push_back(tr.x[k]);
    }
    row_offset += tr.dims.first;
    if (tr.dims.second > max_ncol) max_ncol = tr.dims.second;
  }
  
  out.dims = std::make_pair(row_offset, max_ncol);
  return out;
}

// cbind_triplets
Triplet cbind_triplets(const std::vector<Triplet>& triplet_list) {
  Triplet out;
  int nrows = triplet_list[0].dims.first;
  
  // Verify all have the same number of rows
  for (const auto& tr : triplet_list) {
    if (tr.dims.first != nrows)
      throw std::runtime_error("All triplets must have the same number of rows for cbind");
  }
  
  int col_offset = 0;
  for (const auto& tr : triplet_list) {
    out.i.insert(out.i.end(), tr.i.begin(), tr.i.end());
    for (int idx : tr.j) out.j.push_back(idx + col_offset);
    out.x.insert(out.x.end(), tr.x.begin(), tr.x.end());
    col_offset += tr.dims.second;
  }
  
  out.dims = {nrows, col_offset};
  
  // Debug print
  std::cout << "cbind_triplets: nrows = " << nrows 
  << ", ncols = " << col_offset 
  << ", nnz = " << out.i.size() << std::endl;
  
  return out;
}

// combine extras triplet
Triplet combine_extras_A_trip(const std::vector<TripletSplit>& split_list) {
  Triplet out;
  int row_offset = 0;
  int col_offset = 0;
  int total_rows = 0;
  
  std::vector<int> nrows_list(split_list.size());
  for (size_t k = 0; k < split_list.size(); ++k) {
    nrows_list[k] = split_list[k].pu_trip.dims.first;
    total_rows += nrows_list[k];
  }
  
  for (size_t k = 0; k < split_list.size(); ++k) {
    const Triplet& extra = split_list[k].extra_trip;
    
    if (extra.i.empty()) {
      row_offset += nrows_list[k];
      continue;
    }
    
    for (size_t n = 0; n < extra.i.size(); ++n) {
      out.i.push_back(extra.i[n] + row_offset);
      out.j.push_back(extra.j[n] + col_offset);
      out.x.push_back(extra.x[n]);
    }
    
    col_offset += extra.dims.second;
    row_offset += nrows_list[k];
  }
  
  out.dims = std::make_pair(total_rows, col_offset);
  return out;
}