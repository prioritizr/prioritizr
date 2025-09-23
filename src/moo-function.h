#ifndef MOO_FUNCTION_H
#define MOO_FUNCTION_H

#include <vector>
#include <utility>  
#include <string> 

// Triplet struct 
struct Triplet {
  std::vector<int> i;          // row indices
  std::vector<int> j;          // col indices
  std::vector<double> x;       // values
  std::pair<int, int> dims;    // (nrow, ncol)
};

// Split result: PU part and Extra part
struct TripletSplit {
  Triplet pu_trip;
  Triplet extra_trip;
};

// Functions
// split vectors into pu and extra part
std::pair<std::vector<double>, std::vector<double>>
  split_pu_extra_vec(const std::vector<double>& x, std::size_t insert_after);

// Split triplets
TripletSplit split_pu_extra_triplet(const Triplet& triplet,
                                    std::size_t insert_after);
// Combine triplets by rbind
Triplet rbind_triplets(const std::vector<Triplet>& triplet_list);

// Combine triplets by cbind
Triplet cbind_triplets(const std::vector<Triplet>& triplet_list);

// combine extras triplet function
Triplet combine_extras_A_trip(const std::vector<TripletSplit>& split_list);

#endif