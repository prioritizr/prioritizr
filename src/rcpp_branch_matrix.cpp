#include "package.h"

// [[Rcpp::export]]
arma::sp_mat rcpp_branch_matrix(Rcpp::List x) {
  /// initialization
  // load inputs
  Rcpp::IntegerMatrix edge = x["edge"];
  Rcpp::CharacterVector tip_labels = x["tip.label"];
  Rcpp::IntegerMatrix::Column edge_column_1 = edge(_,0);
  Rcpp::IntegerMatrix::Column edge_column_2 = edge(_,1);
  // create vectors to store results
  std::vector<std::size_t> m_i;
  std::vector<std::size_t> m_j;
  m_i.reserve(edge_column_1.size() * tip_labels.size());
  m_j.reserve(edge_column_1.size() * tip_labels.size());
  // create temporary variables
  Rcpp::IntegerVector::iterator curr_pos;
  std::size_t curr_node;
  std::size_t curr_branch;
  // main processing
  for (std::size_t curr_tip = 0;
       curr_tip < static_cast<std::size_t>(tip_labels.size()); ++curr_tip) {
    // set variables to be the tip corresponding to the i'th species
    curr_node = (curr_tip+1);
    // loop through branches until the curr_pos is pointing to the base
    while (true) {
      // find the element where curr_node is at the top of the branch
      curr_pos = std::find(edge_column_2.begin(), edge_column_2.end(),
                           curr_node);
      // break if the base has been found
      if (curr_pos == edge_column_2.end()) {
        break;
      }
      // calculate the branch number for the node (ie. the index of the
      // of the node
      curr_branch = std::distance(edge_column_2.begin(), curr_pos);
      // store the branch number and the tip number
      m_i.push_back(curr_tip);
      m_j.push_back(curr_branch);
      // set curr_node to be the node at the bottom of the current branch
      curr_node = edge_column_1[curr_branch];
    }
  }
  // compile sparse matrix
  m_i.shrink_to_fit();
  m_j.shrink_to_fit();
  arma::umat m_ij(2,m_i.size());
  m_ij.row(0) = arma::conv_to<arma::urowvec>::from(m_i);
  m_ij.row(1) = arma::conv_to<arma::urowvec>::from(m_j);
  arma::colvec m_x(m_i.size());
  m_x.ones();
  arma::sp_mat m(m_ij, m_x, tip_labels.size(), edge_column_1.size());
  // return sparse matrix
  return m;
}
