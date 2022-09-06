#include "package.h"
#include "rcpp_boundary_data.h"

// [[Rcpp::export]]
Rcpp::List rcpp_boundary_data(Rcpp::DataFrame data, arma::sp_mat strm,
                              bool str_tree, double tolerance = 0.00001) {
  //// initialization
  typedef std::array<std::string, 4> LINEID;
  typedef std::pair<std::size_t,std::size_t> PUPAIRID;

  /// declare variables and preallocate memory
  std::vector<std::size_t> PID = data["PID"];
  std::vector<std::size_t> SID = data["SID"];
  std::vector<double> X = data["X"];
  std::vector<double> Y = data["Y"];

  // calculation vars
  std::size_t tol = static_cast<std::size_t>(round(log10(1.0 / tolerance)));
  boost::unordered_set<LINEID> line_id;
  line_id.reserve(PID.size() * 10);
  std::unordered_multimap<LINEID, LINE, boost::hash<LINEID>> line_UMMAP;
  line_UMMAP.reserve(PID.size() * 10);
  boost::unordered_set<PUPAIRID> pupair_id;
  pupair_id.reserve(PID.size() * 10);
  std::unordered_multimap<PUPAIRID, PUPAIR, boost::hash<PUPAIRID>> pupair_UMMAP;
  pupair_UMMAP.reserve(PID.size() * 10);

  // export vars
  std::vector<std::size_t> puid0;
  std::vector<std::size_t> puid1;
  std::vector<double> length;
  std::vector<std::string> warnings;
  warnings.reserve(PID.size() * 10);

  //// preliminary processing
  // add in vertices for shared edges that do not have matching vertices
  LINE currLine;
  int currPidStart = 0;
  // perform pre-processing without a STR Query Tree
  {
    std::size_t i = 1;
    std::size_t j = 0;
    if (!str_tree) {
      while (i != PID.size()) {
        if ((PID[i] == PID[currPidStart]) && (SID[i] == SID[currPidStart])) {
          j = 0;
          while (j != PID.size()) {
            if (PID[currPidStart] != PID[j]) {
              if (is_between(X[i], Y[i], X[i - 1], Y[i - 1], X[j], Y[j])) {
                PID.insert(PID.begin() + i, PID[i]);
                SID.insert(SID.begin() + i, SID[i]);
                X.insert(X.begin() + i, X[j]);
                Y.insert(Y.begin() + i, Y[j]);
              }
            }
            ++j;
          }
        } else {
          currPidStart = i;
        }
        ++i;
      }
    }
  }
  {
    // perform pre-processing using a STR Query Tree
    Rcpp::IntegerVector curr_adjacent_units;
    std::size_t curr_i;
    std::size_t curr_j;
    std::size_t curr_i_start;
    std::size_t curr_j_start;
    std::size_t curr_i_vertex;
    std::size_t curr_j_vertex;
    if (str_tree) {
      // add a fake non-existant PID to the ensure that the while loops
      // terminate when we access non-existant elements in the PID vector
      PID.push_back(std::numeric_limits<std::size_t>::infinity());
      for (arma::sp_mat::const_iterator it = strm.begin();
           it != strm.end(); ++it) {
        // extract planning unit indices
        curr_i = it.row() + 1;
        curr_j = it.col() + 1;
        // find starting indices
        curr_i_start = (std::find(PID.cbegin(), PID.cend(), curr_i) -
                         PID.cbegin()) + 1;
        curr_j_start = (std::find(PID.cbegin(), PID.cend(), curr_j) -
                        PID.cbegin());
        // set starting indices
        curr_i_vertex = curr_i_start;
        curr_j_vertex = curr_j_start;
        // iterate over each vertex in i'th unit
        while (PID[curr_i_vertex] == curr_i) {
          // reset j'th unit
          curr_j_vertex = curr_j_start;
          // iterate over each vertex in j'th unit
          while (PID[curr_j_vertex] == curr_j) {
            // test if j'th vertex intersects between lines in i'th vertex
            if (is_between(X[curr_i_vertex],
                           Y[curr_i_vertex],
                           X[curr_i_vertex - 1],
                           Y[curr_i_vertex - 1],
                           X[curr_j_vertex],
                           Y[curr_j_vertex])) {
              // insert vertex
              PID.insert(PID.begin() + curr_i_vertex , PID[curr_i_vertex]);
              SID.insert(SID.begin() + curr_i_vertex , SID[curr_i_vertex]);
              X.insert(X.begin() + curr_i_vertex, X[curr_j_vertex]);
              Y.insert(Y.begin() + curr_i_vertex, Y[curr_j_vertex]);
              // increment i/j start vertices if needed
              if (curr_j_start > curr_i_vertex)
                ++curr_j_start;
              // reset i'th vertex
              curr_i_vertex = curr_i_start;
            }
            // increment j'th vertex
            ++curr_j_vertex;
          }
          // increment i'th vertex
          ++curr_i_vertex;
        }
      }
      // remove last PID element from PID
      PID.pop_back();
    }
  }

  // generate lines
  std::vector<std::size_t> pos(PID.size());
  std::iota(pos.begin(), pos.end(), 1);
  currPidStart = 0;
  for (std::size_t i = 1; i != PID.size(); ++i) {
    if ((PID[i] == PID[currPidStart]) && (SID[i] == SID[currPidStart])) {
      currLine = LINE(PID[i], pos[i], pos[i - 1], X[i], Y[i], X[i - 1],
                      Y[i - 1], tol);
      line_UMMAP.insert(std::pair<LINEID, LINE>(currLine._id,
                                                currLine));
      line_id.insert(currLine._id);
    } else {
      currPidStart = i;
    }
  }

  // free memory
  PID.clear();
  PID.shrink_to_fit();
  SID.clear();
  SID.shrink_to_fit();
  X.clear();
  X.shrink_to_fit();
  Y.clear();
  Y.shrink_to_fit();
  pos.clear();
  pos.shrink_to_fit();

  //// main processing
  /// construct lines
  {
    // declare local vars
    std::size_t currPID;
    double currLEN;
    PUPAIR currPUPAIR;
    std::unordered_multimap<LINEID,LINE,boost::hash<LINEID>>::iterator it;
    std::pair<std::unordered_multimap<LINEID,LINE,boost::hash<LINEID>>::iterator,std::unordered_multimap<LINEID,LINE,boost::hash<LINEID>>::iterator> range;

    // main loop
    for (auto i = line_id.cbegin(); i != line_id.cend(); ++i) {
      // init
      range = line_UMMAP.equal_range(*i);

      // store line data
      it=  range.first;
      currPID = (it->second)._pid;
      currLEN = (it->second).length();
      ++it;
      if (it == range.second) {
        // store same pid if no duplicate lines
        currPUPAIR = PUPAIR(currPID,currPID,currLEN);
        pupair_id.insert(currPUPAIR._id);
        pupair_UMMAP.insert(std::pair<PUPAIRID, PUPAIR>(currPUPAIR._id,currPUPAIR));
      } else {
        // store second pid at least one duplicate lines
        currPUPAIR = PUPAIR(currPID,(it->second)._pid,currLEN);
        pupair_id.insert(currPUPAIR._id);
        pupair_UMMAP.insert(std::pair<PUPAIRID, PUPAIR>(currPUPAIR._id,currPUPAIR));

        // check to see if more than 2 spatially identical lines
        ++it;
        if (it != range.second) {
          it = range.first;
          for (; it!=range.second; ++it) {
            warnings.push_back((it->second).repr());
          }
        }
      }
    }
  }

  /// construct pairs
  {

    // allocate memory
    puid0.resize(pupair_id.size());
    puid1.resize(pupair_id.size());
    length.resize(pupair_id.size());

    // declare local vars
    std::pair<std::unordered_multimap<PUPAIRID,PUPAIR,boost::hash<PUPAIRID>>::iterator,std::unordered_multimap<PUPAIRID,PUPAIR,boost::hash<PUPAIRID>>::iterator> range;

    // main loop
    std::size_t i=0;
    for (auto j=pupair_id.cbegin(); j!=pupair_id.cend(); ++j) {
      // init
      range=pupair_UMMAP.equal_range(*j);

      // store pu data
      puid0[i]=(range.first->second)._pid0;
      puid1[i]=(range.first->second)._pid1;
      for (auto it=range.first; it!=range.second; ++it) {
        length[i]+=(it->second)._length;
      }
      ++i;
    }
  }

  //// exports
  return Rcpp::List::create(
      Rcpp::Named("data") = Rcpp::DataFrame::create(
        Rcpp::Named("id1") = puid0,
        Rcpp::Named("id2") = puid1,
        Rcpp::Named("boundary") = length),
      Rcpp::Named("warnings") = warnings);
}
