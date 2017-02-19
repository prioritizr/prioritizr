#include "prioritizr.h"
#include "rcpp_boundary_data.h"

// [[Rcpp::export]]
Rcpp::List rcpp_boundary_data(Rcpp::DataFrame data, double tolerance=0.00001) {
  //// initialization
  typedef std::array<int,4> LINEID;
  typedef std::pair<std::size_t,std::size_t> PUPAIRID;
  
  /// declare variables and preallocate memory
  std::vector<std::size_t> PID = data["PID"];
  std::vector<double> X = data["X"];
  std::vector<double> Y = data["Y"];

  // calculation vars
  double tol = std::pow(10, round(log10(1.0 / tolerance)));
  std::vector<std::size_t> pos(PID.size());
  std::iota(pos.begin(), pos.end(), 1);
  boost::unordered_set<LINEID> line_id;
  line_id.reserve(PID.size()*10);
  std::unordered_multimap<LINEID, LINE, boost::hash<LINEID>> line_UMMAP;
  line_UMMAP.reserve(PID.size()*10);
  boost::unordered_set<PUPAIRID> pupair_id;
  pupair_id.reserve(PID.size()*10);
  std::unordered_multimap<PUPAIRID, PUPAIR, boost::hash<PUPAIRID>> pupair_UMMAP;
  pupair_UMMAP.reserve(PID.size()*10);
  
  // export vars
  std::vector<std::size_t> puid0;
  std::vector<std::size_t> puid1;
  std::vector<double> length;
  std::vector<std::string> warnings;
  warnings.reserve(PID.size()*10);
  
  //// preliminary processing
  // generate lines
  int currPIdFirstElement=0;
  LINE currLine;
  for (std::size_t i=1; i!=PID.size(); ++i) {
    if (PID[i]==PID[currPIdFirstElement]) {
      currLine=LINE(PID[i], pos[i], pos[i-1], X[i], Y[i], X[i-1], Y[i-1], tol);
      line_UMMAP.insert(std::pair<LINEID, LINE>(currLine._id, currLine));
      line_id.insert(currLine._id);
    } else {
      currPIdFirstElement=i;
    }
  }
  
  // free memory
  PID.clear();
  PID.shrink_to_fit();
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
    for (auto i=line_id.cbegin(); i!=line_id.cend(); ++i) {
      // init
      range=line_UMMAP.equal_range(*i);
      
      // store line data
      it=range.first;
      currPID=(it->second)._pid;
      currLEN=(it->second).length();
      ++it;
      if (it == range.second) {
        // store same pid if no duplicate lines
        currPUPAIR=PUPAIR(currPID,currPID,currLEN);
        pupair_id.insert(currPUPAIR._id);
        pupair_UMMAP.insert(std::pair<PUPAIRID, PUPAIR>(currPUPAIR._id,currPUPAIR));
      } else {
        // store second pid at least one duplicate lines
        currPUPAIR=PUPAIR(currPID,(it->second)._pid,currLEN);
        pupair_id.insert(currPUPAIR._id);
        pupair_UMMAP.insert(std::pair<PUPAIRID, PUPAIR>(currPUPAIR._id,currPUPAIR));
        
        // check to see if more than 2 spatially identical lines
        ++it;
        if (it != range.second) {
          it=range.first;
          for (; it!=range.second; ++it) {
            warnings.push_back((it->second).repr());
      }
        }
      }
    }
  }
  
  // free memory
//   line_id.swap(set);
  
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
  return(
    Rcpp::List::create(
      Rcpp::Named("data") = Rcpp::DataFrame::create(Named("id1")=puid0, Named("id2")=puid1, Named("boundary")=length),
      Rcpp::Named("warnings")=warnings
    )
  );
}
