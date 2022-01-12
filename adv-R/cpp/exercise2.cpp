#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]

LogicalVector find(CharacterVector x, CharacterVector s){
  
  LogicalVector out(x.size());
  std::unordered_set<String> lookup;
  lookup.insert(s.begin(), s.end());
  
  
  CharacterVector::iterator it;
  LogicalVector::iterator outit;
  for(it = x.begin(), outit = out.begin(); it != x.end(); ++it, ++outit){
    *outit = !lookup.insert(*it).second;
  }
  return (out);
}