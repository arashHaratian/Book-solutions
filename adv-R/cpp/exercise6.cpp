#include <Rcpp.h>
#include <unordered_set>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector setdiffC(IntegerVector x, IntegerVector y) {
  IntegerVector tmp(x.size());
  
  IntegerVector::iterator it,out_it;
  IntegerVector out;
  std::sort(x.begin(), x.end());
  std::sort(y.begin(), y.end());
  out_it = std::set_difference(x.begin(), x.end(), y.begin(), y.end(), tmp.begin());
  
  for(it = tmp.begin(); it != out_it; ++it){
    out.push_back(*it);
  }
   return out; 
}



// [[Rcpp::export]]
IntegerVector intersectC(IntegerVector x, IntegerVector y) {
  IntegerVector tmp(std::min(x.size(), y.size()));
  
  IntegerVector::iterator it,out_it;
  IntegerVector out;
  std::sort(x.begin(), x.end());
  std::sort(y.begin(), y.end());
  out_it = std::set_intersection(x.begin(), x.end(), y.begin(), y.end(), tmp.begin());
  
  for(it = tmp.begin(); it != out_it; ++it){
    out.push_back(*it);
  }
  return out; 
}



// [[Rcpp::export]]
std::unordered_set<int> unionC(IntegerVector x, IntegerVector y) {
  IntegerVector tmp(std::max(x.size(), y.size()));
  
  IntegerVector::iterator it,out_it;
  std::unordered_set<int> out;
  std::sort(x.begin(), x.end());
  std::sort(y.begin(), y.end());
  out_it = std::set_union(x.begin(), x.end(), y.begin(), y.end(), tmp.begin());
  
  for(it = tmp.begin(); it != out_it; ++it){
    out.insert(*it);
  }
  return out; 
}


// // [[Rcpp::export]]
// IntegerVector unionCC(IntegerVector x, IntegerVector y) {
//   IntegerVector tmp(std::max(x.size(), y.size()));
//   
//   IntegerVector::iterator it,out_it;
//   IntegerVector out;
//   std::sort(x.begin(), x.end());
//   std::sort(y.begin(), y.end());
//   out_it = std::set_union(x.begin(), x.end(), y.begin(), y.end(), tmp.begin());
//   
//   for(it = tmp.begin(); it != out_it; ++it){
//     out.push_back(*it);
//   }
//   return out; 
// }
