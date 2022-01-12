#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
std::unordered_set<double> uniqueC(NumericVector x) {
  return std::unordered_set<double>(x.begin(), x.end());
}