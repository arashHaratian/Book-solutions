#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
RObject callWithOne(Function f) {
  return f(1);
}





// #include <Rcpp.h>
// using namespace Rcpp;
// 
// // [[Rcpp::export]]
// RObject callWithOne(Function f) {
//   return f("y", 1);
// }

// f(_["x"] = "y", _["value"] = 1);