#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]

double test1_cpp() {
  double a = 1.0 / 81;
  double b = 0;
  for (int i = 0; i < 729; ++i)
    b += a;
  return b;
}