# include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]

double meadian(NumericVector x){
  n = x.size();
  if (n % 2 == 0){
    std::partial_sort (x.begin(), x.begin() + n / 2 + 1, x.end());
    return (x[n / 2 - 1] + x[n / 2]) / 2;
  } else {
    std::partial_sort (x.begin(), x.begin() + (n + 1) / 2, x.end());
    return x[(n + 1) / 2 - 1];
  }
}