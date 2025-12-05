#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

//' Distance between telemetry fixes.
//' @param x horizontal (longitude) coordinate vector.
//' @param y vertical (latitude) coordinate vector.
// [[Rcpp::export(.distance)]]
NumericVector distance(NumericVector x, NumericVector y) {
  int n = x.length();
  double dx2;
  double dy2;
  NumericVector out(n - 1);

  for (int i = 0; i < (n - 1); i++) {
    dx2 = pow( (x[i + 1] - x[i]) , 2 );
    dy2 = pow( (y[i + 1] - y[i]) , 2 );
    out[i] = std::sqrt(dx2 + dy2);
  }

  return out;
}
