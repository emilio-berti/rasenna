#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

//' Bearing of telemetry fixes.
//' @param x horizontal (longitude) coordinate vector.
//' @param y vertical (latitude) coordinate vector.
// [[Rcpp::export(.bearing)]]
NumericVector bearing(NumericVector x, NumericVector y) {
  int n = x.length();
  double dx;
  double dy;
  double angle;
  NumericVector out(n - 1);

  for(int i = 0; i < (n - 1); i++) {
    dx = x[i + 1] - x[i];
    dy = y[i + 1] - y[i];
    angle = std::atan2(dy, dx);      // signed angle relative to x-axis
    if(angle < 0) angle += 2 * M_PI; // wrap to [0, 2pi)
    out[i] = angle;
  }

  return out;
}

//' Truning angle between telemetry fixes.
//' @param x horizontal (longitude) coordinate vector.
//' @param y vertical (latitude) coordinate vector.
// [[Rcpp::export(.turning_angle)]]
NumericVector turning_angle(NumericVector x, NumericVector y) {
  NumericVector b = bearing(x, y);
  int n = b.size();
  double diff;
  NumericVector out(n - 1);

  const double two_pi = 2.0 * M_PI;

  for(int i = 0; i < (n - 1); i++) {
    diff = b[i + 1] - b[i];
    diff = std::fmod(diff + M_PI, two_pi);
    if(diff < 0) diff += two_pi;
    out[i] = diff - M_PI;
  }

  return out;
}
