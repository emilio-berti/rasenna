#include <Rcpp.h>
using namespace Rcpp;

//' Resample telemetry fixes to a standard temporal resolution.
//' @param t Datetime of the location in Unix time (numeric).
//' @param freq Frequency of resampling.
//' @param th_low Lower threshold (proportion) of frequency to keep.
//' @param th_high Higher threshold (proportion) of frequency to assign to
//'   the same track.
// [[Rcpp::export(.resample)]]
List resample(
    NumericVector t,
    double freq,
    double th_low = 0.9,
    double th_high = 1.1
) {

  int n = t.length();
  LogicalVector include(n);
  NumericVector dt(n);
  NumericVector ID(n);
  th_low *= freq;
  th_high *= freq;

  dt[0] = R_NaN;
  ID[0] = 1;
  int j = 0; //counter for high frequencies fixes

  for (int i = 1; i < n; i++) {
    dt[i] = t[i] - t[i - 1];
    if (dt[i] > th_high) {
      ID[i] = max(ID) + 1;
    }
    if ( dt[i] < th_low ) {
      ID[i] = max(ID);
      include[i] = FALSE;
      if ( i >= j ){
        j += 1; //move counter
        dt[i + 1] = t[i + 1] - t[i - j];
      }
    } else {
      ID[i] = max(ID);
      include[i] = TRUE;
      j = 0; //reset counter
    }
  }

  // set ID and interval for starting track to NA
  for (int i = 1; i < ID.length(); i++) {
    if ( ID[i] != ID[i - 1] ) dt[i] = R_NaN;
    if ( ID[i] == 0 ) ID[i] = R_NaN;
  }

  List ans = List::create(
    Named("timestamp") = t,
    _["interval"] = dt,
    _["include"] = include,
    _["ID"] = ID
  );

  return ans;

}
