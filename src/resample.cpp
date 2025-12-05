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
    const NumericVector& t,
    double freq,
    double th_low = 0.9,
    double th_high = 1.1
) {

  R_xlen_t n = t.size();;
  const double low = th_low * freq;
  const double high = th_high * freq;
  LogicalVector include(n, false);
  NumericVector dt(n, NA_REAL);
  NumericVector ID(n, NA_INTEGER);

  // explicitly include first fix
  int current_id = 1;
  ID[0] = current_id;
  include[0] = true;
  dt[0] = NA_REAL;
  R_xlen_t last_kept = 0; // index of last included fix

  for (R_xlen_t i = 1; i < n; ++i) {
    double interval = t[i] - t[last_kept];

    // handle negative or NA intervals conservatively: start new track
    if (NumericVector::is_na(interval)) {
      ID[i] = current_id;
      include[i] = false;
      dt[i] = NA_REAL;
      continue;
    }

    if (interval > high) {  // big gap -> new track; interval for track start set to NA
      current_id += 1;
      ID[i] = current_id;
      include[i] = true;
      dt[i] = NA_REAL;
      last_kept = i;
    } else if (interval < low) {  // too frequent -> exclude this fix; do not advance last_kept
      ID[i] = current_id;
      include[i] = false;
      dt[i] = interval; // chosen: interval to last kept (clear semantics)
    } else {  // acceptable interval -> keep
      ID[i] = current_id;
      include[i] = true;
      dt[i] = interval;
      last_kept = i;
    }
  }

  return List::create(
    Named("timestamp") = t,
    _["interval"] = dt,
    _["include"] = include,
    _["ID"] = ID
  );

}
