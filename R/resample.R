#' @title Resample GPS fixes
#'
#' @param df data.frame to resample.
#' @param ts_col character name of the column with timestamp information.
#' @param freq frequency to resample to (e.g., 1/hour).
#' @param units units of the frequency. One of :
#'  - "hours"
#'  - "minutes"
#'  - "seconds"
#' @param low lower threshold to cut, in proportion.
#' @param high higher threshold to assign to a new track, in proportion.
#'
#' @details
#' Resampling is done in two ways:
#'   1) Fixes that are closer than freq * low are removed and time intervals
#'    are calculated again.
#'   2) Fixes that are further apart than freq * high are kept, but they are
#'    assigned different IDs, signifying they belong the different tracks.
#'    Tracks should not be considered to be continuous in time. Doing otherwise
#'    will likely result in inaccuracies and biases in virtually any movement
#'    analysis performed afterwards.
#'
#' @return data.frame, the original dataframe with two new columns:
#'   1) ID, the ID of the track. See details.
#'   2) dt, the time interval between fixes, with units specified with @param units.
#'
#' @examples
#' data(belcho)
#' belcho[["timestamp"]] <- strptime(
#'   belcho[["timestamp"]],
#'   format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
#' )
#' res <- resample(belcho, ts_col = "timestamp", freq = 3, units = "hours")
resample <- function(
    df,
    ts_col,
    freq,
    units,
    low = 0.9,
    high = 1.1
) {
  stopifnot(is(df[[ts_col]], "POSIXt"))
  # to Unix time
  ts <- df[[ts_col]]
  ts_unix <- as.numeric(ts)
  # frequencies to seconds
  to_seconds <- switch(units,
                       hours = 60 * 60,
                       minuts = 60,
                       seconds = 1)
  freq <- freq * to_seconds
  res <- .resample(ts_unix, freq, low, high)

  ans <- df[res[["include"]], ]
  ans[["ID"]] <- res[["ID"]][res[["include"]]]
  ans[["dt"]] <- res[["interval"]][res[["include"]]] / to_seconds

  tally <- table(ans[["ID"]])
  if ( any(tally == 1) ) {
    message("Some tracks have only one observation and will be removed.")
    ids <- setdiff(ans[["ID"]], names(tally)[tally == 1])
    ans <- ans[ans[["ID"]] %in% ids, ]
  }

  return (ans)

}
