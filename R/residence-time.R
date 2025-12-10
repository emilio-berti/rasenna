#' @title Residence Time
#' @description The residence time quantifies how much time is
#'  spent in a given area.
#' @details Residence time is calculated as the total time spent
#'  consecutively within one defined area. If the track is divided
#'  into clusters (see `clusterize()`), then residence time is 
#'  calculated as the sum of the residence times for each cluster.
#' @param df data.frame of the track.
#' @param ts_col character name of the column with timestamp information.
#' @param unit 'seconds', 'minutes', or 'hours'.
#' @return Numeric vector with the residence time.
#' @examples
#'  data(capra)
#'  capra$timestamp <- as.POSIXct(capra[["timestamp"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#'  capra <- capra[-c(1, 2), ] # drop release
#'  capra <- clusterize(capra)
#'  residence_time(capra, unit = "days")
residence_time <- function(
  df,
  ts_col = "timestamp",
  unit = c("seconds", "minutes", "hours", "days")
) {

  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))
  stopifnot(is(df[[ts_col]], "POSIXt"))

  unit <- match.arg(unit)

  if ("cluster" %in% colnames(df)) {
    # multiple clusters

    # split clusters
    track_id <- cumsum(c(1, diff(df$cluster) != 0))
    cl <- numeric(max(track_id))
    res_time <- numeric(max(track_id))

    # for each cluster, calculate residence time
    for (i in seq_along(cl)) {
      track <- df[track_id == i, ]
      cl[i] <- unique(track$cluster)
      res_time[i] <- difftime(
        track[[ts_col]][nrow(track)],
        track[[ts_col]][1],
        units = "secs"
      )
    }
    res_time <- tapply(res_time, INDEX = cl, FUN = sum)

  } else {
    # only one cluster

    res_time <- difftime(
      df[[ts_col]][nrow(df)],
      df[[ts_col]][1],
      units = "secs"
    )

  }

  # convert to the specified units
  out <- switch(
    unit,
    "seconds" = as.numeric(res_time),
    "minutes" = as.numeric(res_time) / 60,
    "hours" = as.numeric(res_time) / 60 / 60,
    "days" = as.numeric(res_time) / 60 / 60 / 24
  )
  if (!is.null(names(res_time))) names(out) <- names(res_time)

  return(out)
  
}
